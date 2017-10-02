## GAGES_Deductive_GetData.R
#' This is intended to extract the data necessary for deductive classification of
#' the GAGES catchments following various different procedures.
#' 
#' The data will then be saved as a CSV file.
#' 
#' This script outputs the following variables:
#'   -relief = (max elevation) - (min elevation) [m]
#'   -flatland.overall = percent area with <= 1% slope [%]
#'   -flatland.upland = percent of flatland.overall above mean watershed elevation [%]
#'   -sand.prc = mean percent sand in soil [%]
#'   -k.log = mean log permeability [m2]
#'   -precip.mm = precipitation [mm/yr]
#'   -pet.mm = potential ET [mm/yr]
#'   -precip.winter = winter precipitation seasonality indicator
#'   -precip.summer = summer precipitation seasonality indicator
#'
#' These correspond to the following columns in the New_catchments_v9.xlsx spreadsheed from Carolina Massmann.
#'   -relief = elev_rge (DB_basins, from NED, 10 m resolution)
#'   -flat.overall = NA
#'   -flat.upland = NA
#'   -sand.prc = sandtotal (DB_basins col 79, from SSURGO, weight percent of particles 0.5-2.0 mm in diameter)
#'   -k.log = logk (DB_basins col 23, from Gleeson et al. 2011)
#'   -precip.mm = pp_a (DB_climate, col 2)
#'   -pet.mm = pet_a (DB_climate, col 3)
#'   -precip.winter = pp_11-02 (DB_climate, col 19) = (Nov to Feb precip)/((annual precip/12)*4)
#'   -precip.summer = pp_06-08 (DB_climate, col 18) = (Jun to Aug precip)/((annual precip/12)*3)

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(raster)
require(dplyr)
require(ggplot2)
require(reshape2)
require(rgdal)
require(rgeos)
require(broom)

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/"

# directory containing GIS data files
GIS.dir <- "C:/Users/Sam/Dropbox/GIS_GeneralFiles/HydroSHEDS/"

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- paste0(data.in.dir, "plots/")

## 1) get the data aggregated by Carolina
# read in necessary DB files
db.climate <- read.csv(paste0(data.in.dir, "FromCarolina+Thorsten/DB_climate.csv"), header=F, skip=6)
db.basins <- read.csv(paste0(data.in.dir, "FromCarolina+Thorsten/DB_basins.csv"), header=F, skip=6)

# subset to relevant columns
df.climate <- data.frame(basin = db.climate[,1],
                         precip.mm = db.climate[,2],
                         pet.mm = db.climate[,3],
                         precip.winter = db.climate[,19],
                         precip.summer = db.climate[,18],
                         stringsAsFactors=F)

df.basins <- data.frame(basin = db.basins[,2],
                        sand.prc = db.basins[,79],
                        k.log = db.basins[,23],
                        stringsAsFactors=F)

# convert sand to numeric
df.basins$sand.prc <- as.numeric(levels(df.basins$sand.prc)[as.numeric(df.basins$sand.prc)])

## 2) get elevation-derived data not available from Carolina
# read in shapefiles
shp <- readOGR(dsn=paste0(data.in.dir, "FromCarolina+Thorsten/shapefiles"), layer="basins_CONUS")

# read in DEM and slope rasters from HydroBASINS
r.dem <- raster(paste0(GIS.dir, "DEM_15s/na_dem_15s.tif"))
r.slope <- raster(paste0(GIS.dir, "slope_15s/na_slope_15s.tif"))

# reproject shapefile to match DEM
shp.r <- spTransform(shp, crs(r.dem))

# trim rasters to shapefile extent
r.dem.s <- crop(r.dem, shp.r)
r.slope.s <- crop(r.slope, shp.r)

# figure out raster cell numbers contained within each polygon
df.cells <- extract(r.dem.s, shp.r, cellnumbers=T, df=T)

# rename DEM column
colnames(df.cells)[colnames(df.cells)=="na_dem_15s"] <- "elev.m"

# extract elevations and slopes
df.cells$slope.prc <- extract(r.slope.s, df.cells$cell)

# summarize by catchment
df.elev <- summarize(group_by(df.cells, ID),
                     n.land      = sum(is.finite(elev.m) | is.finite(slope.prc)),
                     elev.m.min  = round(min(elev.m, na.rm=T), 1),
                     elev.m.mean = round(mean(elev.m, na.rm=T), 1),
                     elev.m.max  = round(max(elev.m, na.rm=T), 1),
                     flat.n      = sum(slope.prc < 1, na.rm=T),
                     flat.up.n   = sum(slope.prc < 1 & elev.m > elev.m.mean, na.rm=T))

# get list of IDs for merging later
df.shp <- data.frame(basin = shp.r@data$GAGE_ID,
                     ID = seq(1, dim(shp.r@data)[1]),
                     stringsAsFactors=F)

# merge with basin column
df.shp <- left_join(df.shp, df.elev, by=c("ID"))

# convert basin column to numeric
df.shp$basin <- as.numeric(levels(df.shp$basin)[as.numeric(df.shp$basin)])

## 3) Merge all data frames
# merge
df.out <- left_join(df.basins, df.climate, by="basin")
df.out <- left_join(df.out, df.shp, by="basin")

# calculate useful metrics
df.out$elev.m.range <- df.out$elev.m.max - df.out$elev.m.min
df.out$flat.overall <- round((100*df.out$flat.n/df.out$n.land), 2)
df.out$flat.upland <- round(100*(df.out$flat.up.n/df.out$flat.n), 2)

# any columns with no flatland also set flatland.upland to 0
df.out$flat.upland[df.out$flat.n==0] <- 0

# get rid of junk columns
df.out$ID <- NULL
df.out$flat.n <- NULL
df.out$flat.up.n <- NULL
df.out$n.land <- NULL

## 4) Save
write.csv(df.out, paste0(data.dir, "GAGES_Deductive_GetData.csv"), row.names=F)

## 5) Make some plots
# read in data
df.out <- read.csv(paste0(data.dir, "GAGES_Deductive_GetData.csv"))

# list of variables to plot
vars.plot <- c("sand.prc", "k.log", "precip.mm", "pet.mm", "defc.mm", "elev.m.mean", "elev.m.range", "flat.overall", "flat.upland")

# subset to plot variables only, plus basin number
df.plot <- subset(df.out, select=c("basin", vars.plot))

# melt to long-form
df.melt <- melt(df.plot, id=c("basin"))

# density plot
p.dens <- 
  ggplot(df.melt, aes(x=value)) +
  geom_density() +
  facet_wrap(~variable, scales="free") +
  theme_bw() +
  theme(panel.grid=element_blank())
ggsave(paste0(plot.dir, "GAGES_Deductive_GetData_p.dens.png"), p.dens, width=9, height=9, units="in")