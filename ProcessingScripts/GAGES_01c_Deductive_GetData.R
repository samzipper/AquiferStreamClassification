## GAGES_Deductive_GetData.R
#' This is intended to extract the data necessary for deductive classification of
#' the GAGES catchments following various different procedures.
#' 
#' The data will then be saved as a CSV file.
#' 
#' This script outputs the following variables:
#'   -relief.m = (max elevation) - (min elevation) [m]
#'   -flatland.overall = percent area with <= 1% slope [%]
#'   -flatland.upland = percent of flatland.overall above mean watershed elevation [%]
#'   -WTD.m (min, max, mean, sd) = water table depth [m]
#'   -sand.top15cm.prc (min, max, mean, sd) = mean percent sand in soil, top 15 cm [%]
#'   -DTB.cm (min, max, mean, sd) = depth to bedrock, cm [%]
#'   -porosity (min, max, mean, sd) = porosity [-]
#'   -logk = mean log permeability [m2]
#'   -precip.mm = precipitation [mm/yr]
#'   -pet.mm = potential ET [mm/yr]
#'   -precip.cold = proportion of annual precip occurring when air temp < 0 C
#'   -precip.winter = winter precipitation seasonality indicator
#'   -precip.summer = summer precipitation seasonality indicator
#'   -defc.mm = PET - precip [mm/yr]
#'   -dryness = Feddema dryness index [mm/yr]
#'   -event.size.mm = mean precipitation event size [mm]
#'   -prc.lakes = percent of basin covered in lakes [%]
#'
#' Some of correspond to the following columns in the New_catchments_v9.xlsx spreadsheed from Carolina Massmann.
#'   -precip.mm = pp_a (DB_climate, col 2)
#'   -pet.mm = pet_a (DB_climate, col 3)
#'   -precip.winter = pp_11-02 (DB_climate, col 19) = (Nov to Feb precip)/((annual precip/12)*4)
#'   -precip.summer = pp_06-08 (DB_climate, col 18) = (Jun to Aug precip)/((annual precip/12)*3)
#'   -precip.cold = pp_cold (DB_climate, col 110)
#'   
#' Other variables come from a variety of sources:
#'   -relief.m = HydroSHEDS DEM
#'   -flatland.overall = HydroSHEDS DEM
#'   -flatland.upland = HydroSHEDS DEM
#'   -WTD.m (min, max, mean, sd) = Fan et al. (2013)
#'   -sand.top15cm.prc (min, max, mean, sd) = SoilGrids1km
#'   -DTB.cm (min, max, mean, sd) = SoilGrids1km
#'   -logk (min, max, mean, sd) = GLHYMPS 1.0
#'   -porosity (min, max, mean, sd) = GLHYMPS 1.0
#'   -prc.lakes = HydroLAKES

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

# path on GSAS to Fan et al. (2013) WTD data
WTD.dir <- "Z:/2.active_projects/Zipper/1.Spatial_data/global/wt_water_table_wells/1original/wt1_fan2103_water_table_depth/geotiffs/v2/"

# path to soil data directory
soil.dir <- "Z:/2.active_projects/Zipper/1.Spatial_data/global/s_soils/"

# path to GLHYMPS (2014) directory
GLHYMPS.dir <- "Z:/2.active_projects/Zipper/1.Spatial_data/global/k_permeability_porosity/2derived/GLHYMPS/v1_2014/"

# path to lakes data
lakes.dir <- "Z:/2.active_projects/Zipper/1.Spatial_data/global/riv_river_network_lakes/2derived/HydroLAKES_v10/"

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- paste0(data.in.dir, "plots/")

## 1) get the data aggregated by Carolina
# read in necessary DB files
db.climate <- read.csv(paste0(data.in.dir, "FromCarolina+Thorsten/DB_climate.csv"), header=F, skip=6)
df.event.size <- read.csv(paste0(data.dir, "GAGES_Extract_ClimateData.csv"))  # this is from the script GAGES_Extract_ClimateData.R

# subset to relevant columns
df.climate <- data.frame(basin = db.climate[,1],
                         precip.mm = db.climate[,2],
                         pet.mm = db.climate[,3],
                         precip.winter = db.climate[,19],
                         precip.summer = db.climate[,18],
                         precip.cold = db.climate[,110],
                         stringsAsFactors=F)

## 2) get data not available from Carolina
# read in shapefiles
shp <- readOGR(dsn=paste0(data.in.dir, "FromCarolina+Thorsten/shapefiles"), layer="basins_CONUS")

# read in DEM and slope rasters from HydroBASINS
r.dem <- raster(paste0(GIS.dir, "DEM_15s/global_dem_15s.vrt"))
r.slope <- raster(paste0(GIS.dir, "slope_15s/global_slope_15s.vrt"))

# reproject shapefile to match DEM
shp.r <- spTransform(shp, crs(r.dem))

# trim rasters to shapefile extent
r.dem.s <- crop(r.dem, shp.r)
r.slope.s <- crop(r.slope, shp.r)

# figure out raster cell numbers contained within each polygon
df.cells <- extract(r.dem.s, shp.r, cellnumbers=T, df=T)

# rename DEM column
colnames(df.cells)[colnames(df.cells)=="global_dem_15s"] <- "elev.m"

# extract elevations and slopes
df.cells$slope.prc <- extract(r.slope.s, df.cells$cell)

# summarize by catchment
df.elev <- summarize(group_by(df.cells, ID),
                     n.land.pts  = sum(is.finite(elev.m) | is.finite(slope.prc)),
                     elev.m.min  = round(min(elev.m, na.rm=T), 1),
                     elev.m.mean = round(mean(elev.m, na.rm=T), 1),
                     elev.m.max  = round(max(elev.m, na.rm=T), 1),
                     slope.prc.min   = round(min(slope.prc, na.rm=T), 1),
                     slope.prc.mean  = round(mean(slope.prc, na.rm=T), 1),
                     slope.prc.max   = round(max(slope.prc, na.rm=T), 1),
                     n.flat.pts      = sum(slope.prc < 1, na.rm=T),
                     n.flat.up.pts   = sum(slope.prc < 1 & elev.m > elev.m.mean, na.rm=T))
df.elev$relief.m <- df.elev$elev.m.max - df.elev$elev.m.min

# get list of IDs for merging later
df.shp <- data.frame(basin = shp.r@data$GAGE_ID,
                     ID = seq(1, dim(shp.r@data)[1]),
                     stringsAsFactors=F)

# merge with basin column
df.shp <- left_join(df.shp, df.elev, by=c("ID"))

# convert basin column to numeric
df.shp$basin <- as.numeric(levels(df.shp$basin)[as.numeric(df.shp$basin)])

## read water table depth data
r.WTD <- raster(paste0(WTD.dir, "N_America_model_wtd_v2.tif"))
r.WTD.s <- crop(r.WTD, shp.r)
df.WTD <- extract(r.WTD.s, shp.r, df=T)
colnames(df.WTD)[colnames(df.WTD)=="N_America_model_wtd_v2"] <- "WTD.m"

# summarize by basin
df.WTD.basin <- summarize(group_by(df.WTD, ID),
                          n.WTD.pts  = sum(is.finite(WTD.m)),
                          WTD.m.min  = round(min(WTD.m, na.rm=T), 1),
                          WTD.m.mean = round(mean(WTD.m, na.rm=T), 1),
                          WTD.m.max  = round(max(WTD.m, na.rm=T), 1),
                          WTD.m.sd   = round(sd(WTD.m, na.rm=T), 1))

# merge with basin
df.shp <- left_join(df.shp, df.WTD.basin, by="ID")

## soil data
# load rasters
r.sand.top15cm <- raster(paste0(soil.dir, "2derived/SoilGrids1km/SNDPPT_M_meanTop015cm_1km_ll.tif"))
r.DTB.cm <- raster(paste0(soil.dir, "1original/SoilGrids1km/BDTICM_M_1km_ll.tif"))

# crop to extent
r.sand.s <- crop(r.sand.top15cm, shp.r)
r.DTB.s <- crop(r.DTB.cm, shp.r)

# figure out raster cell numbers contained within each polygon
df.soil <- extract(r.sand.s, shp.r, cellnumbers=T, df=T)

# rename sand column
colnames(df.soil)[colnames(df.soil)=="SNDPPT_M_meanTop015cm_1km_ll"] <- "sand.top15cm.prc"

# extract elevations and slopes
df.soil$DTB.cm <- extract(r.DTB.s, df.soil$cell)

# summarize by basin
df.soil.basin <- summarize(group_by(df.soil, ID),
                           n.soil.pts  = sum(is.finite(sand.top15cm.prc)),
                           sand.top15cm.prc.min  = round(min(sand.top15cm.prc, na.rm=T), 1),
                           sand.top15cm.prc.mean = round(mean(sand.top15cm.prc, na.rm=T), 1),
                           sand.top15cm.prc.max  = round(max(sand.top15cm.prc, na.rm=T), 1),
                           sand.top15cm.prc.sd   = round(sd(sand.top15cm.prc, na.rm=T), 1),
                           DTB.cm.min  = round(min(DTB.cm, na.rm=T), 1),
                           DTB.cm.mean = round(mean(DTB.cm, na.rm=T), 1),
                           DTB.cm.max  = round(max(DTB.cm, na.rm=T), 1),
                           DTB.cm.sd   = round(sd(DTB.cm, na.rm=T), 1))

# merge with basin
df.shp <- left_join(df.shp, df.soil.basin, by="ID")

## porosity and permeability data
# load data
r.porosity <- raster(paste0(GLHYMPS.dir, "GLHYMPS_subsetCONUS_porosity.tif"))
r.logk <- raster(paste0(GLHYMPS.dir, "GLHYMPS_subsetCONUS_logk.tif"))

# set nodata value
r.porosity[r.porosity[]==0] <- NaN
r.logk[r.logk[]==0] <- NaN

# reproject
r.porosity.proj <- projectRaster(from=r.porosity, res=res(r.porosity), crs=crs(shp))
r.logk.proj <- projectRaster(from=r.logk, to=r.porosity.proj)

# crop
r.porosity.s <- crop(r.porosity.proj, shp)
r.logk.s <- crop(r.logk.proj, shp)

# extract data
df.GLHYMPS <- extract(r.porosity.s, shp, cellnumbers=T, df=T)

# rename DEM column
colnames(df.GLHYMPS)[colnames(df.GLHYMPS)=="GLHYMPS_subsetCONUS_porosity"] <- "porosity"

# extract elevations and slopes
df.GLHYMPS$logk <- extract(r.logk.s, df.GLHYMPS$cell)

# summarize
df.GLHYMPS.basin <- summarize(group_by(df.GLHYMPS, ID),
                              n.GLHYMPS.pts = sum(is.finite(porosity)),
                              porosity.min  = round(min(porosity, na.rm=T), 3),
                              porosity.mean = round(mean(porosity, na.rm=T), 3),
                              porosity.max  = round(max(porosity, na.rm=T), 3),
                              porosity.sd   = round(sd(porosity, na.rm=T), 3),
                              logk.min  = round(min(logk, na.rm=T), 3),
                              logk.mean = round(mean(logk, na.rm=T), 3),
                              logk.max  = round(max(logk, na.rm=T), 3),
                              logk.sd   = round(sd(logk, na.rm=T), 3))

# merge with basin
df.shp <- left_join(df.shp, df.GLHYMPS.basin, by="ID")

## get lake area
# load file
r.lakes <- raster(paste0(lakes.dir, "conus_HydroLAKES_v10_LakeType.tif"))

# trim to shapefile extent
r.lakes.s <- crop(r.lakes, shp.r)
df.lakes <- extract(r.lakes.s, shp.r, df=T)
colnames(df.lakes)[colnames(df.lakes)=="conus_HydroLAKES_v10_LakeType"] <- "LakeType"

# summarize by basin
df.lakes.basin <- summarize(group_by(df.lakes, ID),
                          n.points.pts = sum(is.finite(LakeType)),
                          n.lakes.pts = sum(LakeType>0))

# calculate percent lakes
df.lakes.basin$prc.lakes <- round(100*df.lakes.basin$n.lakes.pts/df.lakes.basin$n.points.pts, 1)

# merge with basin
df.shp <- left_join(df.shp, df.lakes.basin, by="ID")

## 3) Merge all data frames
# merge
df.out <- left_join(df.climate, df.event.size[,c("basin", "event.size.mm")], by="basin")
df.out <- left_join(df.out, df.shp, by="basin")

# calculate useful metrics
df.out$elev.m.range <- df.out$elev.m.max - df.out$elev.m.min
df.out$flat.overall <- round((100*df.out$n.flat.pts/df.out$n.land.pts), 2)
df.out$flat.upland <- round(100*(df.out$n.flat.up.pts/df.out$n.flat.pts), 2)

## calculate derived variables
# precipitation deficit [mm] - used in Wolock et al
df.out$defc.mm <- df.out$pet.mm - df.out$precip.mm

# flatland in lowlands [%] - used in Wolock et al
df.out$flat.lowland <- 100 - df.out$flat.upland

# Feddema dryness index [-] - used by Leibowitz et al
df.out$dryness <- NaN
df.out$dryness[df.out$pet.mm > df.out$precip.mm] <- 
  df.out$precip.mm[df.out$pet.mm > df.out$precip.mm]/df.out$pet.mm[df.out$pet.mm > df.out$precip.mm] - 1
df.out$dryness[df.out$precip.mm > df.out$pet.mm] <- 
  1 - df.out$pet.mm[df.out$precip.mm > df.out$pet.mm]/df.out$precip.mm[df.out$precip.mm > df.out$pet.mm]

# any columns with no flatland also set flatland.upland to 0
df.out$flat.upland[df.out$n.flat.pts==0] <- 0
df.out$flat.lowland[df.out$n.flat.pts==0] <- 0

## 4) Save
write.csv(df.out, paste0(data.dir, "GAGES_Deductive_GetData.csv"), row.names=F)

## 5) Make some plots
# read in data
df.out <- read.csv(paste0(data.dir, "GAGES_Deductive_GetData.csv"))

# list of variables to plot
vars.plot <- c("event.size.mm", "sand.top15cm.prc.mean", "elev.m.range", "flat.overall", "precip.cold", "porosity.mean", "logk.mean", "WTD.m.mean", "DTB.cm.mean", "dryness", "prc.lakes")

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
