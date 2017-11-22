## NorthAmerica_01_SelectCatchments.R
#' This script is intended to read produce an output file which is a
#' list of the catchments used for final analysis.

rm(list=ls())

source("C:/Users/Sam/WorkGits/CONUS_CatchmentClassification/ProcessingScripts/paths+packages.R")

## define selection criteria
area.km2.max <- 5000   # max allowed basin area [km^2]

## USA catchments from HCDN-2009
# read shapefile
shp.HCDN <- readOGR(dsn=paste0(substr(dir.GAGES, 1, nchar(dir.GAGES)-1)), layer="HCDN-2009_Boundaries")  # need to get rid of final / in path for readOGR

# look at data
head(shp.HCDN@data)

# the column "DRAIN" is drainage area in km2
shp.HCDN.out <- subset(shp.HCDN, DRAIN<= area.km2.max)

## USA catchments from NNRW
# read shapefile
shp.NNRW <- readOGR(dsn=paste0(substr(dir.NNRW, 1, nchar(dir.NNRW)-1)), layer="NNRW_Boundaries")  # need to get rid of final / in path for readOGR

# look at data
head(shp.NNRW@data)

# the column "are_km2" is drainage area in km2
shp.NNRW.out <- subset(shp.NNRW, are_km2 <= area.km2.max)

## Canadian catchments from CANOPEX
# read catchment info
df.CANOPEX <- read.csv(paste0(dir.CANOPEX, "STATION_METADATA.csv"), stringsAsFactors=F)

# look at data
head(df.CANOPEX)

# the columns HYDROSHEDS_AREA is the drainage area in km2
df.CANOPEX.out <- subset(df.CANOPEX, HYDROSHEDS_AREA <= area.km2.max)

## set up data frames to merge into output
df.CANOPEX.merge <- data.frame(network = "CANOPEX",
                               station = df.CANOPEX.out$STATION_ID,
                               name = df.CANOPEX.out$STATION_NAME,
                               state = gsub("'", "", df.CANOPEX.out$PROVINCE),
                               county = "Canada",
                               lat = df.CANOPEX.out$STATION_LATITUDE,
                               lon = df.CANOPEX.out$STATION.LONGITUDE,
                               area.km2 = df.CANOPEX.out$HYDROSHEDS_AREA)

df.HCDN.merge <- data.frame(network = "HCDN-2009",
                            station = shp.HCDN.out@data$statn,
                            name = shp.HCDN.out@data$STATION_N,
                            state = shp.HCDN.out@data$STATE,
                            county = "USA",
                            lat = shp.HCDN.out@data$LAT_G,
                            lon = shp.HCDN.out@data$LONG_,
                            area.km2 = shp.HCDN.out@data$DRAIN)

df.NNRW.merge <- data.frame(network = "NNRW",
                            station = shp.NNRW.out@data$station,
                            name = shp.NNRW.out@data$name,
                            state = abb2state(shp.NNRW.out@data$State, convert=T),
                            county = "USA",
                            lat = shp.NNRW.out@data$Latitud,
                            lon = shp.NNRW.out@data$Longitd,
                            area.km2 = shp.NNRW.out@data$are_km2)

## get rid of duplicates
df.NNRW.merge <- subset(df.NNRW.merge, !(station %in% df.HCDN.merge$station))

## make output dataset
df.out <- rbind(df.CANOPEX.merge, df.HCDN.merge, df.NNRW.merge)

## save output dataset
write.csv(df.out, paste0(dir.data, "NorthAmerica_01_SelectCatchments.csv"))

## make some plots
# histogram of catchment area
p.area.hist <-
  ggplot(df.out, aes(x=area.km2, fill=network)) +
  geom_histogram(breaks=seq(0,5000,500), position="stack") +
  theme_bw()
ggsave(paste0(dir.plot, "NorthAmerica_01_SelectCatchments_p.area.hist.png"),
       p.area.hist, width=6, height=6, units="in")

# histograms of lat/lon
p.lat.hist <- 
  ggplot(df.out, aes(x=lat, fill=network)) +
  geom_histogram(breaks=seq(15,70,5), position="stack") +
  theme_bw() +
  coord_flip()

p.lon.hist <- 
  ggplot(df.out, aes(x=lon, fill=network)) +
  geom_histogram(breaks=seq(-160,-50,10), position="stack") +
  theme_bw()
ggsave(paste0(dir.plot, "NorthAmerica_01_SelectCatchments_p.lat.lon.hist.png"),
       arrangeGrob(p.lat.hist, p.lon.hist, ncol=2), width=12, height=6, units="in")

# map with points
usa <- map_data("worldHires", "USA")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")
p.map.points <-
  ggplot() +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_point(data=df.out, aes(x=lon, y=lat, color=network), shape=21) +
  coord_equal(xlim=c(-160, -50), ylim=c(18, 70)) +
  theme(legend.position="bottom")
ggsave(paste0(dir.plot, "NorthAmerica_01_SelectCatchments_p.map.points.png"),
       p.map.points, width=8, height=6, units="in")

