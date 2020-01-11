## CAMELS_CompareStationsToNNRW.R
#' This script is intended to compare the stations included in the CAMELS dataset
#' to the National Network of Reference Watersheds (https://my.usgs.gov/nnrw/main/home)

rm(list=ls())

source("C:/Users/Sam/WorkGits/CONUS_CatchmentClassification/ProcessingScripts/paths+packages.R")

## load CAMELS names
camels.names <- read.table(paste0(dir.CAMELS, "camels_attributes_v2.0/camels_name.txt"), sep=";", header=T, quote="")
camels.topo <- read.table(paste0(dir.CAMELS, "camels_attributes_v2.0/camels_topo.txt"), sep=";", header=T, quote="")
camels.merge <- left_join(camels.names, camels.topo, by="gauge_id")

## load NNRW names
NNRW.names <- read.table(paste0(dir.NNRW, "NNRW_CoreWatersheds.csv"), sep=",", header=T)

## extract names only
camels.stations <- sprintf("%08d", camels.merge$gauge_id)
NNRW.stations <- sprintf("%08d", NNRW.names$Site.Code)

## what overlap is there?
stations.both <- camels.stations[camels.stations %in% NNRW.stations]
stations.camels.only <- camels.stations[!(camels.stations %in% NNRW.stations)]
stations.NNRW.only <- NNRW.stations[!(NNRW.stations %in% camels.stations)]

## get distribution of area
df.NNRW <- data.frame(station = NNRW.stations,
                      area.km2 = NNRW.names$Watershed.Area..km.2.)

df.camels <- data.frame(station = camels.stations,
                        area.gages2.km2 = camels.merge$area_gages2,
                        area.geospatial.km2 = camels.merge$area_geospa_fabric)

df.both <- rbind(data.frame(station = stations.both,
                            network = "both"),
                 data.frame(station = stations.camels.only,
                            network = "camels"),
                 data.frame(station = stations.NNRW.only,
                            network = "NNRW"))

df.both <- left_join(df.both, df.NNRW, by="station")
df.both$area.km2[is.na(df.both$area.km2)] <- df.camels$area.gages2.km2[match(df.both$station[is.na(df.both$area.km2)], df.camels$station)]

## compare area for common watersheds
df.common <- left_join(subset(df.NNRW, station %in% stations.both), subset(df.camels, station %in% stations.both))

p.area.gages.compare <-
  ggplot(df.common, aes(y=area.km2, x=area.gages2.km2)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point() +
  stat_smooth(method="lm") +
  theme_bw()

p.area.geospatial.compare <-
  ggplot(df.common, aes(y=area.km2, x=area.geospatial.km2)) +
  geom_abline(intercept=0, slope=1, color="gray65") +
  geom_point() +
  stat_smooth(method="lm") +
  theme_bw()

# histogram
p.hist.area <-
  ggplot(df.both, aes(x=area.km2, fill=network)) +
  geom_histogram()

# area above a threshold
sum(df.both$area.km2>5000)
