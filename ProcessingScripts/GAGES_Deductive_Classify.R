## GAGES_Deductive_Classify.R
#' This script loads output from GAGES_Deductive_GetData.R and classifies
#' the catchments following the procedure of Wolock et al. (2004).

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(ggplot2)
require(dplyr)
require(rgdal)
require(broom)
require(sp)
require(raster)

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/plots/"

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/FromCarolina+Thorsten/"

## script controls
# list of variables to use for classification
vars.classify <- c("sand.prc", "k.log", "defc.mm", "elev.m.range", "flat.overall", "flat.upland", "flat.lowland")

# select the first n bands explaining this proportion of total variance:
var.prc <- 0.8

# number of output classes
n.class <- 6

## load data from GAGES_Deductive_GetData.R
# load data
df <- read.csv(paste0(data.dir, "GAGES_Deductive_GetData.csv"))

# trim to complete cases only
df <- df[complete.cases(df), ]

## calculate derived variables
# precipitation deficit [mm] - used in Wolock et al
df$defc.mm <- df$precip.mm - df$pet.mm

# flatland in lowlands [%] - used in Wolock et al
df$flat.lowland <- 100 - df$flat.upland

# Feddema dryness index [-] - used by Leibowitz et al
df$dryness <- NaN
df$dryness[df$pet.mm > df$precip.mm] <- 
  df$precip.mm[df$pet.mm > df$precip.mm]/df$pet.mm[df$pet.mm > df$precip.mm] - 1
df$dryness[df$precip.mm > df$pet.mm] <- 
  1 - df$pet.mm[df$precip.mm > df$pet.mm]/df$precip.mm[df$precip.mm > df$pet.mm]


# 1. Wolock et al. (2004) classification based on clustering --------------
# standardize input variables based on mean and sd
vars.scale <- as.data.frame(apply(df[,vars.classify], 2, scale))

# PCA transformation
fmla <- as.formula(paste0("~ ", paste0(vars.classify, collapse="+")))
PCA.fit <- prcomp(fmla, data=vars.scale, na.action=na.omit)

# select PCs to retain
PCA.retain <- min(which(summary(PCA.fit)$importance["Cumulative Proportion", ] >= var.prc))

# select PC output data
PCs <- PCA.fit$x[,1:PCA.retain]

# k-means clustering
set.seed(1)
fit.km <- kmeans(PCs, n.class)

# add results to input 
df$class.Wolock <- fit.km$cluster

# summarize by cluster
df.class.Wolock <- summarize(group_by(df, class.Wolock),
                             sand.prc.mean = mean(sand.prc),
                             k.log.mean = mean(k.log),
                             defc.mm.mean = mean(defc.mm),
                             elev.m.range.mean = mean(elev.m.range),
                             flat.overall.mean = mean(flat.overall),
                             flat.upland.mean = mean(flat.upland),
                             flat.lowland.mean = mean(flat.lowland))

# 2. Leibowitz et al. (2016) classification based on categories -----------
# categorize variables
df$class.sand <- cut(df$sand.prc, quantile(df$sand.prc, seq(0,1,length.out=3)), 
                     include.lowest=T, labels=c("L", "H"))

df$class.perm <- cut(df$k.log, quantile(df$k.log, seq(0,1,length.out=3)), 
                     include.lowest=T, labels=c("L", "H"))

df$class.climate <- cut(df$dryness, c(-1, -0.66, -0.33, 0, 0.33, 0.66, 1), 
                        include.lowest=T, labels=c("A", "S", "D", "M", "W", "V"))

df$class.seasonality <- "S"
df$class.seasonality[df$precip.winter > df$precip.summer] <- "W"

df$class.terrain <- "T"
df$class.terrain[df$flat.overall < 10 & df$elev.m.range > 300] <- "M"
df$class.terrain[df$flat.overall > 50] <- "F"

# overall classification
df$class.Leibowitz <- factor(paste(df$class.climate, df$class.seasonality, df$class.perm, df$class.terrain, df$class.sand, sep="-"))

# Make maps ---------------------------------------------------------------



# load shapefile
shp <- readOGR(dsn=paste0(data.in.dir, "shapefiles"), layer="basins_CONUS")

# subset to basins with data
shp <- subset(shp, Basin_ID %in% df$basin)

# reproject to WGS84
shp <- spTransform(shp, CRS("+init=epsg:4326"))

# fortify to data frame
shp@data$id <- rownames(shp@data)
df.map <- tidy(shp)
df.map <- left_join(df.map, shp@data, by="id")
df.map$basin <- as.numeric(levels(df.map$Basin_ID)[as.numeric(df.map$Basin_ID)])

# get rid of junk columns
df.map$test <- NULL
df.map$test2 <- NULL
df.map$Centroid <- NULL
df.map$X_centroid <- NULL
df.map$Y_centroid <- NULL

# join with data frame
df.map <- left_join(df.map, df, by="basin")

# US outline
df.usa <- map_data("usa")

# make a plot of cluster
p.map.cluster <-
  ggplot() +
  geom_polygon(data=df.usa, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=df.map, aes(x=long, y=lat, fill=factor(class.Wolock), group=basin)) +
  theme_bw() +
  theme(panel.grid=element_blank())