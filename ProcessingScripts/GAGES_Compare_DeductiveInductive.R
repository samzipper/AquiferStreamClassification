## GAGES_Compare_DeductiveInductive.R
#' This script is intended to perform a deductive classification and 
#' compare the results to some streamflow metrics.

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(ggplot2)
require(dplyr)
require(rgdal)
require(broom)
require(sp)
require(raster)
require(ggforce)
require(reshape2)

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/plots/"

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/FromCarolina+Thorsten/"

# Prep data ---------------------------------------------------------------

# load data
df.ind <- read.csv(paste0(data.dir, "GAGES_Inductive_GetData.csv"), stringsAsFactors=F)
df.ded <- read.csv(paste0(data.dir, "GAGES_Deductive_GetData.csv"), stringsAsFactors=F)

# merge data
df <- left_join(df.ind, df.ded, by="basin")
df <- df[complete.cases(df), ]

# Deductive classification, following Wolock --------------------------------------------------------

# list of variables to use for classification
vars.classify <- c("event.size.mm", "sand.top15cm.prc.mean", "elev.m.range", "flat.overall", "precip.cold", "porosity.mean", "logk.mean", "WTD.m.mean", "DTB.cm.mean", "dryness", "prc.lakes")

# select the first n bands explaining this proportion of total variance:
var.prc <- 0.8

# number of output classes
n.class <- 6

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
df$class.ded <- fit.km$cluster

# summarize by cluster
df.class.ded <- summarize(group_by(df, class.ded),
                          sand.prc.mean = mean(sand.prc),
                          k.log.mean = mean(k.log),
                          defc.mm.mean = mean(defc.mm),
                          elev.m.range.mean = mean(elev.m.range),
                          flat.overall.mean = mean(flat.overall),
                          flat.upland.mean = mean(flat.upland),
                          flat.lowland.mean = mean(flat.lowland))

# Set up map data ---------------------------------------------------------------

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


# Make plots --------------------------------------------------------------

## map of deductive classification
p.map.ded <-
  ggplot() +
  geom_polygon(data=df.usa, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=df.map, aes(x=long, y=lat, fill=factor(class.ded), group=basin)) +
  scale_fill_discrete(name="Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")
ggsave(paste0(plot.dir, "GAGES_Compare_DeductiveInductive_p.map.ded.png"),
       p.map.ded, width=8, height=6, units="in")

## scatterplots within deductive classification to evaluate groups
df.scatter.ded <- subset(df, select=c(vars.classify, "class.ded"))
df.scatter.ded <- melt(df.scatter.ded, id=c("dryness", "class.ded"))

p.scatter.dryness <-
  ggplot(df.scatter.ded, aes(y=dryness, x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_point(aes(color=factor(class.ded)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.ded)), color=NA) +
  scale_fill_discrete(name="Deductive Class") +
  scale_color_discrete(name="Deductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")
ggsave(paste0(plot.dir, "GAGES_Compare_DeductiveInductive_p.scatter.dryness.png"),
       p.scatter.dryness, width=8, height=6, units="in")

## scatter plots comparing streamflow variables, colored by deductive class
df.scatter.ind <- subset(df, select=c("BFI", "Q.mm_y", "Q90.mm_d", "Q10.mm_d", "FDC.slope", "Qs.range", "n.high.pulses", "class.ded"))
df.scatter.ind <- melt(df.scatter.ind, id=c("BFI", "class.ded"))

p.scatter.BFI <-
  ggplot(df.scatter.ind, aes(y=BFI, x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_point(aes(color=factor(class.ded)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.ded)), color=NA) +
  scale_fill_discrete(name="Deductive Class") +
  scale_color_discrete(name="Deductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")
ggsave(paste0(plot.dir, "GAGES_Compare_DeductiveInductive_p.scatter.BFI.png"),
       p.scatter.BFI, width=8, height=6, units="in")
