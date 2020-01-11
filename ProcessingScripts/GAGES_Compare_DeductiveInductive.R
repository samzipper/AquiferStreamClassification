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
source(paste0(git.dir, "ProcessingScripts/euclidDist.R"))

# Script controls ---------------------------------------------------------

## controls
# select the first n bands explaining this proportion of total variance:
var.prc <- 0.9

# number of output classes
n.class.all <- seq(5,10)

# list of variables to use for deductive classification
vars.classify.ded <- c("event.size.mm", "sand.top15cm.prc.mean", "elev.m.range", "flat.overall", "precip.cold", 
                       "porosity.mean", "logk.mean", "WTD.m.mean", "DTB.cm.mean", "dryness", "prc.lakes")

# list of variables to use for inductive classification
vars.classify.ind <- c("BFI", "Q.mm_y", "Q90.mm_d", "Q10.mm_d", "FDC.slope", "Qs.range", "n.high.pulses")

## directories
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
df.combo <- left_join(df.ind, df.ded, by="basin")
df.combo <- df.combo[complete.cases(df.combo), ]

# clean up
rm("df.ind", "df.ded")

# Loop through variables --------------------------------------------------

first <- T
for (n.class in n.class.all){
  for (PCA.vars.ded in c(T,F)){
    for (PCA.vars.ind in c(T)){
      # get data
      df <- df.combo
      
      # Deductive classification, following Wolock --------------------------------------------------------
      
      # standardize input variables based on mean and sd
      vars.in.ded <- as.data.frame(apply(df[,vars.classify.ded], 2, scale))
      
      if (PCA.vars.ded){
        # PCA transformation
        fmla.ded <- as.formula(paste0("~ ", paste0(vars.classify.ded, collapse="+")))
        PCA.fit.ded <- prcomp(fmla.ded, data=vars.in.ded, na.action=na.omit)
        
        # select PCs to retain
        PCA.retain.ded <- min(which(summary(PCA.fit.ded)$importance["Cumulative Proportion", ] >= var.prc))
        
        # select PC output data
        PCs.ded <- PCA.fit.ded$x[,1:PCA.retain.ded]
        
        # cluster data
        cluster.ded.data <- PCs.ded
        
      } else {
        # cluster data
        cluster.ded.data <- vars.in.ded
        
      }
      
      # k-means clustering
      set.seed(1)
      fit.km.ded <- kmeans(cluster.ded.data, n.class)
      
      # add results to input 
      df$class.ded <- fit.km.ded$cluster
      
      # Inductive classification ------------------------------------------------
      
      # standardize input variables based on mean and sd
      vars.in.ind <- as.data.frame(apply(df[,vars.classify.ind], 2, scale))
      
      if (PCA.vars.ind){
        
        # PCA transformation
        fmla.ind <- as.formula(paste0("~ ", paste0(vars.classify.ind, collapse="+")))
        PCA.fit.ind <- prcomp(fmla.ind, data=vars.in.ind, na.action=na.omit)
        
        # select PCs to retain
        PCA.retain.ind <- min(which(summary(PCA.fit.ind)$importance["Cumulative Proportion", ] >= var.prc))
        
        # select PC output data
        PCs.ind <- PCA.fit.ind$x[,1:PCA.retain.ind]
        
        # cluster data
        cluster.ind.data <- PCs.ind
        
      } else {
        
        # cluster data
        cluster.ind.data <- vars.in.ind
        
      }
      
      # k-means clustering
      set.seed(1)
      fit.km.ind <- kmeans(cluster.ind.data, n.class)
      
      # add results to input 
      df$class.ind <- fit.km.ind$cluster
      
      # Calculate average distance to center ------------------------------------
      
      ## 1: average distance of inductive data to center of deductive clusters
      # find center of cluster for each group
      i.group <- which(colnames(df)=="class.ded")             # column to be used for grouping
      groups <- unique(df[,i.group])[order(unique(df[,i.group]))]  # get unique groups, in numeric/alphabetical order
      group.centers <- matrix(NA, nrow=length(groups), ncol=dim(cluster.ded.data)[2])
      df$ind.dist.to.ded.center <- NaN   # euclidean distance to center of each deductive group calculated from inductive coordinates
      for (g in groups){
        # extract data points
        i.ded.group <- which(df[,i.group]==g)
        data <- cluster.ded.data[i.ded.group,]
        
        # center (calculated as mean in each dimension)
        group.center <- t(as.matrix(apply(data, 2, mean)))
        
        # distance to center
        dist.to.center <- euclidDist(data, group.center)
        
        # add to data frame
        df$ind.dist.to.ded.center[i.ded.group] <- dist.to.center
      }
      
      ## 2: average distance of inductive data to center of inductive clusters
      # find center of cluster for each group
      i.group <- which(colnames(df)=="class.ind")             # column to be used for grouping
      groups <- unique(df[,i.group])[order(unique(df[,i.group]))]  # get unique groups, in numeric/alphabetical order
      group.centers <- matrix(NA, nrow=length(groups), ncol=dim(cluster.ind.data)[2])
      df$ind.dist.to.ind.center <- NaN   # euclidean distance to center of each deductive group calculated from inductive coordinates
      for (g in groups){
        # extract data points
        i.ind.group <- which(df[,i.group]==g)
        data <- cluster.ind.data[i.ind.group,]
        
        # center (calculated as mean in each dimension)
        group.center <- t(as.matrix(apply(data, 2, mean)))
        
        # distance to center
        dist.to.center <- euclidDist(data, group.center)
        
        # add to data frame
        df$ind.dist.to.ind.center[i.ind.group] <- dist.to.center
      }
      
      # Calculate performance indicators ----------------------------------------
      df.iter <- data.frame(n.class = n.class,
                            PCA.vars.ded = PCA.vars.ded,
                            PCA.vars.ind = PCA.vars.ind,
                            mean.ind.dist.to.ind.center = mean(df$ind.dist.to.ind.center),
                            mean.ind.dist.to.ded.center = mean(df$ind.dist.to.ded.center))
      df.iter$cluster.performance.ratio <- df.iter$mean.ind.dist.to.ded.center/df.iter$mean.ind.dist.to.ind.center
      
      # make overall data frame
      if (first){
        df.all <- df.iter
        first <- F
      } else {
        df.all <- rbind(df.all, df.iter)
      }
      
      # status update
      print(paste0(n.class, " ", PCA.vars.ded, " ", PCA.vars.ind, " complete"))
      
    }
  }
}

# Make plots --------------------------------------------------------------

# cluster performance ratio as a function of number of classes and whether deductive variables were PCA-transformed
p.class.PCA <-
  ggplot(df.all, aes(x=PCA.vars.ded, y=n.class, fill=cluster.performance.ratio)) +
  geom_raster() +
  scale_x_discrete("PCA-transform\ndeductive variables?") +
  scale_y_continuous("Number of Classes") +
  scale_fill_continuous(name="Cluster Performance\n(perfect=1)") +
  theme_bw() +
  theme(panel.grid=element_blank())
ggsave(paste0(plot.dir, "GAGES_Compare_DeductiveInductive_p.class.PCA.png"),
       p.class.PCA, width=4, height=6, units="in")


#### the below plots will no longer work

# deductive PCs, grouped by deductive clusters
p.scatter.PCs.ded.ded <-
  ggplot(df, aes(y=PC1.ded, x=PC2.ded)) +
  geom_point(aes(color=factor(class.ded)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.ded), color=factor(class.ded)), expand=unit(0.5, "lines")) +
  scale_fill_discrete(name="Deductive Class") +
  scale_color_discrete(name="Deductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")

# inductive PCs, grouped by deductive clusters
p.scatter.PCs.ind.ded <-
  ggplot(df, aes(y=PC1.ind, x=PC2.ind)) +
  geom_point(aes(color=factor(class.ded)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.ded), color=factor(class.ded)), expand=unit(0.5, "lines")) +
  scale_fill_discrete(name="Deductive Class") +
  scale_color_discrete(name="Deductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")

# inductive PCs, grouped by deductive clusters
p.scatter.PCs.ind.ind <-
  ggplot(df, aes(y=PC1.ind, x=PC2.ind)) +
  geom_point(aes(color=factor(class.ind)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.ind), color=factor(class.ind)), expand=unit(0.5, "lines")) +
  scale_fill_discrete(name="Inductive Class") +
  scale_color_discrete(name="Inductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")

aov(class.ded ~ PC1.ind + PC2.ind, data=df)

## scatterplots within deductive classification to evaluate groups
df.scatter.ded <- melt(subset(df, select=c(vars.classify, "class.ded")), id=c("dryness", "class.ded"))

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


## map of deductive classification

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

# plot
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
