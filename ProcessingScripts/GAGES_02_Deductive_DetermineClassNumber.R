## GAGES_Deductive_DetermineClassNumber.R
#' This script is intended to determine the optimal number of classes for k-means clustering
#' based on the relationship between within-class variance and the number of classes.

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
require(mclust)

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

# Data processing  --------------------------------------------------------

# list of variables to use for classification
vars.classify <- c("event.size.mm", "sand.top15cm.prc.mean", "elev.m.range", "flat.overall", "precip.cold", "porosity.mean", "logk.mean", "WTD.m.mean", "DTB.cm.mean", "dryness", "prc.lakes")

# select the first n bands explaining this proportion of total variance:
var.prc <- 0.8

# standardize input variables based on mean and sd
vars.scale <- as.data.frame(apply(df[,vars.classify], 2, scale))

# PCA transformation
fmla <- as.formula(paste0("~ ", paste0(vars.classify, collapse="+")))
PCA.fit <- prcomp(fmla, data=vars.scale, na.action=na.omit)

# select PCs to retain
PCA.retain <- min(which(summary(PCA.fit)$importance["Cumulative Proportion", ] >= var.prc))

# select PC output data
PCs <- PCA.fit$x[,1:PCA.retain]

# k-means analysis ---------------------------------------

# data frame for output
df.n <- data.frame(n.classes = seq(2,30),
                   ss.within = NaN,   # within-cluster sum of squares
                   ss.between = NaN)  # between-cluster sum of squares

for (n in df.n$n.classes){
  # set seed
  set.seed(1)
  
  # cluster
  fit.km <- kmeans(PCs, n)
  
  # extract fit data
  df.n$ss.within[df.n$n.classes==n]  <- fit.km$tot.withinss
  df.n$ss.between[df.n$n.classes==n] <- fit.km$betweenss
  
}

# plot
df.n.melt <- melt(df.n, id="n.classes")
p.var <- 
  ggplot(df.n.melt, aes(x=n.classes, y=value)) +
  geom_point() + 
  facet_wrap(~variable, ncol=1, labeller=as_labeller(c("ss.within"="Within-Class", "ss.between"="Between Class"))) +
  scale_x_continuous(name="Number of Classes") +
  scale_y_continuous(name="Sum of Squres") +
  theme_bw()

# reclassify based on optimal number of classes
fit.km <- kmeans(PCs, 16)
class.km <- fit.km$cluster

# Bayesian clustering -----------------------------------------------------

fit.clust.BIC <- mclustBIC(PCs, G=5:25)
plot(fit.clust.BIC)
summary(fit.clust.BIC)

fit.clust <- Mclust(PCs, G=5:25, x=fit.clust.BIC)

# extract fit probability data
df.mclust.prob <- fit.clust$z

# vector of classification
class.mclust <- fit.clust$classification

# Nb Cluster comparison metrics -------------------------------------------

fit.nb <- NbClust(df[,vars.classify], min.nc=5, max.nc=25, method="kmeans")

# classification comparison -----------------------------------------------

p.clust.comp <- 
  ggplot(data.frame(mclust=class.mclust, km=class.km), aes(x=mclust, y=km)) + 
  geom_bin2d()
