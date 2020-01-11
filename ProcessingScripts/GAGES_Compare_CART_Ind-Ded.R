## GAGES_Compare_CART_Ind-Ded.R
#' This script is intended to make a CART decision tree
#' to predict each inductive variable using all the 
#' deductive variables as inputs.
#' 
#' Output will be plots of variable importance for each
#' inductive variable.
#' 
#' Tutorial on Random Forests: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(ggplot2)
require(randomForest)
require(rpart)
require(rpart.plot)
require(rattle)
require(RColorBrewer)

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/plots/"

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/FromCarolina+Thorsten/"

# list of deductive variables
vars.classify.ded <- c("event.size.mm", "sand.top15cm.prc.mean", "elev.m.range", "flat.overall", "precip.cold", "porosity.mean", "logk.mean", "WTD.m.mean", "DTB.cm.mean", "dryness", "prc.lakes")

# list of inductive variables
vars.classify.ind <- c("BFI", "Q.mm_y", "Q90.mm_d", "Q10.mm_d", "FDC.slope", "Qs.range", "n.high.pulses")

# Prep data ---------------------------------------------------------------

# load data
df.ind <- read.csv(paste0(data.dir, "GAGES_Inductive_GetData.csv"), stringsAsFactors=F)
df.ded <- read.csv(paste0(data.dir, "GAGES_Deductive_GetData.csv"), stringsAsFactors=F)

# merge data
df <- left_join(df.ind, df.ded, by="basin")
df <- df[complete.cases(df[,c(vars.classify.ded, vars.classify.ind)]), ]

# clean up
rm("df.ind", "df.ded")

# Build random forests ----------------------------------------------------

first.var <- T
for (var.ind in vars.classify.ind){
  
  # extract variables of interest
  df.var <- subset(df, select=c(var.ind, vars.classify.ded))
  
  # normalize to z-score
  df.var.scale <- as.data.frame(apply(df.var, 2, scale))
  
  # set up formula
  fmla <- as.formula(paste0(var.ind, " ~ ", paste(vars.classify.ded, collapse="+")))
  
  # fit random forest
  set.seed(1)
  fit <- rpart(fmla, data=df.var.scale)
  
  
  # construct data frame to save
  df.var <- data.frame(var.ind = var.ind,
                       var.ded = row.names(fit$importance),
                       var.ded.ChangeMSE = fit$importance[,1],
                       r2.mean = mean(fit$rsq))
  
  if (first.var){
    df.var.all <- df.var
    first.var <- F
  } else {
    df.var.all <- rbind(df.var.all, df.var)
  }
  
  # status update
  print(paste0(var.ind, " complete"))
  
}

## plots of variable importance
# raster
p.var.imp.raster <- 
  ggplot(df.var.all, aes(x=var.ded, y=var.ind, fill=var.ded.ChangeMSE)) +
  geom_raster() +
  theme_bw() +
  scale_x_discrete(name="Deductive Variable") +
  scale_y_discrete(name="Inductive Variable") +
  scale_fill_gradient(name="Change in Standardized MSE") +
  theme(legend.position="top",
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.grid=element_blank())

# boxplot
p.var.imp.box <- 
  ggplot(subset(df.var.all, var.ded="dryness"), aes(x=var.ded, y=var.ded.ChangeMSE)) +
  geom_boxplot() +
  geom_point(position="jitter", shape=21, color="blue") +
  theme_bw() +
  scale_x_discrete(name="Deductive Variable") +
  scale_y_continuous(name="Change in Standardized MSE") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.grid=element_blank())