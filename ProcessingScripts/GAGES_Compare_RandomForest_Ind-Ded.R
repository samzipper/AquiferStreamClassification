## GAGES_Compare_RandomForest_Ind-Ded.R
#' This script is intended to make a random forest model
#' to predict each inductive variable using all the 
#' deductive variables as inputs.
#' 
#' Output will be plots of variable importance for each
#' inductive variable.
#' 
#' Tutorial on Random Forests: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(ggplot2)
require(gridExtra)
require(randomForest)
require(dplyr)

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
  fit <- randomForest(fmla,
                      data=df.var.scale, 
                      importance=TRUE, 
                      ntree=2000)
  
  
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

# summarize for model fit
df.var.ind.fit <- dplyr::summarize(group_by(df.var.all, var.ind),
                                   r2 = mean(r2.mean))

## plots of variable importance
# raster
p.var.imp.raster <- 
  ggplot(df.var.all, aes(x=var.ded, y=var.ind, fill=var.ded.ChangeMSE)) +
  geom_raster() +
  theme_bw() +
  scale_x_discrete(name="Deductive Variable") +
  scale_y_discrete(name="Inductive Variable") +
  scale_fill_gradient(name="Change in\nStandardized MSE") +
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

ggsave(paste0(plot.dir, "GAGES_Compare_RandomForest_Ind-Ded.png"),
       arrangeGrob(p.var.imp.box, p.var.imp.raster, ncol=2),
       width=8, height=6, units="in")

# plot of model fit
p.var.ind.fit <- 
  ggplot(df.var.ind.fit, aes(x=var.ind, y=r2)) +
  geom_bar(stat="identity") +
  scale_x_discrete(name="Inductive Variable") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid=element_blank())

# summarize by means
df.var.ded <- dplyr::summarize(group_by(df.var.all, var.ded),
                               imp.mean = mean(var.ded.ChangeMSE))
df.var.ded <- df.var.ded[order(-df.var.ded$imp.mean),]
