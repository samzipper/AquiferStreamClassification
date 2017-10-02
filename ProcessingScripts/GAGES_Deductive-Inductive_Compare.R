## GAGES_Deductive-Inductive_Compare.R
#' This script is intended to compare the results of deductive classification (GAGES_Deductive_Classify.R)
#' and inductive classification (GAGES_Inductive_Classify.R).

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(dplyr)
require(ggplot2)
require(reshape2)
require(ggforce)

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/plots/"

# Prep data ---------------------------------------------------------------

# load data
df.ind <- read.csv(paste0(data.dir, "GAGES_Inductive_Classify.csv"), stringsAsFactors=F)
df.ded <- read.csv(paste0(data.dir, "GAGES_Deductive_Classify.csv"), stringsAsFactors=F)

# merge data
df <- left_join(df.ind, df.ded, by="basin")
df <- df[complete.cases(df), ]

# Comparison plots --------------------------------------------------------

## for each deductive class, number of basins in each inductive class
df.counts <- dplyr::summarize(group_by(df, class.Wolock),
                              Q.1 = sum(class.Q.kmeans==1),
                              Q.2 = sum(class.Q.kmeans==2),
                              Q.3 = sum(class.Q.kmeans==3),
                              Q.4 = sum(class.Q.kmeans==4),
                              Q.5 = sum(class.Q.kmeans==5),
                              Q.6 = sum(class.Q.kmeans==6))
df.counts.melt <- melt(df.counts, id=c("class.Wolock"))
p.counts <-
  ggplot(df.counts.melt, aes(x=variable, y=factor(class.Wolock), fill=value)) +
  geom_raster() +
  scale_x_discrete("Inductive Class", expand=c(0,0)) +
  scale_y_discrete("Deductive Class", expand=c(0,0)) +
  scale_fill_continuous(name="Count", low="white", high="red") +
  theme_bw()

## as p.counts, but normalized based on number of basins in each inductive class
df.counts.ind <- dplyr::summarize(group_by(df, class.Q.kmeans),
                                  count = sum(is.finite(class.Wolock)))
df.counts.norm <- df.counts
df.counts.norm$Q.1 <- df.counts.norm$Q.1/df.counts.ind$count[df.counts.ind$class.Q.kmeans==1]
df.counts.norm$Q.2 <- df.counts.norm$Q.2/df.counts.ind$count[df.counts.ind$class.Q.kmeans==2]
df.counts.norm$Q.3 <- df.counts.norm$Q.3/df.counts.ind$count[df.counts.ind$class.Q.kmeans==3]
df.counts.norm$Q.4 <- df.counts.norm$Q.4/df.counts.ind$count[df.counts.ind$class.Q.kmeans==4]
df.counts.norm$Q.5 <- df.counts.norm$Q.5/df.counts.ind$count[df.counts.ind$class.Q.kmeans==5]
df.counts.norm$Q.6 <- df.counts.norm$Q.6/df.counts.ind$count[df.counts.ind$class.Q.kmeans==6]
df.counts.norm.melt <- melt(df.counts.norm, id=c("class.Wolock"))
p.counts.norm <-
  ggplot(df.counts.norm.melt, aes(x=variable, y=factor(class.Wolock), fill=value)) +
  geom_raster() +
  scale_x_discrete("Inductive Class", expand=c(0,0)) +
  scale_y_discrete("Deductive Class", expand=c(0,0)) +
  scale_fill_continuous(name="Prop. of\nInd. Class", low="white", high="red") +
  theme_bw()
ggsave(paste0(plot.dir, "GAGES_Deductive-Inductive_Compare_p.counts.norm.png"),
       p.counts.norm, width=8, height=6, units="in")

## scatter plots comparing streamflow variables, colored by deductive class
df.scatter <- subset(df, select=c("BFI", "Q.mm_y", "Q90.mm_d", "Q10.mm_d", "FDC.slope", "flow.winter", "flow.summer", "class.Wolock"))
df.scatter.BFI <- melt(df.scatter, id=c("BFI", "class.Wolock"))

p.scatter.BFI <-
  ggplot(df.scatter.BFI, aes(y=BFI, x=value)) +
  facet_wrap(~variable, scales="free") +
  geom_point(aes(color=factor(class.Wolock)), shape=21) +
  geom_mark_hull(aes(fill=factor(class.Wolock)), color=NA) +
  scale_fill_discrete(name="Deductive Class") +
  scale_color_discrete(name="Deductive Class") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        legend.position="bottom")
ggsave(paste0(plot.dir, "GAGES_Deductive-Inductive_Compare_p.scatter.BFI.png"),
       p.scatter.BFI, width=8, height=6, units="in")
