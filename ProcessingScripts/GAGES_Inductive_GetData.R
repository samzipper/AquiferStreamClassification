## GAGES_Inductive_GetData.R
#' This is intended to extract the data necessary for inductive classification of
#' the GAGES catchments following various different procedures.
#' 
#' The data will then be saved as a CSV file.
#' 
#' This script outputs the following variables:
#'   -BFI = baseflow index
#'   -Q.mm_y = mean annual discharge [mm/yr]
#'   -Q90.mm_d = 10th percentile, flow duration curve [mm/day] (exceeded 90% of days)
#'   -Q10.mm_d = 90th percentile, flow duration curve [mm/day] (exceeded 10% of days)
#'   -FDC.slope = slope of flow duration curve between 33=66 percentiles
#'   -flow.winter = winter discharge seasonality indicator
#'   -flow.summer = summer discharge seasonality indicator
#'
#' These correspond to the following columns in the New_catchments_v9.xlsx spreadsheed from Carolina Massmann.
#'   -BFI = BFI_df (DB_discharge, col 4)
#'   -Q.mm_y = Qyear (DB_discharge, col 6)
#'   -Q90.mm_d = FDC_010 (DB_discharge, col 97)
#'   -Q10.mm_d = FDC_090 (DB_discharge, col 177)
#'   -FDC.slope = sl_FSC (DB_discharge, col 700)
#'   -flow.winter = Q_11-02 (DB_discharge, col 706)
#'   -flow.summer = Q_06-08 (DB_discharge, col 705)

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(dplyr)
require(ggplot2)
require(reshape2)

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/"

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# directory to save plots
plot.dir <- paste0(data.in.dir, "plots/")

## 1) get the data aggregated by Carolina
# read in necessary DB files
db.discharge <- read.csv(paste0(data.in.dir, "FromCarolina+Thorsten/DB_discharge.csv"), header=F, skip=6)

# subset to relevant columns
df.out <- data.frame(basin = db.discharge[,1],
                     BFI = db.discharge[,4],
                     Q.mm_y = db.discharge[,6],
                     Q90.mm_d = db.discharge[,97],
                     Q10.mm_d = db.discharge[,177],
                     FDC.slope = db.discharge[,700],
                     flow.winter = db.discharge[,706],
                     flow.summer = db.discharge[,705],
                     stringsAsFactors=F)

## 4) Save
write.csv(df.out, paste0(data.dir, "GAGES_Inductive_GetData.csv"), row.names=F)

## 5) Make some plots
# read in data
df.out <- read.csv(paste0(data.dir, "GAGES_Inductive_GetData.csv"))

# list of variables to plot
vars.plot <- c("BFI", "Q.mm_y", "Q90.mm_d", "Q10.mm_d", "FDC.slope", "flow.winter", "flow.summer")

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
ggsave(paste0(plot.dir, "GAGES_Inductive_GetData_p.dens.png"), p.dens, width=9, height=9, units="in")
