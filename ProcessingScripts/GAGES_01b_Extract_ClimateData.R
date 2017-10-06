## GAGES_Extract_ClimateData.R
#' This script is intended to extract some climate variables from the raw .mat
#' to supplement those included in the New_catchments_v9.xlsx spreadsheet.

rm(list=ls())

# git directory
git.dir <- "C:/Users/Sam/WorkGits/GAGES_Classification/"

require(R.matlab)
require(raster)
require(dplyr)
require(ggplot2)
require(reshape2)
require(rgdal)
require(rgeos)
require(broom)
require(lubridate)

# directory containing data from Carolina and Thorsten
data.in.dir <- "C:/Users/Sam/Dropbox/Work/HydrologicLandscapes/GAGES/FromCarolina+Thorsten/Forcings/CData/"

# directory to save output data
data.dir <- paste0(git.dir, "data/")

# get list of raw files
files <- list.files(data.in.dir)

# make output data frame
df.out <- data.frame(files=files,
                     basin="NA",
                     event.size.mm=NaN,
                     stringsAsFactors=F)

# scroll through files
for (f in files){
  # index of out
  i.out <- which(df.out$files==f)
  
  # get basin ID
  df.out$basin[i.out] <- substr(f, 10, 17)
  
  # load .mat file
  data <- readMat(paste0(data.in.dir, f), sparseMatrixClass="matrix")
  
  # extract data
  df.in <- data.frame(date=ymd(apply(data[[1]][[2]][[1]][[5]], 1, paste, collapse="-")),
                      precip=as.numeric(data[[1]][[2]][[1]][[1]]))
  
  # subset to period of interest
  df.in <- subset(df.in, date >= ymd("1980-10-1") & date <= ymd("2009-09-30"))
  
  # get mean of nonzero precip days
  df.out$event.size.mm[i.out] <- mean(df.in$precip[df.in$precip!=0])
  
  # status update
  print(paste0(i.out, " ", df.out$basin[i.out], " complete, ", round(df.out$event.size.mm[i.out], 2), " mm"))
}

# write output
write.csv(df.out, paste0(data.dir, "GAGES_Extract_ClimateData.csv"), row.names=F)
