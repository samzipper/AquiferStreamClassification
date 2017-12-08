## NorthAmerica_02_GetUSGSdata.R
#' This script is intended to download USGS data corresponding to stream gauging stations in the
#' HCDN and NNRW reference gauge networks.

rm(list=ls())

source("C:/Users/Sam/WorkGits/AquiferStreamClassification/ProcessingScripts/paths+packages.R")

## read in data
# read in catchments selected for processing, from script NorthAmerica_01_SelectCatchments.R
df.in <- read.csv(paste0(dir.data, "NorthAmerica_01_SelectCatchments.csv"), stringsAsFactors=F)
df.in <- subset(df.in, network %in% c("HCDN-2009", "NNRW"))

## cycle through stations
for (i in 1:length(df.in$station)){
  # get station name
  station <- sprintf("%08d", as.numeric(df.in$station[i]))
  
  # download data from USGS
  df.station.in <- importDVs(station, code="00060", stat="00003", sdate="1900-01-01", edate="2016-12-31")
  
  # cfs to mm/d conversion factor
  cfs.to.mm <- (0.3048^3)*(0.001^3)*(1/df.in$area.km2[i])*86400*1000*1000
  
  # get columns of interest
  df.out <- data.frame(date = ymd(df.station.in$dates),
                       discharge.mm = df.station.in$val*cfs.to.mm)
  
  # save
  write.csv(df.out, paste0(dir.GAGES.Q, station, "_Discharge.csv"), row.names=F)
  
  # status update
  print(paste0(station, ": ", i, " of ", dim(df.in)[1], " complete, ", Sys.time()))
  
}
