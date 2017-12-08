## path+packages.R
# paths, packages, functions, etc which will be used in various processing scripts

## packages
require(ggplot2)
require(dplyr)
require(reshape2)
require(rgeos)
require(rgdal)
require(maptools)
require(mapdata)
require(grid)
require(gridExtra)
require(gtable)
require(stringr)
require(lubridate)
require(quantreg)
require(zoo)
require(hydrostats)
require(waterData)
require(bio3d)  # needed for rle2 function

## paths
# path to git repository
dir.git <- "C:/Users/Sam/WorkGits/AquiferStreamClassification/"

# path to Dropbox
dir.drop <- "C:/Users/Sam/Dropbox/"

# path to derived datasets in git repository
dir.data <- paste0(dir.git, "data/NorthAmerica/")

# path to CAMELS data on GSAS
dir.CAMELS <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/ws_watersheds/1original/CAMELS/"

# path to NNRW data on GSAS
dir.NNRW <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/ws_watersheds/1original/NNRW/"

# path to GAGESII data on GSAS
dir.GAGES <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/ws_watersheds/1original/GAGESII/"

# path to GAGESII gap-filled streamflow data on GSAS
dir.GAGES.Q <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/riv_river_network_streamflow/2derived/GAGESII/"

# path to CANOPEX data on GSAS
dir.CANOPEX <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/ws_watersheds/1original/CANOPEX/"

# which CANOPEX data to use? NRCAN or ENVCAN
data.CANOPEX <- "NRCAN"

# path to save plots
dir.plot <- paste0(dir.drop, "Work/HydrologicLandscapes/NorthAmerica/Plots/")

## functions
# function for state abbreviation - function from https://gist.github.com/ligyxy/acc1410041fe2938a2f5
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}

# function to gap-fill hydro data
fillMissNoPlot <- function (dataset, block = 30, pmiss = 40, pmin = 365, model = "trend", smooth = TRUE, ...){
  # this is based on the FillMiss script from waterData package but modified to:
  #  -not make a plot
  #  -get rid of long gaps but preserve data from before/after
  pck <- is.na(dataset$val)
  
  # deal with gaps < length block
  pck.1s <- rep(1,length(pck))
  pck.1s[pck] <- NA
  pck.1s <- as.numeric(na.approx(pck.1s, maxgap=block))
  pck[pck.1s==1] <- F
  
  percent <- 0
  max.mis <- 0
  if (sum(pck) > 0) {
    percent <- 100*(sum(pck)/length(dataset$val))
    rles <- rle2(pck)
    
    # data to save: FALSE runs > pmin
    lengths <- c(0, rles$lengths)
    values <- c(T, rles$values)
    inds <- c(0, rles$inds)
    
    # figure out data to keep (length F >= pmin)
    rle.keep <- which(!values & lengths>pmin)
    i.keep.end <- inds[rle.keep]
    i.keep.start <- inds[rle.keep-1]+1
    
    # get indices to keep
    start.flag <- T
    for (i in 1:length(i.keep.start)){
      keep <- seq(i.keep.start[i], i.keep.end[i], 1)
      if (start.flag){
        keep.all <- keep
        start.flag <- F
      } else {
        keep.all <- c(keep.all, keep)
      }
    }
    
    # subset dataset
    dataset <- dataset[keep.all, ]
    
    # get indices of missing data now
    pck <- is.na(dataset$val)
    
    # gap-fill
    my.series <- window(dataset$val)
    my.struct <- StructTS(my.series, type = model)
    if (smooth) {
      fit <- tsSmooth(my.struct)
    } else {
      fit <- fitted(my.struct)
    } 
    dataset$val[pck] <- fit[pck, 1]
    lng <- length(grep("fM", levels(dataset$qualcode)))
    if (lng < 1) {
      dataset$qualcode <- factor(dataset$qualcode, levels = c(levels(dataset$qualcode), 
                                                              "fM"))
    }
    dataset$qualcode[pck] <- "fM"
  }
  dataset
}
