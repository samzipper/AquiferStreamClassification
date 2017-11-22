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
require(gridExtra)

## paths
# path to git repository
dir.git <- "C:/Users/Sam/WorkGits/CONUS_CatchmentClassification/"

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

# path to CANOPEX data on GSAS
dir.CANOPEX <- "Z:/2.active_projects/Zipper/1.Spatial_data/regional/NorthAmerica/ws_watersheds/1original/CANOPEX/"

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
