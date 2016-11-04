
init <- function(
  
){
  library(ggplot2)
  library(ggmap)
  library(dplyr)
  library(reshape2)
  library(zoo)
  library(scales)
  library(extrafont)
  library(grid)
  library(RPostgreSQL)
  library(rgdal)
  library(maptools)
  library(gpclib)
  library(sp)
  gpclibPermit() # support for gpclib will be withdrawn from maptools at the next major release
  source("/home/PCUser/nyc-taxi-data/analysis/helpers.R")
  
  assign('gcl',
         list(
           root = '/home/PCUser/nyc-taxi-data'
         ), envir = .GlobalEnv)
  return(setwd(gcl$root))
}

generateNewYorkMap <- function(
  
){
  tracts = sp::spTransform(rgdal::readOGR(dsn="./nyct2010_15b", layer = "nyct2010"),
                           sp::CRS("+proj=longlat +datum=WGS84"))
  # the +towgs84 tag should be used where needed to make sure that datum transformation does take place
  tracts@proj4string
  tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
  tracts.points = ggplot2::fortify(tracts, region = "id") # FIXME: library(broom) is preferred
  # if rror: isTRUE(gpclibPermitStatus()) is not TRUE, library(gpclib)
  tracts.map = inner_join(tracts.points, tracts@data, by = "id")
  
  nyc_map = tracts.map
  ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")
  manhattan_map = filter(tracts.map, BoroName == "Manhattan")

  return( list(nyc = nyc_map, mt = manhattan_map, ex = ex_staten_island_map) )  
}


