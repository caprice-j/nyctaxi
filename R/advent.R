
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
  library(lubridate)
  gpclibPermit() # support for gpclib will be withdrawn from maptools at the next major release
  if ( Sys.info()['sysname'] == 'Darwin' ){
      #source("/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyc")
       libDir <- '/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyc-taxi-data'
      rootDir <- '/Users/PCUser/Dropbox/CU2016/F16CLASSES/BIG_Ching_Yang_Lin/nyctaxi'
  } else {
      source("/home/PCUser/nyc-taxi-data/analysis/helpers.R")
       libDir <- '/home/PCUser/nyc-taxi-data'
      rootDir <- '~/nyctaxi'
  }

  
  assign('gcl',
         list(
           lib = libDir,
           root = rootDir,
           trips20160613_19allRData = paste0(rootDir,'/data/trips.20160613_19.all.RData')
         ), envir = .GlobalEnv)
  return(setwd(gcl$lib))
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
  

  return( list(nyc = nyc_map, mt = manhattan_map, ex = ex_staten_island_map,
               mtg = list(geom_polygon(data = manhattan_map, aes(x = long, y = lat, group = group), fill = "#080808", color = "#080808"))) )  
}

# function for dplyr
inManhattan <- function(
  lonv, latv,
  predefined=c('manhattan')[1],
  slope=1, intercept=114.69
){
  if (predefined == 'manhattan')
    return( ifelse(1 * lonv + 114.69 - latv < 0, TRUE, FALSE) )

  return( slope * lonv + intercept - latv < 0, TRUE, FALSE )
}

isConstant <- function(ratiov) {
  return(
      ifelse(( .19999 < ratiov & ratiov < .20001),
             "20%",
             ifelse(( .24999 < ratiov & ratiov < .25001),
                    "25%",
                    ifelse(( .29999 < ratiov & ratiov < .30001),
                           "30%",
                           "other")))
  )
}

latlon2meter <- function(lon, lat)
    spTransform(SpatialPoints(cbind(lon,lat), proj4string=CRS("+ellps=WGS84 +datum=WGS84 +proj=longlat")),
                CRS("+init=epsg:2908"))
rotateManhattanY <- function(latm, lonm, theta)  cos(theta) * latm  - sin(theta) * lonm
rotateManhattanX <- function(latm, lonm, theta)  sin(theta) * latm  + cos(theta) * lonm