load(file = 'data/trips.20160613_19.all.RData')
library(tidyverse)
library(lubridate)
library(rgdal)

# translation bounds From: http://www.spatialreference.org/ref/epsg/2908/
EPSG2908 <- list( lonmin = -74.2700, lonmax = -71.7500,
                  latmin =  40.4700, latmax =  41.3100,
                  lonoff =   978000, latoff =   190000, # offset for meter (confirmed by "eyeballs")
                  theta = -.503)

canTrans <-
    univWeek %>%
    filter(
        EPSG2908$lonmin <  pickup_longitude &  pickup_longitude < EPSG2908$lonmax &
            EPSG2908$lonmin < dropoff_longitude & dropoff_longitude < EPSG2908$lonmax &
            EPSG2908$latmin <  pickup_latitude  &  pickup_latitude  < EPSG2908$latmax &
            EPSG2908$latmin < dropoff_latitude  & dropoff_latitude  < EPSG2908$latmax
    ) # removed 2% rows

pmetered <- latlon2meter(canTrans$pickup_longitude, canTrans$pickup_latitude)
dmetered <- latlon2meter(canTrans$dropoff_longitude, canTrans$dropoff_latitude)

set.seed(1236)

huge <- canTrans %>%
    mutate( cab_type_id = as.factor(cab_type_id),
            pom = pmetered@coords[,'lon'] - EPSG2908$lonoff, # pickup longitude in meter (offsetted)
            pam = pmetered@coords[,'lat'] - EPSG2908$latoff, # pickup latitude in meter  (offsetted)
            dom = dmetered@coords[,'lon'] - EPSG2908$lonoff,
            dam = dmetered@coords[,'lat'] - EPSG2908$latoff,
            #py_dep = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude, # angles distorted
            #px_dep = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude, # angles distorted
            py = rotateManhattanY(pam,pom,EPSG2908$theta), px = rotateManhattanX(pam,pom,EPSG2908$theta),
            dy = rotateManhattanY(pam,pom,EPSG2908$theta), dx = rotateManhattanX(pam,pom,EPSG2908$theta),
            h = hour(pickup_datetime), h2 = floor(h/2), h3 = floor(h/3), wday = wday(pickup_datetime, label=TRUE),
            rate = tip_amount / (total_amount - tip_amount),
            rateType = isConstant(rate), isCons = ifelse(rateType=='other', FALSE, TRUE),
            px4 = ceiling(px) - ceiling(px)%%250, py4 = ceiling(py) - ceiling(py)%%250,
            px3 = ceiling(px) - ceiling(px)%%100, py3 = ceiling(py) - ceiling(py)%%100,
            px2 = ceiling(px) - ceiling(px)%%50 , py2 =  ceiling(py) - ceiling(py)%%50,
            min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
            hpay = tip_amount*60/as.numeric(min), isHigh = as.numeric(hpay >= 12)
    ) %>% 
    filter( inMt & -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
    filter( -7000 < px & px < 5000 & 0 < py & py < 700000 ) %>%
    filter( 0 < min & min < 120 ) %>%
    filter( payment_type == 1 ) %>%
    sample_n(3000000)

huge <- huge %>% left_join(., huge %>% group_by(px4,py4) %>% summarize( n=n(), consRate = sum(isCons)/n ), by=c('px4','py4'))
