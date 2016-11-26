# http://www.ehow.com/info_8496448_much-make-new-york-city.html
# says NYC taxi drivers earn $14.74 per hour on average.

#
# "Income for owners of independent medallions is derived from
# the fares and tips received from passengers less the
# cost of owning and maintaining a vehicle and medallion. "
#  http://www.nyc.gov/html/tlc/downloads/pdf/2014_taxicab_fact_book.pdf
# 

load(file = 'data/trips.20160613_19.all.RData')
library(tidyverse)
library(lubridate)
set.seed(1236)
theta <- -.632 # 
inTimesSquare <- function(px,py) return(-83.775 < px & px < -83.765 & -10.82 < py & py < -10.81)
in11thAve     <- function(px,py) return(-83.7856 < px & px < -83.78495 )
huge <- univWeek %>% filter( inMt & -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
    mutate( py = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude,
            px = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude,
            dy = cos(theta) * dropoff_latitude - sin(theta) * dropoff_longitude,
            dx = sin(theta) * dropoff_latitude + cos(theta) * dropoff_longitude,
            h = hour(pickup_datetime), h2 = floor(h/2), h3 = floor(h/3),
            day = day(pickup_datetime),
            min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
            hpay = tip_amount*60/as.numeric(min),
            inTS = inTimesSquare(px,py), in11thAve = in11thAve(px,py),
            px3 = round(px,3), py3 = round(py,3),
            px4 = round(px,4), py4 = round(py,4)) %>% 
    filter( -83.79 < px & px < -83.745 & -10.9 < py & py < -10.725 ) %>%
    filter( payment_type == 1 ) %>%
    sample_n(3000000)

huge %>% group_by(day) %>% summarize_each(funs(min,mean,median,max), tip_amount, min)
huge %>% mutate( hfare=as.numeric(min)/fare_amount*60) %>% summarize_each(funs(mean,median,max), tip_amount, min, hfare)
huge %>% group_by(day) %>% filter( min == max(min)) # what's these 24h trips?

as.numeric(huge$min[1])/mini$fare_amount[1]*60

# => not promising

# street analysis

with(huge,plot(px,py,pch=18, cex=.3))
text(landmark$x, landmark$y, label="TSQ", col="red", cex=1.5)

with(huge%>%filter(in11thAve),plot(px,py, pch=18))

huge %>% filter(in11thAve) %>% mutate( t = day*8 + h3 ) %>% group_by(py3,t) %>% summarise( n = n(), tip = median(tip_amount), hpay= mean(hpay) ) %>% ggplot() + geom_rect(aes(xmin=t,xmax=t+1,ymin=py3-.0005,ymax=py3+.0005,fill=n)) + scale_y_continuous(breaks=seq(-10.85,-10.725,by=.001))

huge %>% filter( in11thAve & py3 == -10.84 ) %>% group_by(px4) %>% summarize( n = n() )
huge %>% filter( px4 == -83.7852 & py3 == -10.84)

maped <- get_map(c(-74.00562, 40.75076), source = c('google', 'osm', 'stamen', 'cloudmade')[1], zoom=16)
ggmap(maped) + geom_point(aes(pickup_longitude, pickup_latitude), data = huge %>% filter( in11thAve & py3 == -10.84 ))

nyc <- get_map(c(-73.98, 40.76076), source = c('google', 'osm', 'stamen', 'cloudmade')[1], zoom=14)
ggmap(nyc) + geom_point(aes(pickup_longitude,pickup_latitude), data=huge%>%filter(inTS)%>%sample_n(1000))
