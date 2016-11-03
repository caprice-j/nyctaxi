library(dplyr)
mydb <- src_postgres(dbname = 'nyctaxi', host = 'localhost', user = 'postgres', password = 'bgdt')
tbl_trips <- tbl(mydb, "trips") # NB: dplyr cannot manipulate postgis. use rgdal or sp

library(RPostgreSQL)
library(rgdal)
library(sp)
library(maptools)
library(gpclib)
library(ggmap)
gpclibPermit() # support for gpclib will be withdrawn from maptools at the next major release
# census truct (the target area of statistics): https://en.wikipedia.org/wiki/Census_tract
#
# +datum= is to be prefered to +ellps=,
# because the datum always fixes the ellipsoid, but the ellipsoid never fixes the datum.
# dsn : data source name (folder)
# layer: shape file name without extension
tracts = sp::spTransform(rgdal::readOGR(dsn="../nyct2010_15b", layer = "nyct2010"), sp::CRS("+proj=longlat +datum=WGS84"))
# the +towgs84 tag should be used where needed to make sure that datum transformation does take place
tracts@proj4string
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
tracts.points = ggplot2::fortify(tracts, region = "id") # FIXME: library(broom) is preferred
# if rror: isTRUE(gpclibPermitStatus()) is not TRUE, library(gpclib)
tracts.map = inner_join(tracts.points, tracts@data, by = "id")

nyc_map = tracts.map
ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")
manhattan_map = filter(tracts.map, BoroName == "Manhattan")


ggplot() +
  geom_polygon(data = manhattan_map,
               aes(x = long, y = lat, group = group),
               fill = "#080808", color = "#080808") 




gs_origin = query("
                  SELECT
                  dropoff_datetime,
                  date_trunc('day', dropoff_datetime) AS dropoff_day,
                  EXTRACT(EPOCH FROM dropoff_datetime - date_trunc('day', dropoff_datetime)) AS second_of_day,
                  pickup_datetime,
                  EXTRACT(EPOCH FROM pickup_datetime - date_trunc('day', pickup_datetime)) AS second_of_day_pc,
                  pickup_nyct2010_gid,
                  pickup_longitude,
                  pickup_latitude,
                  EXTRACT(HOUR FROM dropoff_datetime) AS hour,
                  EXTRACT(DOW FROM dropoff_datetime) AS dow
                  FROM goldman_sachs_dropoffs
                  ")

gs = gs_origin %>%
  mutate(timestamp_for_x_axis_dp = as.POSIXct(second_of_day,    origin = "1970-01-01", tz = "UTC"),
         timestamp_for_x_axis = as.POSIXct(second_of_day_pc, origin = "1970-01-01", tz = "UTC"))

library(lubridate)

ggplot(data = gs %>% filter((dow %in% 1:5) & hour(dropoff_datetime) %in% 2:3 ),
       aes(x = pickup_longitude, y = pickup_latitude)) +
geom_polygon(data = manhattan_map, aes(x = long, y = lat, group = group),
             fill = "#080808", color = "#080808") + geom_label(aes(label=dow, color=as.factor(dow)), size=4)



ggplot(data = gs %>% filter(dow %in% 1:5) %>%,
       aes(x = timestamp_for_x_axis)) +
  geom_histogram(binwidth = 1200) +
  stat_bin(binwidth=1200, geom="text", aes(label=..count..), vjust=-1.5, angle=5, size=3) +
  scale_x_datetime("\ndrop off time", date_breaks='1 hour', date_minor_breaks = "1 hour", labels=date_format("%H"))+#, labels = date_format("%l %p"), minor_breaks = "1 hour") +
  scale_y_continuous("taxi drop offs\n", labels = comma) +
  title_with_subtitle("GS Weekday Taxi Drop Offs at 200 West St", "Based on NYC TLC data from 7/2015–6/2016") +
  theme_tws(base_size = 19)
