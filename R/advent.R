library(dplyr)
mydb <- src_postgres(dbname = 'nyctaxi', host = 'localhost', user = 'postgres', password = 'bgdt')
tbl_trips <- tbl(mydb, "trips") # NB: dplyr cannot manipulate postgis. use rgdal or sp


library(rgdal)
library(sp)
# 
# dbname = "yourdatabase"
# host = "yourhost"
# user = "AUser"
# pass = "ThisUsersPassword"
# name = "ASpatialTable" # Postgis table
# 
# dsn = "PG:dbname='nyctaxi' host='localhost' user='postgres' password='bgdt'"
# res = readOGR(dsn,'geometry')
# 
# plot(res)