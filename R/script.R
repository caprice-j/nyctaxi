source('~/nyctaxi/R/advent.R')

init()
maplist <- generateNewYorkMap()
# str(maplist, max.level=1)

mydb <- src_postgres(dbname = 'nyctaxi', host = 'localhost', user = 'postgres', password = 'bgdt')
