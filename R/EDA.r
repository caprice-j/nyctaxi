tables <- as.vector(dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema='public'"))


for(tbl in tables){
  glimpse(dbGetQuery(con, paste("SELECT * FROM", tbl, " LIMIT 10")))
}


mydf <- query(" select * from goldman_sachs_dropoffs limit 5000") %>% dplyr::select(-pickup, -dropoff)

ggplot(mydf) + geom_point(aes(x=dropoff_latitude, y=dropoff_longitude))


murray <- data_frame(lat=c(40.715198, 40.715447), lon=c(-74.013120, -74.015003)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
  west <- data_frame(lat=c(40.715273, 40.713930), lon=c(-74.013697, -74.013848)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 vesey <- data_frame(lat=c(40.714006, 40.714436), lon=c(-74.014442, -74.015353)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 neave <- data_frame(lat=c(40.715806, 40.714845), lon=c(-74.015405, -74.016161)) %>% mutate(s=(first(lat)-last(lat)) / (first(lon)-last(lon)), i=lat - lon*s )
 
streets <- list(geom_abline(slope=murray$s[1], intercept=murray$i[1], color="pink",      lwd=3),
                geom_abline(slope=  west$s[1], intercept=  west$i[1], color="skyblue",   lwd=3),
                geom_abline(slope= vesey$s[1], intercept= vesey$i[1], color="darkgreen", lwd=3),
                geom_abline(slope= neave$s[1], intercept= neave$i[1], color="orange",    lwd=3))

png("EDA/GS-dropoff-cluster.png", width=960, height=960)
  mydf %>% mutate( t = ifelse(dropoff_latitude<40.7144, 'STREET', 'GOLDMAN') ) %>% ggplot() + 
  geom_point(aes(x=dropoff_longitude, y=dropoff_latitude, color=t), size=.5) + streets + 
  xlim(-74.0162, -74.0135) + ylim(40.7138, 40.7156) + 
  labs(title="Goldman Sachs Tower (West 200) Dropoff Location", subtitle="Two clusters: GS Entrance and Street Intersection")
dev.off()


mydf %>% mutate( t = ifelse(dropoff_latitude<40.7144, 'STREET', 'GOLDMAN') ) %>% ggplot() + geom_segment(aes(x=pickup_longitude, xend=dropoff_longitude,
                                                                                                             y=pickup_latitude,  yend=dropoff_latitude, color=t)) # + streets

maplist$nyc

ggplot() +geom_polygon(data = maplist$mt,
             aes(x = long, y = lat, group = group),
             fill = "#080808", color = "#080808") +   geom_point(data=mydf %>% mutate( t = ifelse(dropoff_latitude<40.7144, 'STREET', 'GOLDMAN') ) , aes(x=dropoff_latitude, y=dropoff_longitude, color=t), size=.5) 

qmap("200 West St, New York ")

 
