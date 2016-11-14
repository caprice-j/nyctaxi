# univ <-
#   #query(" select * from trips order by random() limit 1000") %>% # FIXME: SO SLOW
#   query(" select * from trips where id % 5000 = 1") %>%
#   dplyr::select(-pickup, -dropoff) %>%
#   mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )

# save(univ, file="trips.1in5000.random.RData")

master <- univ %>% filter( as.POSIXct("2016-06-01") < pickup_datetime & pickup_datetime < as.POSIXct("2016-06-07") ) %>% mutate( pt = pickup_datetime, dt = dropoff_datetime) 

master %>% mutate( pt = 3600*hour(pt) + 60*minute(pt) + second(pt), len = dropoff_datetime - pickup_datetime) %>% head(2000) %>% arrange(pt) %>% mutate(r = row_number()) %>% ggplot() + geom_segment(aes(x=pt, xend=pt+len, y=r, yend=r)) 

#univ %>% summarize( min = min(pickup_datetime), max= max(pickup_datetime) ) 

# Weighted Interval Scheduling http://www.geeksforgeeks.org/weighted-job-scheduling-log-n-time/

sort((master %>% mutate( pt = 3600*hour(pt) + 60*minute(pt) + second(pt), len = dropoff_datetime - pickup_datetime) )$len,decreasing = TRUE)[1:5]


# Incorrect data: 

# 
# univ2 <-
#  query(" select * from trips WHERE CAST(pickup_datetime AS DATE) = '2016-06-07'") %>%  # NB: SUPER SLOW. takes > 2h 
#  dplyr::select(-pickup, -dropoff) %>%
#  mutate( inMt  = inManhattan(pickup_longitude, pickup_latitude) )
# save(univ2, file="trips.20160607.all.RData")

load(file = 'data/trips.20160607.all.RData')

master <- univ2 %>% filter( payment_type == '1' ) %>% sample_n( size = 30000 ) %>% mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)) ) %>% rename( m = tip_amount )

summed <- master %>% group_by(h) %>% summarize( mean = mean(m), sd = sd(m), median = median(m), max = max(m), min = min(m), n = n() )

png("EDA/TRIP-tip-time-series.png", width=960, height=960)
  summed %>% ggplot(aes(x=h)) + geom_text(aes(y=mean, label=n)) + geom_point(aes(y=median), color='red') + geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd))
dev.off()
# 4 - 5 a.m. is sparse. low predict accuracy might result


png("EDA/TRIP-tip-density.png", width=960, height=960)
  ggplot(master) + geom_density(aes(x=log2(m), color=h2), alpha=.4, lwd=1)
dev.off()
                                      
table(master$tip_amount, master$payment_type)
