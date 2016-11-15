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

master <- univ2 %>% mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins') ) %>% filter( payment_type == '1' & 2 < min & min < 120) %>% sample_n( size = 30000 ) %>% mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)), pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>% rename( tip = tip_amount )

summed <- master %>% group_by(h) %>% summarize( mean = mean(tip), sd = sd(tip), median = median(tip), max = max(tip), min = min(tip), n = n() )

png("EDA/TRIP-tip-time-series.png", width=960, height=960)
  summed %>% ggplot(aes(x=h)) + geom_text(aes(y=mean, label=n)) + geom_point(aes(y=median), color='red') + geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd))
dev.off()
# 4 - 5 a.m. is sparse. low predict accuracy might result


png("EDA/TRIP-tip-density.png", width=960, height=960)
  ggplot(master) + geom_density(aes(x=log2(tip), color=h2), alpha=.4, lwd=1)
dev.off()
                                      
ggplot(master) + geom_point(aes(x=tip, y=len))

source('R/pairs-extension.R')

paired <- master %>% select( -starts_with('pickup'), -starts_with('dropoff'), -h, -payment_type, -trip_type, -ehail_fee ) %>% mutate( vendor_id = as.numeric(vendor_id), store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)), min = as.numeric(min), h2 = as.numeric(h2)) %>% mutate( hpay = tip*60/min ) %>% filter( !is.na(hpay) )  %>% mutate_each(funs(jitter), cab_type_id, vendor_id, store_and_fwd_flag, passenger_count, h2 )

png("EDA/TRIP-scatterplot-matrix.png", width=2560, height=1900)
  pairs(paired, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
dev.off()

library(plotly)

ggplot(paired %>% arrange(hpay) %>% mutate( r = row_number() ) ) + geom_point(aes(x=r, y=hpay), size=1); ggplotly()

ggplot(paired %>% arrange(hpay) %>% mutate( r = row_number() ) ) + geom_point(aes(x=r, y=log2(hpay)), size=1)


table(master$tip_amount, master$payment_type)

mean(paired$tip)
2.788412 * 8 # = $ 22.3


preped <- master %>% select( -starts_with('pickup'), -starts_with('dropoff'), -h, -payment_type, -trip_type, -ehail_fee ) %>% mutate( vendor_id = as.numeric(vendor_id), store_and_fwd_flag = as.numeric(as.factor(store_and_fwd_flag)), min = as.numeric(min), h2 = as.numeric(h2)) %>% mutate( hpay = tip*60/min ) %>% filter( !is.na(hpay) )

light <- preped %>% sample_n( size= 20000 )

png("EDA/TRIP-intervals.png", width=960, height=960)
  ggplot(light %>% arrange(pt) ) + geom_segment(aes(y=log2(hpay), yend=log2(hpay), x = pt/60, xend = pt/60 + min, color=as.factor(vendor_id) ), alpha=.6)
dev.off()

png("EDA/TRIP-intervals-cut1.png", width=2560, height=1900)
  ggplot(light %>% filter( hpay >= 1 ) )  + facet_grid( cut_number(min, n=5) ~ .) + geom_hline( aes(yintercept=3), color='red', lwd=.5, linetype="dotted") + geom_hline( aes(yintercept=4), color='green', lwd=.5, linetype="dotted")  + geom_hline( aes(yintercept=2), color='orange', lwd=.5, linetype="dotted") + geom_segment(aes(y=log2(hpay), yend=log2(hpay), x = pt/60, xend = pt/60 + min, color=tip ), alpha=.6) + scale_x_continuous(breaks=seq(0,1560,by=30)) + theme_bw()
dev.off()


plot(light$min, log2(light$hpay), pch=18)
