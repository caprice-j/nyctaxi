load(file = 'data/trips.20160607.all.RData')

heavy <-
  univ2 %>%
  filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
           40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
  mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
          hpay = tip_amount*60/as.numeric(min),
          isPremium = 12 * tip_amount - hpay < 0,
          #rate = tip_amount / total_amount,
          rate = tip_amount / (total_amount - tip_amount),
          is16 = ifelse(.166666 < hpay & hpay < .166667, TRUE, FALSE),
          is20 = ifelse(.199999 < hpay & hpay < .200001, TRUE, FALSE)
          ) %>%
  filter( payment_type == '1' & 2 < min & min < 120) %>%
  mutate( h = hour(pickup_datetime), h2 = as.factor(round(h/2,0)),
          pt = 3600*hour(pickup_datetime) + 60*minute(pickup_datetime) + second(pickup_datetime) ) %>%
  rename( tip = tip_amount )

png("EDA/20160607-constantTipRatio.png", width=960, height=960)
  tmp <- heavy %>% filter(rate < 1) %>% mutate( isInt = ifelse( round(rate*100,0) == rate*100, TRUE, FALSE ) )
  ggplot(tmp %>% filter(! isInt),aes(total_amount, rate) ) +
  geom_point(size=1.3, color="#888888") + geom_point(data=tmp %>% filter(isInt), color="red") + scale_y_continuous(breaks=seq(0,1,by=.05))
  rm(tmp)
dev.off()
