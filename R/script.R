source('~/nyctaxi/R/advent.R')

init()
maplist <- generateNewYorkMap()
# str(maplist, max.level=1)

mydb <- src_postgres(dbname = 'nyctaxi', host = 'localhost', user = 'postgres', password = 'bgdt')


s20161117 <- function(){
  load(gcl$trips20160613_19allRData)
  heavy <-
    univWeek %>% sample_frac(.1) %>%
    filter(-74.05 < pickup_longitude & pickup_longitude < -73.6 &
             40.50 < pickup_latitude  & pickup_latitude  < 41) %>% # remove seemingly incorrect records
    mutate( min = difftime(dropoff_datetime,pickup_datetime,units='mins'),
            hpay = tip_amount*60/as.numeric(min),
            isPremium = 12 * tip_amount - hpay < 0,
            #rate = tip_amount / total_amount,
            rate = tip_amount / (total_amount - tip_amount),
            is20 = ifelse(.199999 < rate & rate < .200001, TRUE, FALSE),
            is25 = ifelse(.249999 < rate & rate < .250001, TRUE, FALSE),
            is30 = ifelse(.299999 < rate & rate < .300001, TRUE, FALSE)
    ) %>%
    filter( payment_type == '1' & 2 < min & min < 120) %>%
    mutate( h = hour(pickup_datetime) ) %>%
    rename( tip = tip_amount ) %>%
    mutate( isHigh = isHigh(rate), po = round(pickup_longitude,3), pa = round(pickup_latitude,3) ) %>%
    group_by(po, pa) %>%
    mutate( n = n(), wellPaid = sum(isHigh)/ n, wday = wday(pickup_datetime) ) %>%
 
    paired <-
    heavy %>% dplyr::select(hpay, h, po, pa, wellPaid, wday, rate) %>% sample_frac(.1)
  
  png("../nyctaxi/EDA/20160613_19-scatterplot-matrix.png", width=2560, height=1900)
    pairs(paired, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
  dev.off()
  
  ggplot(rfmaster) + geom_point(aes(x=h,y=wellPaid, color=hpay), position=position_jitter(width=.4))
  with( rfmaster %>% filter(hpay<60), plot(wellPaid, hpay) ) 
}