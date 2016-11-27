
master <- univUWS %>% sample_n(50000) %>% mutate( over20 = as.numeric(rate >= .20) )
mm  <- model.matrix( ~ tz:wd + pickup_longitude + pickup_latitude + px + py +
                         passenger_count + rid + vendor_id + cab_type_id + store_and_fwd_flag + isHigh + 0 ,master)

rg <- list()
objVar <- 'isHigh'
colmm <- colnames(mm)
config <-
list(
    when_latlon_who = colmm[! grepl('p[xy]', colmm)],
    when_where_who  = colmm[! grepl('picku', colmm)],
    when_where      = colmm[colmm != 'passenger_count'],
    where           = c('px', 'py')
)
config <- sapply(config, function(x) unique(c(x,objVar)) )

for( i in seq_along(config) ) {
    message(i, appendLF = FALSE)
    rg[[i]] <- ranger(data= ok[, config[[i]]], dependent.variable.name = objVar,
                      num.trees=100, classification = TRUE, importance = "permutation")
    message(" Error: ", rg[[i]]$prediction.error)
}

#sapply(rg, function(x) x$variable.importance)
par(las=2, mar=c(4,12,1,1), mfrow=c(2,2))
#sapply(rg, function(x) barplot(x$variable.importance, horiz=TRUE))

# 1 Error: 0.41808
# 2 Error: 0.4159
# 3 Error: 0.41678
# 4 Error: 0.48132

afterm <- model.matrix( ~ tz:wd + px + py +
                          passenger_count +
                          rid + vendor_id + cab_type_id + store_and_fwd_flag + 
                          trip_distance + fare_amount + min + isHigh + 0 ,master)

rgafter <- ranger(data= afterm, dependent.variable.name = objVar,
                  num.trees = 100, classification = TRUE, importance = "permutation")
rgafter$prediction.error # OOB error rate: 20.1 % 

cheatm <- model.matrix( ~ tz:wd + px + py +
                            passenger_count +
                            rid + vendor_id + cab_type_id + store_and_fwd_flag + 
                            trip_distance + fare_amount + min + hpay + isHigh + 0 ,master)

rgcheat <- ranger(data= cheatm, dependent.variable.name = objVar,
                  num.trees = 50, classification = TRUE, importance = "permutation")
rgcheat$prediction.error # OOB error rate: 0 % (because hpay >= .12 completely explains isHigh)

sum(master$isHigh) / length(master$isHigh) # random guess error: 49.7 %

png("EDA/random-forests-varImp.png", height=960, width=1200)
    par(las=2, mar=c(4,12,3,1), mfrow=c(2,2))
    barplot(rg[[4]]$variable.importance, horiz=TRUE, main ="RndFst( hpayOver12Doller ~ features ) (OOB err = .48) (before, xy-only)\n (Note: Random Guess Error Rate: 49.7%)")
    barplot(rg[[2]]$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .41) (before, xy&wday&h)")
    barplot(rgafter$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .20) (After)")
    barplot(rgcheat$variable.importance, horiz=TRUE, main ="RndFst (OOB err = .00) (Cheat)")
dev.off()

tipm <- model.matrix( ~ tz:wd + pickup_longitude + pickup_latitude + px + py + passenger_count +
                        rid + vendor_id + cab_type_id + store_and_fwd_flag + 
                        trip_distance + fare_amount + over20 + min + 0 ,master)

rgtip <- ranger(data= tipm, dependent.variable.name = "over20",
                  num.trees = 60, classification = TRUE, importance = "permutation")
# OOB preiction error 29.12 %
barplot(rgtip$variable.importance, horiz=TRUE)
