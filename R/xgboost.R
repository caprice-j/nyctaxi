source('R/set_univWeek.R', echo=TRUE)
mini <- huge[1:100000, ]
test <- huge[  100001:200000, ]
tiny <- huge[101:2000, ] %>% mutate( isCons = as.numeric(isCons))

library(xgboost)
library(caret)
library(dummies)
featurev <- c('cab_type_id', 'vendor_id', 'store_and_fwd_flag', 'rate_code_id', 'pickup_longitude', 'pickup_latitude', 'h', 'wday')
featurev <- c('cab_type_id', 'vendor_id', 'store_and_fwd_flag', 'rate_code_id', 'px3', 'py3', 'h', 'wday')

#as.data.frame(dummyVars( ~., data = mini[, featurev]))
dummied <- dummy.data.frame(tiny[, featurev], sep=".", dummy.classes = c("factor","ordered", "character"))

dtr <- xgb.DMatrix(as(as.matrix(dummied), 'sparseMatrix'), label=as(tiny$isCons,'sparseMatrix'))
set.seed(12345)
xgb.train(params = list(eta=.3, max_depth=3, objective='binary:logistic'), data = dtr, nrounds = 100, metrics=list("error"))
xgb.cv(params = list(eta=.3, max_depth=4, objective='binary:logistic'), data = dtr, nrounds = 100, nfold = 2, metrics=list("error"), print_every_n = 10)

