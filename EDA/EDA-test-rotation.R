load(file = 'data/trips.20160613_19.all.RData')
library(tidyverse)
library(lubridate)
set.seed(1236)
theta <- -.632 # 
mini <- univWeek %>% filter( inMt & -74.1 < pickup_longitude & pickup_longitude < -73.925  ) %>%
    mutate( py = cos(theta) * pickup_latitude  - sin(theta) * pickup_longitude,
            px = sin(theta) * pickup_latitude  + cos(theta) * pickup_longitude,
            dy = cos(theta) * dropoff_latitude - sin(theta) * dropoff_longitude,
            dx = sin(theta) * dropoff_latitude + cos(theta) * dropoff_longitude,
             h = hour(pickup_datetime) ) %>% 
    filter( -83.8 < px & px < -83.745 & -10.9 < py & py < -10.725 ) %>%
    filter( payment_type == 1 ) %>%
    sample_n(100000)

if (FALSE) {
    rotate_ny <- function(mini, theta = -.632) {
        mini$y <- cos(theta) * mini$pickup_latitude - sin(theta) * mini$pickup_longitude
        mini$x <- sin(theta) * mini$pickup_latitude + cos(theta) * mini$pickup_longitude
        plot(mini$x, mini$y, pch=18)
    }
    rotate_ny(mini, 0)
    rotate_ny(mini, -.632) ; abline(a=-83.90, b=0,col="red")
    abline(v=-83.78,col="red")
    
    landmark <- data_frame( name = c('timessq'),
                            lat = c( 40.758855),
                            lon = c(-73.985134) ) %>% 
        mutate( y = cos(theta) * lat - sin(theta) * lon,
                x = sin(theta) * lat + cos(theta) * lon )
    
    png("EDA/univWeek-GPS-accuracy.png", width=480, height=960)
        plot(mini$px, mini$py, pch=18, xlab="", ylab="", cex=.6, 
             main="Weekly Pickup Location in Manhattan \n(n=100,000)", axes=FALSE)
        text(landmark$x, landmark$y, label="TSQ", col="red", cex=1.5)
    dev.off()
}
  
# Useful: https://www.google.com/maps/@40.7635299,-73.9162908,724a,20y,270h,83.12t/data=!3m1!1e3?hl=en

tiny <- mini %>% filter( -83.8 < px & px < -83.745 & -10.9 < py & py < -10.725 ) %>%
    mutate( px3 = round(px,3), py3 = round(py,3) )

library(rpart)
library(rpart.plot)
library(plotmo)

s <- 50
model_raw <-
    rpart( tip_amount ~ ., data = tiny %>% select(tip_amount, pickup_longitude, pickup_latitude),
           control = rpart.control(minsplit=s,minbucket=15,cp=0,maxsurrogate=0,maxcompete=0))
model_xy <-
    rpart( tip_amount ~ ., data = tiny %>% select(tip_amount, px, py),
           control = rpart.control(minsplit=s,minbucket=15,cp=0,maxsurrogate=0,maxcompete=0))
model_xydist <-
    rpart( tip_amount ~ ., data = tiny %>% select(tip_amount, px, py, trip_distance),
           control = rpart.control(minsplit=s,minbucket=15,cp=0,maxsurrogate=0,maxcompete=0))
model_xyh <-
    rpart( tip_amount ~ ., data = tiny %>% select(tip_amount, px, py, h),
           control = rpart.control(minsplit=s,minbucket=15,cp=0,maxsurrogate=0,maxcompete=0))

if (FALSE) {
    xrange <- sort(unique(tiny$px3))
    yrange <- sort(unique(tiny$py3))
    xd <- (xrange[2]-xrange[1])*.5 # x dodge
    yd <- (yrange[2]-yrange[1])*.5
    testdata <- expand.grid( px = xrange, py = yrange)
    out <- bind_cols( testdata, data_frame(pred = predict(model,testdata)))
    ggplot(out) + geom_rect(aes(xmin=px-xd,ymin=py-yd,
                                xmax=px+xd,ymax=py+yd,fill=pred)) +
        geom_point(aes(x=px3,y=py3), data=tiny%>%sample_n(1000), alpha=.4, color="white")
}

tiny$pred_raw    <- predict(model_raw   , tiny %>% select(pickup_latitude, pickup_longitude))
tiny$pred_xy     <- predict(model_xy    , tiny %>% select(px, py))
tiny$pred_xydist <- predict(model_xydist, tiny %>% select(px, py, trip_distance))
tiny$pred_xyh    <- predict(model_xyh   , tiny %>% select(px, py, h))
rmse <- function(pred, act) round(mean((pred - act)^2),3)
par(mfrow=c(2,1))
plot(tiny$pred_raw, tiny$tip_amount);     rmse(tiny$pred_raw, tiny$tip_amount)
plot(tiny$pred_xy, tiny$tip_amount);      rmse(tiny$pred_xy, tiny$tip_amount)
plot(tiny$pred_xyh, tiny$tip_amount);     rmse(tiny$pred_xyh, tiny$tip_amount)
plot(tiny$pred_xydist, tiny$tip_amount);  rmse(tiny$pred_xydist, tiny$tip_amount) # of course, smallest

plot(tiny$pred_raw, tiny$pred_xy)
decent <- which(tiny$pred_xy != tiny$tip_amount)

rmse(tiny$pred_xy[decent],  tiny$tip_amount[decent])
rmse(tiny$pred_raw[decent], tiny$tip_amount[decent])


#roundXlim <- c(-83.79,-83.745)
#roundYlim <- c(-10.889, -10.73)
png("EDA/univWeek-raw-xy.png", width=640, height=1280)
    plot(tiny$px , tiny$py , pch=18, xlab="", ylab="")

dev.off()

png("EDA/univWeek-rounded-xy.png", width=640, height=1280)
    plot(tiny$px3, tiny$py3, pch=18, xlab="", ylab="")
dev.off()

paste0("Decision Tree rpart( tip ~ x + y )\nRMSE = ",rmse(tiny$pred_raw, tiny$tip_amount))
paste0("Decision Tree rpart( tip ~ x + y )\nRMSE = ",rmse(tiny$pred_xy, tiny$tip_amount)) # 6% reduce with the same model

#model_xy$frame %>% filter(var == "<leaf>" & n == 1)
#model_raw$frame %>% filter(var == "<leaf>" & n == 1)



#library(plotly)
#plot_ly(tiny, x = ~ x, y = ~ y )
