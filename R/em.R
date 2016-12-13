

inMidtown <- function(px, py) {
    return(
        ifelse(-4000<px & px < -1000 & 26000 < py & py < 31000, 1, 0)
    )
}

dim(inMid)


univ1month <- univ1month %>% dplyr::select(-id, -pickup_nyct2010_gid, -dropoff_nyct2010_gid) %>% mutate( inMid = inMidtown(px,py) )
master <- univ1month  %>% filter( -7000 < px & px < 3000 & 26000 < py & py < 31000) #%>% sample_n(300000)

# %>% : pipe function
# 






# mini <- univ1month %>% sample_n(10000) %>% mutate( inMidtown = ifelse(inMid==1,"in","out") ) 
# small <- univ1month %>% filter( inMid  == 1 ) %>% sample_n(50000)
# ggplot(mini %>% filter( trip_distance < 25) ) + geom_histogram(aes(trip_distance), binwidth=.3) # 3 modals
# ggplot(mini %>% filter( hpay < 50) ) + geom_histogram(aes(hpay)) # two modals
# 
# plot(mini$px,mini$py)
# ggplot(master, aes(x = px, y = py)) + geom_point(aes(color=inMid))

# Hmmm
# scaled <- master %>% mutate( pxs = ((px) - mean(px)) / sd(px),
#                              pys = ((py) - mean(py)) / sd(py))
# ggplot(scaled, aes(pxs, pys)) + geom_point()
# ggplot(scaled, aes(pxs, pys, color=isHigh)) + geom_point()

pdf('EDA/streets-histograms.pdf', width = 10,height=10)
for (sample_y in seq(26000, 31000, by=50) ) {
    p <-
    ggplot(master %>% filter(px > -4500 & px < 1250 &
                             py > sample_y-25 &
                             py < sample_y+25 )) + geom_histogram(aes(px), binwidth=80) +
        labs(title = paste0("y = ", sample_y))
    print(p)
    message(sample_y)
}
dev.off()

# gaussian mixture model
ggplot(master) + geom_point(aes(px, py), size=.5) + scale_y_continuous(breaks=seq(26000,31000,by=100)) + scale_x_continuous(breaks=seq(-8000,3000,by=500))

ggplot(master %>% filter(px < -1600) ) + geom_point(aes(px, py), size=.5) + scale_y_continuous(breaks=seq(26000,31000,by=100)) + scale_x_continuous(breaks=seq(-8000,3000,by=500))

ggplot(master %>% filter( 29750 < py & py < 30800 & -2500 < px & px < 0 ) ) + geom_point(aes(px, py))

mixed <- master %>% filter( -8000 < px & px < -3000 & 26500 < py & py < 27000)
mixed <- master %>% filter( 29750 < py & py < 30800 & -2500 < px & px < 0 )
mixed <- master %>% filter( -2250 < px & px < -1600 & py < 30800 )

ggplot(mixed) + geom_point(aes(px, py), size=.5)
ggplot(mixed) + geom_histogram(aes(px), binwidth=80)
ggplot(mixed) + geom_histogram(aes(py), binwidth=10)
# http://docs.ggplot2.org/0.9.3.1/geom_histogram.html

library(stats)
library(mixtools) # https://www.jstatsoft.org/article/view/v032i06
first_mu<-seq(26400,30600,by=268)
emed <- normalmixEM(mixed$px, lambda = .2, mu = c(-7000,-6000,-5000,-4000,-3000), sigma = 5)
emed <- normalmixEM(mixed$px, lambda = .2, mu = c(-2500,-1400,-800,-400), sigma = c(5,3,2,2) )
emed <- normalmixEM(mixed$py, lambda = 1/length(first_mu), mu = first_mu, sigma = 5, verb = TRUE, epsilon = 5e-2)
plot(emed, density = TRUE, cex.axis = 1.4, cex.lab = 1.5, cex.main = 1.5, breaks=100,
     main2 = "Membership Density of Avenues", xlab2 = "Rotated x-axis")

emed[c("lambda", "mu", "sigma")]

plot(mixed$px, mixed$py)
emed$mu

png("EDA/GAUSSIAN-means.png", width = 960, height=960)
with(master %>% filter(px < -1600) %>% sample_n(100000), plot(px,py, pch=18, cex=.5,
                                                              main = "Estimated Gaussian Means (after 986 iterations)"))
abline(h=emed$mu,col="red",lwd=2)

abline(h=first_mu, lwd=2, col="blue")

# credit: http://stackoverflow.com/questions/25313578/any-suggestions-for-how-i-can-plot-mixem-type-data-using-ggplot2
gg.mixEM <- function(EM, breaks=50, npoly = 500) {
    b <- breaks
    require(ggplot2)
    x       <- with(EM,seq(min(x),max(x),len=npoly))
    x_inter <- x[2] - x[1]
    pars    <- with(EM,data.frame(comp=colnames(posterior), mu, sigma,lambda))
    em.df   <- data.frame(x=rep(x,each=nrow(pars)),pars)
    em.df$y <- with(em.df,lambda*dnorm(x,mean=mu,sd=sigma))

    em.df <- em.df %>% mutate( y = ifelse((x==min(x)|x==max(x)) & y > 10^-06,0,y))
    
    # tooHighMin <- em.df %>% filter( x == min(x) & y > 10^-06)
    # tooHighMax <- em.df %>% filter( x == max(x) & y > 10^-06)
    # 
    # em.df <- bind_rows(em.df,
    #                    tooHighMin %>% mutate( y = 0, x = x - .1 * sd(x) ),
    #                    tooHighMax %>% mutate( y = 0, x = x + .1 * sd(x) )
    #                    )
    
    ggplot(data.frame(x=EM$x),aes(x,y=..density..)) + 
        geom_histogram(fill=NA,color="black", bins=b)+
        geom_polygon(data=em.df%>%arrange(x),aes(x,y,fill=comp),color="grey50", alpha=0.5)+
        scale_fill_discrete("Component\nMeans",labels=format(em.df$mu,digits=3))+
        theme_bw()
}

png("EDA/avenue-membership.png", width=480, height = 480)
    gg.mixEM(emed)
dev.off()


str(emed)

n_points <- 1000
x       <- seq(min(emed$x),max(emed$x),len=n_points)

tmp    <- data.frame(comp = colnames(emed$posterior),
                      mu = emed$mu,
                      sigma = emed$sigma,
                      lambda = emed$lambda)
em.df   <- data.frame(x=rep(x,each=nrow(tmp)),tmp)
em.df$y <- em.df$lambda * dnorm(em.df$x, mean=em.df$mu,sd=em.df$sigma)

ggplot(em.df ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")

ggplot(em.df %>% filter( -5000 < x & x < -4500  ) ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")

ggplot(em.df %>% filter( -5000 < x & x < -4500  ) ) + geom_bar(aes(x=x, y=y, fill=comp), position="fill", stat="identity")


decision_boundary <- em.df %>% group_by(x) %>% mutate( ratio =  y / sum(y) ) %>% arrange(desc(y)) %>% slice(1:2) %>% group_by(x) %>%  filter( min(ratio) > .49) %>% slice(1) %>% group_by() %>% .$x

plot(mixed$px, mixed$py)
abline(v=decision_boundary)

# multivariate normal

data("Waterdata")
cutpts <- 10.5*(-6:6)
watermult <- makemultdata(Waterdata, cuts = cutpts)

mult_x <- mixed %>% dplyr::select(px,py) %>% as.matrix

mved <- mvnormalmixEM(mult_x, mu = c(-2500,-1400,-800,-400), k = 4, arbmean = TRUE, arbvar = TRUE, verb = TRUE, epsilon = 7e-1)

mved$mu
nnn <- 1000
tmp <- 
bind_rows(
    cbind(rmvnorm(nnn, mved$mu[[1]], mved$sigma[[1]] ), comp = 'c1') %>% data.frame,
    cbind(rmvnorm(nnn, mved$mu[[2]], mved$sigma[[2]] ), comp = 'c2') %>% data.frame
) %>% mutate_each(funs(as.numeric), V1, V2)
ggplot(tmp) + geom_point(aes(x=V1,y=V2,color=comp))


bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)

# http://mi.eng.cam.ac.uk/~mjfg/local/4F10/lect2.pdf

#demo(persp)

bivn


pmat = persp(x=1:5,y=1:3,z=matrix(3:17,ncol=3), xlab="X", theta=-60, ylab="Y", zlab="Z", ticktype="detailed", zlim = c(-1, 15) )
lines(trans3d(c(1,5), y=2:3, z= -1, pm = pmat), col = 3, lwd=4)
