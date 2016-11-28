

inMidtown <- function(px, py) {
    return(
        ifelse(-4000<px & px < -1000 & 26000 < py & py < 31000, 1, 0)
    )
}




univ1month <- univ1month %>% mutate( inMid = inMidtown(px,py) )
master <- univ1month  %>% filter( -7000 < px & px < 3000 & 26000 < py & py < 31000) %>% sample_n(100000)
dim(inMid)







mini <- univ1month %>% sample_n(10000) %>% mutate( inMidtown = ifelse(inMid==1,"in","out") ) 
small <- univ1month %>% filter( inMid  == 1 ) %>% sample_n(50000)
ggplot(mini %>% filter( trip_distance < 25) ) + geom_histogram(aes(trip_distance), binwidth=.3) # 3 modals
ggplot(mini %>% filter( hpay < 50) ) + geom_histogram(aes(hpay)) # two modals

plot(mini$px,mini$py)
ggplot(master, aes(x = px, y = py)) + geom_point(aes(color=inMid))

# Hmmm
# scaled <- master %>% mutate( pxs = ((px) - mean(px)) / sd(px),
#                              pys = ((py) - mean(py)) / sd(py))
# ggplot(scaled, aes(pxs, pys)) + geom_point()
# ggplot(scaled, aes(pxs, pys, color=isHigh)) + geom_point()



# gaussian mixture model

mixed <- master %>% filter( -8000 < px & px < -3000 & 26500 < py & py < 27000)

ggplot(mixed) + geom_histogram(aes(px), binwidth=80)

library(stats)
library(mixtools) # https://www.jstatsoft.org/article/view/v032i06
emed <- normalmixEM(mixed$px, lambda = .5, mu =c (-7000,-6000,-5000,-4000,-3000), sigma = 5)
plot(emed, density = TRUE, cex.axis = 1.4, cex.lab = 1.5, cex.main = 1.5, breaks=100,
     main2 = "Membership Density of Avenues", xlab2 = "Rotated x-axis")

emed[c("lambda", "mu", "sigma")]

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

gg.mixEM(emed)
