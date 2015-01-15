## 
## Suppose that we have 

x <- c(1,4,10,7,35,50,0,21,19,25,28,21,17,1,16,50,26,55,19,12,14,15,68,25,8,16,
       53,33,202,315,11,147,7)

cr1 <- function(x,theta){
        #in this function we use cr = (sum(ci^theta))^(1/(theta+1)) to calculate
        #cr
        y <- 0
        y <- (sum(x^theta))^(1/(theta+1))
        y
}

cr2 <- function(x,theta){
        y <- 0
        y <- ((sum(x^(theta)))/length(x))^(1/(theta))
        y
}

y1<- function(theta){cr1(x,theta)}

library(manipulate)
myr1 <- function(theta){mapply(yr1,1:theta)}
manipulate(plot(myr1(theta)),theta = slider(1,100,initial = 10))

#mainipulate the two functions at the same time

plot_1 <- function(theta){
        plot(mapply(yr1,1:theta),col = "blue",pch = 1,ylim = c(0,320))
        lines(mapply(yr2,1:theta),type = "b",col = "red",pch = 4)
        legend("bottomright",c("first","second"),
               col = c("blue","red"),pch = c(1,4))
}

manipulate(plot_1(theta),theta = slider(1,100,initial = 10))


# We can try to use the "rChart" packages to draw a interactive picture

library(rCharts)

#We should first create a data frame
dataf <- data.frame(x = 1:123, yr1 = mapply(yr1,1:123),yr2 = mapply(yr2,1:123))
# Notice to use the data.frame function, not the as.data.frame funciton
m1 <- mPlot(x = "x", y=c("yr1","yr2"),type = "Line",data = dataf)
m1$set(pointSize = 0, lineWidth = 1)
m1











