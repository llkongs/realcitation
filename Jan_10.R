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


#calculate the real citation value of the selected year

cry <- function(data,year,theta = 1,m = 35){
        data <- data[!is.na(data[,"py"]),] #delete the data that have no pulication years
        year <- as.numeric(year)
        if(year < min(as.numeric(data[,"py"]),na.rm = TRUE)) {
                stop("Invalid year:too small!")
                
        }
        ydata <- subset(data, py <= year) # find the necessary articles
        n1 <- dim(ydata)[1] #find number of articles
        n2 <- dim(ydata)[2] # number of columns
        h <- vector() #storage the citation of each paper
        for (i in 1:n1){
                puby <- as.numeric(ydata[i,"py"])
                h[i] <- sum(ydata[i,(122-(2015-puby)):(122-(2015-year))],na.rm = TRUE)    # count number of citaitons till this year            
        }
        
        cdata <- NULL
        cdata$rc <- round(sum(log((3*h/m)^theta+1)),3)
        cdata$ct <- n1 #ordinary citation
        as.data.frame(cdata)
}
# m represents the average citations in a research area

#But often review has much more citations than ordinary #research articles

twocit <- function(data,theta = 1, m = 35){
        data <- data[!is.na(data[,"py"]),] #delete the data that have no pulication years
        miny <- min(as.numeric(data[,"py"]),na.rm = TRUE)
        xa <- NULL
        for (year in miny:2014){
                xa <- rbind(xa,cry(data,year,theta,m))          
        }
        
        xa$year <- 1:(2014-min(as.numeric(data[,"py"]))+1)
        xa
}

x <- 1:14
y <- c(8,50,57,76,56,84,63,64,56,63,40,62,40,44)
z <- vector()
for (i in 1:14) {
        z[i] <- sum(y[1:i])
}
fit <- nls(z ~ 40 * (exp(a1 * (pnorm((log(x)-a2)/a3))-1)),start = list(a1=3,a2=3,a3=2))

mcmodel <- function(x,y,m = 35,data,lambda = 2,mu = 2,sigma = 2){
        fit <- nls(y ~ m * (exp(lambda * (pnorm((log(x)-mu)/sigma))-1)),start = list(lambda=lambda,mu=mu,sigma=sigma))
        fit
}



#the derivative of the citation dynamyics model

deriv(~ 35*(exp(lambda * (pnorm((log(year) - mu)/sigma)) - 1)),
      c('lambda','mu','sigma'),#function(lambda,mu,sigma,year){}
      )


#the fit model with regularization



