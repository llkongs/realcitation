
# This script is used to test whether the costfunction is converged or not.

t1 <- seq(1,5.2,by = 0.01)
df1 <- vector()
for (i in t1){df1 <- c(df1,CostF(i,1.21,3.3,x=3,y=115))}
plot(t1,df1,type = "l") #minimum 4.5


t2 <- seq(1.13,1.145,by = 0.001)
df2 <- vector()
for(i in t2){df2 <- c(df2,CostF(4.535242,i,1.18,x=x,y=z))}
plot(t2,df2,type = "l") # minimum 1.21
abline(h = 0,col = "blue")

t3 <- seq(0.5,5,by = 0.01)
df3 <- vector()
for(i in t3){df3 <- c(df3,CostF(4.5,1.21,i,x=4,y=191))}
plot(t3,df3,type = "l") #minmum 3.3

# M1 <- data.frame(seq(3,5,by = 0.01),seq(0.5,2.5,by = 0.01))
# colnames(M1) <- c('t1','t2')
# M1$z <- 1:201
# for (i in 1:201){M1[i,3] <- CostF(M1[i,1],M1[i,2],3.3,x=3,y =115)}


t1 <- seq(3,5,by = 0.01);t2 <- seq(0.5,2.5,by = 0.01)
z <- matrix(rep(1:201,201))
dim(z) <- c(201,201)

for(i in 1:201){
        for (j in 1:201){
           z[i,j] <- CostF(t1[i],t2[j],3.3,x = 3, y= 115)     
        }
}

library(rgl)
library(evd)
library(MASS)
persp3d(t1,t2,z,col = "yellow",xlim = c(3,5),ylim = c(0,3))

t3 <- seq(2,4,by = 0.01)

z13 <- matrix(rep(1:201,201))
dim(z13) <- c(201,201)

for(i in 1:201){
        for (j in 1:201){
                z13[i,j] <- CostF(t1[i],1.21,t3[j],x = 3, y= 115)     
        }
}
persp3d(t1,t3,z13,col = "yellow")


z23 <- z13
for(i in 1:201){
        for (j in 1:201){
                z23[i,j] <- CostF(4.5,t2[i],t3[j],x = 3, y= 115)     
        }
}

persp3d(t2,t3,z,col = "yellow")


p12 <- persp3d(t1,t2,z,col = 'yellow')
p13 <- persp3d(t1,t3,z13,col = 'yellow')
p23 <- persp3d(t2,t3,z23,col = 'yellow')


t1 <- seq(2,8,by = 0.2)
t3 <-seq(2,8,by = 0.2)
z123 <- matrix(rep(1:31,31))
dim(z123) <- c(31,31)

for(i in 1:31){
        for (j in 1:31){
                z123[i,j] <- CostF(t1[i],0.8*t1[i] -3,t3[j],x = 3, y= 115)     
        }
}
persp3d(t1,t3,z123,col = "yellow")


t1 <- seq(4.2,4.8,by = 0.01);t3 <- seq(2,6,by = 0.01)
z123 <- matrix(rep(1:length(t1),length(t3)))
dim(z123) <- c(length(t1),length(t3))

for(i in 1:61){
        for (j in 1:length(t3)){
                z123[i,j] <- CostF(t1[i],t1[i]-3,t3[j],x=3,y=115)
        }
}

persp3d(t1,t3,z123,col = 'yellow')
