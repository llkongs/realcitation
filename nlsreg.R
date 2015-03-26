
# In this scripts, I will wirte a nonlinear least regression myself.

# First, I will define the cost function as

# z ~ 40 * (exp(a1 * (pnorm((log(x)-a2)/a3))-1))

# The prediction model is :

hx <- function(t1,t2,t3,m = 35,x){
        hx <- m * exp(t1 * (pnorm((log(x) - t2)/t3)) - 1)
        hx
}

# The regularized cost function is :

CostF <- function(t1,t2,t3,m = 35,x,y, lambda = 1){
        costreg <-  sum((m * exp(t1 * (pnorm((log(x) - t2)/t3)) - 1) 
                 - y)^2) #+ 0.5 * lambda * (t1^2 + t2^2 + t3^2))/length(x)
        costreg
}

CostF1 <- function(t1,t2,t3,m = 35,x,y, lambda = 1){
        costreg <-  0.5*(sum((m * exp(t1 * (pnorm((log(x) - t2)/t3)) - 1) 
                         - y)^2) + 0.5 * lambda * (t1^2 + t2^2 + t3^2))/length(x)
        costreg
}

# Then we should use gradient descent to minimize the cost function

gradf <- function(t1,t2,t3, m = 35, x, y,lambda = 1){
        # calculate the partial derivative first by using the deriv() on CostFunction
        len <- length(x)
        .expr2 <- log(x) - t2
        .expr3 <- .expr2/t3
        .expr4 <- pnorm(.expr3)
        .expr7 <- exp(t1 * .expr4 - 1)
        .expr9 <- m * .expr7 - y
        .expr12 <- 0.5 * lambda
        .expr16 <- t3^2
        .expr32 <- dnorm(.expr3)
        .value <- (0.5 * .expr9^2 + .expr12 * (t1^2 + t2^2 + .expr16))/len
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("t1", 
                                                              "t2", "t3")))
        .grad[, "t1"] <- (0.5 * (2 * (m * (.expr7 * .expr4) * .expr9)) + 
                        .expr12 * (2 * t1))/len
        .grad[, "t2"] <- (.expr12 * (2 * t2) - 0.5 * (2 * (m * (.expr7 * 
                        (t1 * (.expr32 * (1/t3)))) * .expr9)))/len
        .grad[, "t3"] <- (.expr12 * (2 * t3) - 0.5 * (2 * (m * (.expr7 * 
                        (t1 * (.expr32 * (.expr2/.expr16)))) * .expr9)))/len
        attr(.value, "gradient") <- .grad
        .value
        .grad
        #grad1 <- data.frame(sum(.grad[,1]),sum(.grad[,2]),sum(.grad[,3]))
        #colnames(grad1) <- c("t1",'t2','t3')
        #grad1
}
#4.5,1.21,3.3

        t1 <- 4;t2 <- 1.51; t3 <- 2.3
        gradm <- array(dim = c(length(x),3,51))
        grads <- data.frame(1:51,1:51,1:51);colnames(grads) <- c('t1','t2','t3')
        grads[1,] <- gradf(t1,t2,t3,x=3,y =115 )
        for (i in 1:50){
                .grad <- gradf(t1 = grads[i,1],t2= grads[i,2],t3 = grads[i,3],
                               m = 35,x = 3,y =115, lambda = 0.2)
                grads[i+1,1] <- grads[i,1] - 0.93 * (.grad[1] + lambda*.grad[1])
                grads[i+1,2] <- grads[i,2] - 0.93* (.grad[2] + lambda*.grad[2])
                grads[i+1,3] <- grads[i,3] - 0.93* (.grad[3] + lambda*.grad[3])
        }
        grads
        
#         dimnames(gradm)[[3]] <- 0:50
#         colnames(gradm) <- c("t1","t2","t3")
#         gradm[, ,'0'] <- cbind(rep(t1,length(x)),rep(t2,length(x)),rep(t3,length(x)))
#         for (i in 1:51) {
#                 gradm[,1,i+1] <- gradm[,1,i] - (.grad[,'t1'] + lambda*grad[,'t1'])
#                 gradm[,2,i+1] <- gradm[,2,i] - (.grad[,'t2'] + lambda*grad[,'t2'])
#                 gradm[,3,i+1] <- gradm[,3,i] - (.grad[,'t3'] + lambda*grad[,'t3'])
#         }

gradf1 <- function(t1,t2,t3, m = 35, x, y){
     
        .expr2 <- log(x) - t2
        .expr3 <- .expr2/t3
        .expr4 <- pnorm(.expr3)
        .expr7 <- exp(t1 * .expr4 - 1)
        .expr9 <- m * .expr7 - y
        .expr15 <- dnorm(.expr3)
        .value <- .expr9^2
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("t1", 
                                         "t2", "t3")))
        .grad[, "t1"] <- 2 * (m * (.expr7 * .expr4) * .expr9)
        .grad[, "t2"] <- -(2 * (m * (.expr7 * (t1 * (.expr15 * (1/t3)))) * 
                                        .expr9))
        .grad[, "t3"] <- -(2 * (m * (.expr7 * (t1 * (.expr15 * (.expr2/t3^2)))) * 
                                        .expr9))
        .grad
}


nlss1 <- function(x1,y1,iter = 10000,t10 = 4, t20 =1, t30 = 2){
        grads <- rep(0,(iter+1)*3);dim(grads) <- c((iter+1),3)
        grads[1,] <- c(t10,t20,t30)
        l1 <- length(x1)
        grads1 <- rep(0,3*l1*(iter+1));dim(grads1) <- c(l1,3,iter+1)
        for(i in 1:iter){
                grads1[,,i ] <- gradf1(grads[i,1],grads[i,2],grads[i,3],x=x1,y=y1)
                
                grads[i+1,1] <- grads[i,1] - sum(grads1[1:l1,1,i])/(10*abs(sum(grads1[1:l1,1,1])))
                grads[i+1,2] <- grads[i,2] - sum(grads1[1:l1,2,i])/(10*abs(sum(grads1[1:l1,2,1])))
                grads[i+1,3] <- grads[i,3] - sum(grads1[1:l1,3,i])/(10*abs(sum(grads1[1:l1,3,1])))
                }
       cc <- list(data.frame(tail(grads)),
        "Cost" = CostF(grads[iter+1,1],grads[iter+1,2],grads[iter+1,3],x=x1,y=y1),
        "result compare"= data.frame(cbind("predict" = 
         35* (exp(grads[iter+1,1] * (pnorm((log(x1) - grads[iter+1,2])/grads[iter+1,3] )) - 1)),"real" = y1)))
       list(cc,plot(cc[[3]]$real,col = "blue",type = "l"),lines(cc[[3]]$predict,col = 'red',type = 'l'))
}


nlss2 <- function(x1,y1,iter = 10000,t10 =4, t20 = 1, t30 =2){
        grads <- rep(0,(iter+1)*3);dim(grads) <- c((iter+1),3)
        grads[1,] <- c(t10,t20,t30)
        l1 <- length(x1)
        grads1 <- rep(0,3*l1*(iter+1));dim(grads1) <- c(l1,3,iter+1)
        for(i in 1:iter){
                grads1[,,i ] <- gradf(grads[i,1],grads[i,2],grads[i,3],x=x1,y=y1)
                
                grads[i+1,1] <- grads[i,1] - (sum(grads1[1:l1,1,i])+1*grads[i,1])/(10*abs(sum(grads1[1:l1,1,1])))
                grads[i+1,2] <- grads[i,2] - (sum(grads1[1:l1,2,i])+1*grads[i,2])/(10*abs(sum(grads1[1:l1,2,1])))
                grads[i+1,3] <- grads[i,3] - (sum(grads1[1:l1,3,i])+1*grads[i,3])/(10*abs(sum(grads1[1:l1,3,1])))
        }
        cc <- list(data.frame(tail(grads)),
        "Cost"= 2*l1*CostF1(grads[iter+1,1],grads[iter+1,2],grads[iter+1,3],x=x1,y=y1),"result compare"= data.frame(cbind("predict" = 
                35* (exp(grads[iter+1,1] * (pnorm((log(x1) - grads[iter+1,2])/grads[iter+1,3] )) - 1)),"real" = y1)))
        list(cc,plot(cc[[3]]$real,col = "blue",type = "l"),lines(cc[[3]]$predict,col = 'red',type = 'l'))
}