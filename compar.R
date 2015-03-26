# nls(z ~ 40 * (exp(a1 * (pnorm((log(x)-a2)/a3))-1))

(model2 <- deriv(~ 35* (exp(t1* (pnorm((log(x) - t2)/t3)) - 1)),
                 c("t1","t2",'t3'), function(t1,t2,t3,x){}))

fit <- (nls(z ~ model2(t1,t2,t3,x),start=list(t1=4,t2=1,t3 = 2)))


35* (exp(4.537565* (pnorm((log(x) - 1.13896)/1.180853)) - 1))


compar <- function(x,z){
        
}