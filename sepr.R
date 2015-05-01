
# function(x, year){
#         a <- subset(x, pyear <= year)
#         a <- a[ , 1:(year - 2014 + 72)]
#         for (i in 1:length(a$tc))  a$tc[i] <- sum((a[i, ])[8:(year - 2014 + 72)], na.rm = TRUE)
#         return(a)
# }

sepr <- function(data, year) {
        a <- subset(data, pyear <= year)
        a <- a[,1:(year - 2014 + 122)]
        for (i in 1:length(a$tc)) a$tc[i] <- sum((a[i,])[8:(year - 2014 + 122)])
        return(a)
}