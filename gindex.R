

#g index definition 


# function(data, totct = "tc") {
#         # data is a data.frame contains a single author's research articles.
#         # totct is a variable name(a column index) that represents the total 
#         # citation of each article.
#         Hindex <- function(data, totct = "tc") {
#                 a <- vector()
#                 b <- vector()
#                 tlc <- data[, totct]
#                 for (i in 1:length(tlc)) {
#                         a[i] <- ifelse(length(which(tlc >= i)) == i, i, 0)
#                         b[i] <- ifelse(length(which(tlc >= i)) <= i,
#                                        i, length(tlc))
#                 }
#                 q <- sum(a)
#                 h <- ifelse(q == 0, min(b)-1,q)
#                 h
#         }
#         c <- vector()
#         x <- data[,totct]
#         data <- data[order(x,decreasing = TRUE), ]
#         for (i in 1:length(x)) c[i] <- ifelse(sum(x[1:i]) >= i^2, 1, 0)
#         a <- ifelse(sum(c) < sqrt(sum(x[1:Hindex(data)])), floor(sqrt(sum(x))), sum(c) )
#         return(a)
# }


# Defined in the previous function above , when g-index equals to h-index, this 
#means one's publication is few while his total citaions is very huge. Previously,
#the g-index can bigger than one's total publication, but now, it can't. So I 
#write this function to change this.


gindex <- function(data, totct = "tc"){
        c <- vector()
                 x <- data[,totct]
                 data <- data[order(x,decreasing = TRUE), ]
                 for (i in 1:length(x)) c[i] <- ifelse(sum(x[1:i]) >= i^2, 1, 0)
        return(sum(c))
}
