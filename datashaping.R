#This is a data shaping function

datash <- function(directory){
        file_list <- list.files(directory,pattern=".csv",full.names = TRUE)
        #file_list1 <- list.files(directory,pattern = ".csv")
        num <- length(file_list)
        researcher_data <- matrix(rep(0,123))
        dim(researcher_data) <- c(1,123)
        researcher_data <- data.frame(researcher_data)
        colnames(researcher_data) <- c("researcher","title","authors","source","pyear",
                                       "tc","ac",1900:2015)
        
        for (i in file_list) {
                resc <- read.csv(i,stringsAsFactors = FALSE)
               
                aut <- strsplit(i,".csv")[[1]][1]
                aut <- strsplit(aut,"1/")[[1]][2]
                if (resc[28,1] == "标题"){
                        d1 <- data.frame(cbind(aut,resc[29:(dim(resc)[1]),c(1,2,6,8,20:137)]))
                } else {
                        d1 <- data.frame(cbind(aut,resc[28:(dim(resc)[1]),c(1,2,6,8,20:137)]))  
                }
                colnames(d1) <- c("researcher","title","authors","source","pyear",
                                  "tc","ac",1900:2015)
                researcher_data <- rbind(researcher_data,d1)
        }
         researcher_data <- researcher_data[-1,]
        for(i in 5:123){researcher_data[,i] <- as.numeric(researcher_data[,i])}
      
        researcher_data <- researcher_data[,-123]
        
}


datash2 <- function(directory){
        file_list <- list.files(directory,pattern=".csv",full.names = TRUE)
        #file_list1 <- list.files(directory,pattern = ".csv")
        num <- length(file_list)
        researcher_data <- matrix(rep(0,123))
        dim(researcher_data) <- c(1,123)
        researcher_data <- data.frame(researcher_data)
        colnames(researcher_data) <- c("researcher","title","authors","source","pyear",
                                       "tc","ac",1900:2015)
        
        for (i in file_list) {
                resc <- read.csv(i,stringsAsFactors = FALSE)
                
                aut <- strsplit(i,".csv")[[1]][1]
                aut <- strsplit(aut,"2/")[[1]][2]
                
                if (resc[28,1] == "标题"){
                d1 <- data.frame(cbind(aut,resc[29:(dim(resc)[1]),c(1,2,6,8,20:137)]))
                } else {
                d1 <- data.frame(cbind(aut,resc[28:(dim(resc)[1]),c(1,2,6,8,20:137)]))  
                }
                colnames(d1) <- c("researcher","title","authors","source","pyear",
                                  "tc","ac",1900:2015)
                researcher_data <- rbind(researcher_data,d1)
        }
        researcher_data <- researcher_data[-1,]
        for(i in 5:123){researcher_data[,i] <- as.numeric(researcher_data[,i])}
        
        researcher_data <- researcher_data[,-123]
        
}