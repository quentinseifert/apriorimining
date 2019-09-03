# R-script containing the class "itemsets" and its generics and methods

#### class

setClass("frequentsets",
         slots = list(
           sets = "ngCMatrix",
           support = "numeric",
           items = "character",
           minsup = "numeric"
         ))

#### generics 

setGeneric("show.itemsets",
           function(object){
           standardGeneric("show.itemsets")
})

#### methodes

setMethod("show",
          "frequentsets",
          function(object) {
            
            idx_storage <- sapply(1:dim(object@sets)[1],
                                  FUN = function(z) {which(object@sets[z,])}
            )
            counter_vec <- rep(NA,length(length(tail(idx_storage,n=1)[[1]])))  
            hlp1 <- length(object@support)
            hlp2 <- length(tail(idx_storage,n=1)[[1]]) 
            
            cat("There are",hlp1,"frequently occuring itemsets.\n")
            cat("The largest frequently occuring itemset contains", hlp2,"Items.\n")  
            
            for(j in 1:length(tail(idx_storage,n=1)[[1]])) {
              
              counter <- 0
              
              for (i in 1:length(idx_storage)) {
                
                if(length(idx_storage[[i]])==j) {
                  counter <- counter + 1
                } 
              }
              counter_vec[j]<-counter
            }
            
            for(k in 1:length(tail(idx_storage,n=1)[[1]])) {
              cat(
                "Frequent Itemsets containing",k,"Item(s), appeared",counter_vec[k],"times.\n"
              )
            }
            cat("\n\n\nThe term \"frequently\" refers to the minimum support threshold (",object@minsup,")")
          }
)

setMethod("show.itemsets",
          "frequentsets",
          function(object) {
            idx_storage <- sapply(1:dim(object@sets)[1],
                                  FUN = function(z) {which(object@sets[z,])}
                                  )
            storage <- sapply(1:length(idx_storage),
                               FUN = function(z) {object@items[idx_storage[[z]]]}
                               )
            longest_set <- length(tail(idx_storage,n=1)[[1]])
            
            
            for (i in 1:longest_set) {
              
              cat("\n##############################")
              cat("\nItemsets containing", i ,"item(s):\n\n")
            for (j in 1:length(storage)) {
                
              if(length(storage[[j]]) == i) {
                  
                cat(storage[[j]],"\n",sep = ",")
                }
              } 
            }
          }
        )


setMethod("plot",
          "frequentsets",
          function(x) {
            idx_storage <- sapply(1:dim(x@sets)[1],
                                  FUN = function(z) {which(x@sets[z,])}
            )
            counter_vec <- rep(NA,length(length(tail(idx_storage,n=1)[[1]])))  
            longest_set <- length(tail(idx_storage,n=1)[[1]])
            
            for(j in 1:length(tail(idx_storage,n=1)[[1]])) {
              
              counter<-0
              
              for (i in 1:length(idx_storage)) {
                
                if(length(idx_storage[[i]]) == j) {
                  counter <- counter + 1
                  
                } 
              }
              counter_vec[j] <- counter
              
            }
          
            if(longest_set > 3) {
              
              par(mfrow = c(2,ceiling(longest_set / 2)))   
            } else {
                    
              par(mfrow = c(1,3))
                  }
            
            for (i in 1:longest_set) {
              
              hist(x@support[seq(1:counter_vec[i])],
                   xlab="support",
                   freq=T,
                   main=c(""),
                   breaks=20,
                   col=8)
              abline(v=0.01,col=2,lwd=3)
              legend("topright",c("minsup"),lty = 1,col=2)
              title(i)
            }
         })
              

              


















