# R-script containing the class "transaction" and its generics and methods

#### new object (test) 
rm(list=ls())
library(arules)
data("Groceries")
input <- as(Groceries,"matrix")
testest<-new("purchase",
             data = as(input,"ngCMatrix"),
             items = colnames(input)
)


#### class

setClass("purchase",
         slots = list(
           data = "ngCMatrix",
           items = "character"
         ),
         validity = function(object) {
           if (is.character(object) == TRUE) {
            
              cat("Basic data type needs to be numeric or logical.")
           }
           
           if (ncol(object@data) != length(object@items)) {
             
             cat("ncol(data) and length(items) do not match.")
           }
           
         })

#### generics

setGeneric("show.items",
           function (object) {
             standardGeneric("show.items")
           }
          )

setGeneric("show.rare.items",
           function (object){
             standardGeneric("show.rare.items")
           }
          )

#### methoden

setMethod("show",
          "purchase",
          function(object) {
            
          })


setMethod("show.items",
          "purchase",
          function (object) {
            storage <-rep(NA, length(object@items))
            for (i in 1:length(object@items)) {
              
              storage[i] <- sum(object@data[,i])  
            }
            
            ordered_storage <- order(storage)
            for(i in 1:length(object@items)) {
              
              cat(object@items[ordered_storage[i]],":",sum(object@data[,ordered_storage[i]]),"\n")
            }
          }
)

# currently not working:
setMethod("show.rare.items",
          "purchase",
          function (object) {
            rare_items <- which(colMeans(testest@data) < 0.01)
            storage <- rep(NA, length(testest@items))
            
            for (i in 1:length(testest@items)) {
              
              storage[i] <- sum(testest@data[,i])  
            }
            
            ordered_storage <- order(storage)
            for(i in 1:length(rare_items)) {
              
              cat(testest@items[ordered_storage[as.numeric(rare_items[i])]],":",
                  sum(testest@data[,ordered_storage[as.numeric(rare_items[i])]]),"\n")
            }
            
          })








