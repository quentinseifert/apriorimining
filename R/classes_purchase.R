# R-script containing the class "transaction" and its generics and methods

#### new object (test)
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

setGeneric("rareitems",
           function (object, support, absolute = FALSE){
             standardGeneric("rareitems")
           }
          )



#### methoden

setMethod("show",
          "purchase",
          function(object) {
            cat("The transactionmatrix contains",dim(object@data)[1],"transactions and"
                ,dim(object@data)[2],"items.")

          })



setMethod("summary",
          "purchase",
          function (object) {
            storage <-rep(NA, length(object@items))

            for (i in 1:length(object@items)) {

              storage[i] <- sum(object@data[,i])
            }

            ordered_storage <- order(storage)
            item <- object@items[ordered_storage]
            absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,ordered_storage[x]])})

            df <- data.frame(item, absolut)

            return(df)
          })



setMethod("rareitems",
          "purchase",
          function (object, support, absolute = FALSE) {

            means <- colMeans(object@data)
            names <- object@items[means < support]

            if (absolute) {
              absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,x])})
              values <- absolut[means < 0.01]
              ordered_ind <- order(values)
              count <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "..." = rep("...",length(item)), count)
            } else {
              values <- round(means[means < support], digits = 4)
              ordered_ind <- order(values)
              means <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "..." = rep("...",length(item)), means)
            }
            return(storage)
          })


setMethod("rareitems.absolut",
          "purchase",
          function (object) {
            means <- colMeans(object@data)
            names <- object@items[means < 0.01]

            absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,x])})
            smallabsolut <- absolut[means < 0.01]

            smallabsolut_orderedidx <- order(smallabsolut)

            mean <- smallabsolut[smallabsolut_orderedidx]
            item <- names[smallabsolut_orderedidx]

            storage <- data.frame(item, "..." = rep("...",length(item)), mean)

            return(storage)
          })


setMethod("plot",
          "purchase",
          function(x) {

            par(mfrow=c(1,1))
            freqs <- colSums(x@data)
            order(freqs, decreasing = TRUE)
            ordered_ind <- order(freqs, decreasing = TRUE)
            names <- x@items[ordered_ind]
            freqs <- freqs[ordered_ind]
            limit <- round(length(freqs)*0.1)
            barplot(freqs[1:limit], names.arg = names[1:limit], las = 2, cex.names = 0.75)

          })





