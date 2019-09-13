#############################################################################
######################## CLASS: transactiondata #############################
#############################################################################


#############################################################################
#### class

#' transactiondata
#' @description Class containing the transaction data
#' @slot data transaction data as a ngCMatrix
#' @slot items character vector containing itemnames
#' @export

setClass("transactiondata",
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

###############################################################################
#### generics



setGeneric("rareitems",
           function (object, support, absolute = FALSE){
             standardGeneric("rareitems")
           })

setGeneric("frequentitems",
           function (object, support, absolute = FALSE){
             standardGeneric("frequentitems")
           })

setGeneric("supporttesting",
           function (object){
             standardGeneric("supporttesting")
           })


#### methoden


#' @describeIn transactiondata Shows number and items in the data

setMethod("show",
          "transactiondata",
          function(object) {
            cat("The transactionmatrix contains",dim(object@data)[1],"transactions and"
                ,dim(object@data)[2],"items.")

          })


#' @describeIn transactiondata Returns data.frame of all item in the data and
#' their frequencies

setMethod("summary",
          "transactiondata",
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




#' @describeIn transactiondata ' lists all items with a
#' support below are user specified minimum support

setMethod("rareitems",
          "transactiondata",
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




#' @describeIn transactiondata barplot of the most frequently occuring
#' items displaying their absolute frequencies

setMethod("plot",
          "transactiondata",
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



#' @describeIn transactiondata provides the number of frequent items for four
#'different minimum support values, giving an impression of the influence of
#'the minimum support

setMethod("supporttesting",
          "transactiondata",
          function (object) {

            storage <- NULL
            a <- NULL
            b <- NULL
            mean_thresholds <- c(0.01, 0.05, 0.10, 0.20)

            means <- colMeans(object@data)
            means <- means[order(means)]

            for (i in 1:length(mean_thresholds)) {

              storage[i] <- sum(means > mean_thresholds[i])

            }

            for (i in 1:length(mean_thresholds)) {

              a[i] <- paste("Minimum support of",sprintf("%3.2f", mean_thresholds[i]),"~>")
              b[i] <- paste(storage[i],"of",length(means),"items will be frequent")
            }

            df <- data.frame(a, b)
            colnames(df) <- NULL

            return(df)
          })




setMethod("frequentitems",
          "transactiondata",
          function (object, support, absolute = FALSE) {

            means <- colMeans(object@data)
            names <- object@items[means > support]

            if (absolute) {
              absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,x])})
              values <- absolut[means > support]
              ordered_ind <- order(values)
              count <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "..." = rep("...",length(item)), count)
            } else {
              values <- round(means[means > support], digits = 4)
              ordered_ind <- order(values)
              means <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "..." = rep("...",length(item)), means)
            }
            return(storage)
          })


