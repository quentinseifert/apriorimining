#############################################################################
######################## Class: transactiondata #############################
#############################################################################

#############################################################################
#### class

#' Transactiondata
#' @description The S4 class \code{transactiondata} characterizes the
#' entered transactionmatrix using two different slots. An object of the
#' class \code{transactiondata} can be analyzed with \code{show}, \code{summary}
#' ,\code{plot}, \code{itemtail} and \code{tryminimum}.
#' @slot data Transaction data in the shape of an sparsematrix (ngCMatrix)
#' @slot items Character vector containing the itemnames of the
#' trancastionmatrix
#' @export
#' @import methods
#' @import Matrix
#' @import graphics

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

setGeneric("itemtail",
           function (object, support, absolute = FALSE, rare = FALSE){
             standardGeneric("itemtail")
           })

setGeneric("tryminimum",
           function (object){
             standardGeneric("tryminimum")
           })

###############################################################################
#### methoden


#' @describeIn transactiondata Shows number of transactions and items in the transactionmatrix
#' @param object object of class \code{transactiondata}

setMethod("show",
          "transactiondata",
          function(object) {
            cat("The transactionmatrix contains",dim(object@data)[1],"transactions and"
                ,dim(object@data)[2],"items.")

          })


#' @describeIn transactiondata Returns data.frame of each item in the transactionmatrix and
#' their frequencies

setMethod("summary",
          "transactiondata",
          function (object) {
            storage <-rep(NA, length(object@items))

            #calculate the sum of each column, to get the total frequencie
            #of each item

            for (i in 1:length(object@items)) {

              storage[i] <- sum(object@data[,i])
            }

            ordered_storage_ind <- order(storage)

            #use the ordered indizes to create a vector containing the
            #ascending frequencies and a vector containing the respective
            #item names

            item <- object@items[ordered_storage_ind]
            frequency <- storage[ordered_storage_ind]

            df <- data.frame(item, frequency)

            return(df)
          })




#' @describeIn transactiondata returns data.frame of either the most freqeunt
#' single items or the most rare single items.
#' User can choose wether the occurence of the items is displayed in terms
#' of absolute or relative values.
#' @param support user specified minimum support
#' @param absolute logical
#' @param rare logical

setMethod("itemtail",
          "transactiondata",
          function (object, support, absolute = FALSE, rare = FALSE) {

          #controlflow divided into user specified

          if (rare) {

            means <- colMeans(object@data)
            names <- object@items[means < support]

            if (absolute) {

              absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,x])})
              values <- absolut[means < 0.01]
              ordered_ind <- order(values)
              frequency <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "frequency.absolut" = frequency)
            } else {

              values <- round(means[means < support], digits = 4)
              ordered_ind <- order(values)
              means <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "frequency.relative" = means)
            }

          } else {

            means <- colMeans(object@data)
            names <- object@items[means > support]


            if (absolute) {

              absolut <- sapply(1:length(object@items), function(x) {sum(object@data[,x])})
              values <- absolut[means > support]
              ordered_ind <- order(values)
              frequency <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "frequency.absolut" = frequency)
            } else {

              values <- round(means[means > support], digits = 4)
              ordered_ind <- order(values)
              means <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "frequency.relative" = means)
            }
          }

            return(storage)
          })

#' @describeIn transactiondata barplot of the most frequently occuring
#' items displaying their absolute frequencies
#' @param x object of class \code{transactiondata}

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


setMethod("tryminimum",
          "transactiondata",
          function (object) {

            storage <- NULL
            a <- NULL
            b <- NULL
            minsups <- c(0.01, 0.05, 0.10, 0.20)

            means <- colMeans(object@data)
            means <- means[order(means)]

            for (i in 1:length(minsups)) {

              storage[i] <- sum(means > minsups[i])

            }

            for (i in 1:length(minsups)) {

              a[i] <- paste("Minimum support of",sprintf("%3.2f", minsups[i]),"~>")
              b[i] <- paste(storage[i],"of",length(means),"items will be frequent")
            }

            df <- data.frame(a, b)
            colnames(df) <- NULL

            return(df)
          })

