#############################################################################
######################## Class: TransactionData #############################
#############################################################################

#############################################################################
#### class

#' TransactionData
#' @description The S4 class \code{TransactionData} characterises the
#' entered transaction matrix using two different slots. An object of the
#' class \code{TransactionData} can be analysed with \code{show}, \code{summary}
#' ,\code{plot}, \code{itemtail} and \code{tryminimum}.
#' @slot data Transaction matrix in the shape of a sparsematrix (ngCMatrix)
#' @slot items Character vector containing the item names of the
#' trancastion matrix
#' @export
#' @import methods
#' @importClassesFrom Matrix ngCMatrix
#' @importFrom graphics abline barplot hist legend par

setClass("TransactionData",
         slots = list(data = "ngCMatrix",
                      items = "character"
                      ))

###############################################################################
#### generics

#' Test minimum support
#' @param object Object of class TransactionData
#' @param support user specified minimum support
#' @param absolute logical
#' @param rare logical
#' @export

setGeneric("itemtail",
           function (object, support, absolute = FALSE, rare = FALSE){

             standardGeneric("itemtail")
           })

#' Test influence of different minimum supports
#' @param object Object of class TransActiondata
#' @export

setGeneric("tryminimum",
           function (object){

             standardGeneric("tryminimum")
           })

###############################################################################
#### methods


#' @describeIn TransactionData Shows number of transactions and items
#' in the transaction matrix
#' @param object Object of class \code{TransactionData}

setMethod("show",
          "TransactionData",
          function(object) {

            cat("The transactionmatrix contains", dim(object@data)[1],
                "transactions and", dim(object@data)[2], "items.")
          })


#' @describeIn TransactionData Returns data.frame of each item in the
#' transaction matrix and their frequencies
#' @export

setMethod("summary",
          "TransactionData",
          function(object) {

            storage <-rep(NA, length(object@items))

            # calculate the sum of each column, to get the total frequency
            # of each item

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


#' @describeIn TransactionData Returns data.frame of either the most freqeunt
#' single items or the most rare single items.
#' User can choose wether the occurence of the items is displayed in terms
#' of absolute or relative values.
#' @param support User specified minimum support
#' @param absolute Logical
#' @param rare Logical
#' @export

setMethod("itemtail",
          "TransactionData",
          function (object, support, absolute = FALSE, rare = FALSE) {

            if (rare) {

              means <- colMeans(object@data)
              names <- object@items[means < support]

            if (absolute) {

              absolut <- sapply(1:length(object@items), function(x) sum(object@data[,x]))
              values <- absolut[means < support]
              ordered_ind <- order(values)
              frequency <- values[ordered_ind]
              item <- names[ordered_ind]
              storage <- data.frame(item, "frequency.absolute" = frequency)
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
              storage <- data.frame(item, "frequency.absolute" = frequency)
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

#' @describeIn TransactionData Bar plot of the most frequently occuring
#' items displaying their absolute frequencies
#' @param x Object of class \code{TransactionData}
#' @export

setMethod("plot",
          "TransactionData",
          function(x) {

            # calculating the absolute frequency for each item

            freqs <- colSums(x@data)

            # create an index vector containing the positions
            # of the ordered frequencies

            ordered_idx <- order(freqs, decreasing = TRUE)

            names <- x@items[ordered_idx]
            freqs <- freqs[ordered_idx]
            limit <- round(length(freqs)*0.1)

            barplot(freqs[1:limit],
                    names.arg = names[1:limit],
                    las = 2,
                    cex.names = 0.75
                    )
          })



#' @describeIn TransactionData Provides the number of frequent items for four
#'different minimum support values, giving an impression of the influence of
#'the minimum support
#' @export


setMethod("tryminimum",
          "TransactionData",
          function(object) {

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

              a[i] <- paste("Minimum support of", sprintf("%3.2f", minsups[i]),"~>")
              b[i] <- paste(storage[i], "of", length(means), "items will be frequent")
            }

            df <- data.frame(a, b)
            colnames(df) <- NULL

            return(df)
          })

