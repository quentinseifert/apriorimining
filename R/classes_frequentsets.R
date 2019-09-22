#############################################################################
######################## Class: FrequentSets ################################
#############################################################################

#############################################################################
#### class

#' FrequentSets
#' @description The S4 class \code{FrequentSets} characterises the frequently
#' occuring item sets using four different slots. Objects of the class
#' \code{FrequentSets} can be analysed with \code{show},
#' \code{summary} and \code{plot}.
#' @slot sets Every generated item set summarised in the shape of a sparsematrix
#' (ngCMatrix)
#' @slot supports Numeric vector containing the calculated support for the
#' respective sets
#' @slot items Character vector containing the item names of the transaction matrix
#' @slot minsup User specified minimum support
#' @export
#' @import methods
#' @importFrom Matrix colSums rowSums
#' @importClassesFrom Matrix ngCMatrix
#' @importFrom graphics abline barplot hist legend par

setClass("FrequentSets",
         slots = list(sets = "ngCMatrix",
                      supports = "numeric",
                      items = "character",
                      minsup = "numeric"
                      ))


##############################################################################
#### methods

#' @describeIn FrequentSets Shows the total number of the frequently occuring
#' item sets and how often the different sets sizes occur.
#' @param object Object of class FrequentSets


setMethod("show",
          "FrequentSets",
          function(object) {

            hlp1 <- length(object@supports)

            cat("Overall\n")

            cat(hlp1, "frequently occuring itemsets\n\n")

            # create a list, containing the indices of the frequently occuring
            # item sets from object@sets

            idx_storage <- sapply(1:dim(object@sets)[1],
                                  FUN = function(z) which(object@sets[z,]))

            # determine the largest set size

            k <- length(tail(idx_storage, n=1)[[1]])

            # create a vector as a storage

            counter_vec <- rep(NA, k)

            # count how often the different set sizes occur
            # and store them in counter_vec

            for (i in 1:k) {
              counter <- 0
              for (j in 1:length(idx_storage)) {
                if (length(idx_storage[[j]]) == i) {
                  counter <- counter + 1
                }
              }
              counter_vec[i] <- counter
            }


            cat("In detail\n")
            for (l in 1:k) {
              cat(counter_vec[l], "sets containing", l, "item(s)\n")
            }
          })


#' @describeIn FrequentSets Provides an overview of the frequently occuring item sets
#' @export

setMethod("summary",
          "FrequentSets",
          function(object) {

            sizes <- unique(rowSums(object@sets))

            freqs <- sapply(sizes, function(a) sum(rowSums(object@sets) == a))

            avgs <- sapply(sizes, function(a) {
              sups <- object@supports[rowSums(object@sets) == a]
              avgs <- sum(sups) / length(sups)
              return(avgs)
            })

            cat("Minimum support =", object@minsup, "\n\n")

            frame <- data.frame(set_size = sizes, frequency = freqs,
                                average_support = avgs)

            top_items <- object@items[order(colSums(object@sets), decreasing = TRUE)]

            if (ncol(object@sets) > 5) {
              cat("The 5 most common items in the frequent itemsets are:\n\n")
              cat(top_items[1:5], sep = "\n")
              cat("\n")
            }
            return(frame)
          })

#' @describeIn FrequentSets Plots a bar plot of the frequencies of the different
#' set sizes.
#' @param x Object of class \code{FrequentSets}
#' @export

setMethod("plot",
          "FrequentSets",
          function(x) {

            idx_storage <- sapply(1:dim(x@sets)[1],
                                  FUN = function(z) which(x@sets[z,]))

            # determine the largest set size

            k <- length(tail(idx_storage, n=1)[[1]])

            # create a vector as a storage

            counter_vec <- rep(NA, k)

            # count how often the different set sizes occur
            # and store them in counter_vec

            for (i in 1:k) {
              counter <- 0
              for (j in 1:length(idx_storage)) {
                if (length(idx_storage[[j]]) == i) {
                  counter <- counter + 1
                }
              }
              counter_vec[i] <- counter
            }


            barplot(counter_vec,
                    main = "Set size frequencies",
                    names.arg = seq(1:length(counter_vec)),
                    xlab = "set size",
                    ylab = "frequency"
                    )

          })










