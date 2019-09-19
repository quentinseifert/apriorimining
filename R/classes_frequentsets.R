#############################################################################
######################## Class: frequentsets ################################
#############################################################################

#############################################################################
#### class

#' Frequentsets
#' @description The S4 class \code{frequentsets} characterizes the frequently
#' occuring itemsets using four different slots. Objects of the class
#' \code{frequentsets} can be analyzed with \code{show},
#' \code{summary} and \code{plot}.
#' @slot sets Every generated itemset summarized in the shape of an sparsematrix
#' (ngCMatrix)
#' @slot supports Numeric vector containing the calculated support for the
#' respective sets
#' @slot items Character vector containing the itemnames of the transactionmatrix
#' @slot minsup User specified minimum support
#' @export
#' @import methods
#' @importFrom Matrix colSums rowSums
#' @importClassesFrom Matrix ngCMatrix
#' @importFrom graphics abline barplot hist legend par

setClass("frequentsets",
         slots = list(
           sets = "ngCMatrix",
           supports = "numeric",
           items = "character",
           minsup = "numeric"
         ))

##############################################################################
#### generics


##############################################################################
#### methods

#' @describeIn frequentsets Shows the total number of the frequently occuring
#' itemsets and how often the different sets sizes occur.
#' @param object object of class frequentsets


setMethod("show",
          "frequentsets",
          function(object) {

            hlp1 <- length(object@supports)

            cat("Overall\n")
            cat(hlp1,"frequently occuring itemsets\n\n")

            #create a list, containing the indices of the frequently occuring
            #itemsets from object@sets

            idx_storage <- sapply(1:dim(object@sets)[1],
                                  FUN = function(z) {which(object@sets[z,])})

            #determine the largest set size

            k <- length(tail(idx_storage,n=1)[[1]])

            #create a vector as a storage

            counter_vec <- rep(NA, k)

            #count how often the different set sizes occur
            #and store them in counter_vec

            for(i in 1: k) {

              counter <- 0

              for (j in 1:length(idx_storage)) {

                if(length(idx_storage[[j]]) == i) {

                  counter <- counter + 1
                }
              }
              counter_vec[i]<-counter
            }

            #cat the counter_vec

            cat("In detail\n")

            for(l in 1:k) {

              cat(counter_vec[l], "sets containing",l,"item(s)\n")
            }
          })


#' @describeIn frequentsets Provides an overview of the frequently occuring itemsets

setMethod("summary",
          "frequentsets",
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

#' @describeIn frequentsets Plots a histogram of the support distribution for each set size
#' @param x object of class \code{frequentsets}

setMethod("plot",
          "frequentsets",
          function(x) {

            #create a list, containing the indices of the frequently occuring
            #itemsets from x@sets

            idx_storage <- sapply(1:dim(x@sets)[1],
                                  FUN = function(z) {which(x@sets[z,])})

            #determine the largest set size

            k <- length(tail(idx_storage,n=1)[[1]])

            #create a vector as a storage

            counter_vec <- rep(NA, k)

            #count how often the different set sizes occur
            #and store them in counter_vec

            for(i in 1: k) {

              counter <- 0

              for (j in 1:length(idx_storage)) {

                if(length(idx_storage[[j]]) == i) {

                  counter <- counter + 1
                }
              }
              counter_vec[i]<-counter
            }

            #seperate the screen for plotting

            if(k > 3) {

              par(mfrow = c(2,ceiling(k / 2)))
            } else {

              par(mfrow = c(1,3))
            }

            #plot the supports for different set sizes

            for (i in 1:k) {

              hist(x@supports[seq(1:counter_vec[i])],
                   xlab = paste0("support ", "(set size: ",i,")"),
                   freq=T,
                   main=c(""),
                   breaks=40,
                   col=8)

              abline(v = x@minsup, col=2, lwd=1)

              legend("topright", legend = c("minsup"), lty = 1, col = 2, cex = 0.75)
            }
         })









