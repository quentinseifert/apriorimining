#############################################################################
######################## CLASS: frequentsets ################################
#############################################################################




##############################################################################
#### class

#' frequentsets
#' @description  frequentsets
#' @slot sets sets in the shape of an
#' @slot supports numeric vector containing the support for the respective sets
#' @slot items character vector containing the itemnames
#' @slot minsup minimum support used to determine the frequent itemsets
#' @export

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

#' @describeIn frequentsets Shows the total number of the frequently occuring itemsets


setMethod("show",
          "frequentsets",
          function(object) {

            idx_storage <- sapply(1:dim(object@sets)[1],
                                  FUN = function(z) {which(object@sets[z,])}
            )
            counter_vec <- rep(NA,length(length(tail(idx_storage,n=1)[[1]])))
            hlp1 <- length(object@supports)
            #hlp2 <- length(tail(idx_storage,n=1)[[1]])

            cat("Overall ->",hlp1,"frequently occuring itemsets\n\n")
            #cat("The largest frequently occuring itemset contains", hlp2,"Items.\n")

            for(j in 1:length(tail(idx_storage,n=1)[[1]])) {

              counter <- 0

              for (i in 1:length(idx_storage)) {

                if(length(idx_storage[[i]])==j) {
                  counter <- counter + 1
                }
              }
              counter_vec[j]<-counter
            }

            cat("In detail\n")

            for(k in 1:length(tail(idx_storage,n=1)[[1]])) {
              cat(
                counter_vec[k], "sets containing",k,"item(s)\n"
              )
            }
            cat("\n\n\nThe term \"frequently\" refers to your chosen supports of(",object@minsup,")")
          }
)


#' @describeIn frequentsets Provides an overview of the frequently overview
#' of the frequently occuring itemsets

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
            cat("minimum supports =", object@minsup, "\n\n")
            frame <- data.frame(set_sizes = sizes, frequencies = freqs,
                                average_supports = avgs)

            top_items <- object@items[order(colSums(object@sets), decreasing = TRUE)]
            if (ncol(object@sets) > 5) {
              cat("The 5 most common items in the frequent itemsets are:\n")
              cat(top_items[1:5], sep = "\n")
              cat("\n")
            }


            return(frame)
          }
        )



#' Plots a histogram of the support distribution for each set size
#' @describeIn frequentsets Plots a histogram of the support distribution for each set size

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

              hist(x@supports[seq(1:counter_vec[i])],
                   xlab=c("supports",i,"item(s)"),
                   freq=T,
                   main=c(""),
                   breaks=20,
                   col=8)
              abline(v=0.01,col=2,lwd=3)
              legend("topright",c("minsup"),lty = 1,col=2)

            }
         })









