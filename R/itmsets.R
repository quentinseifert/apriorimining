#################
# Class 'itmsets'
# At this point more of a placeholder


setClass("itmsets",
         slots = list(
           sets = "ngCMatrix",
           support = "numeric",
           items = "character"
         ))


setMethod("show", "itmsets",
          function(object) {
            cat("Object of class itmsets, contains", nrow(object@sets),"itmsets")
          })




setMethod("print", "itmsets",
          function(x) {
            itemsets <- apply(x@sets,
                              FUN = function(a, b) {
                                index <- which(a == 1)
                                paste(b[index], collapse = ", ")
                              },
                              b = x@items,
                              MARGIN = 1)

            frame <- data.frame(sets = itemsets,
                                support = support)
            print(frame)
          })

mat <- apply(x@sets,
      FUN = function(a, b) {
        index <- which(a == 1)
        paste(b[index], collapse = ", ")
      },
      b = x@items,
      MARGIN = 1)

itemsets <- apply(sets,
                  FUN = function(a, b) {
                    index <- which(a == 1)
                    paste(b[index], collapse = ", ")
                  },
                  b = object@items,
                  MARGIN = 1)

