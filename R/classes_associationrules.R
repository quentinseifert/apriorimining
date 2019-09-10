# R-script containing the class "associationrules" and its generics and methods


#### class

setClass("associationrules",
         slots = list(
           items = "character",
           antecedent = "ngCMatrix",
           consequent = "ngCMatrix",
           measurements = "matrix"
         ))



#### methodes


setMethod("show",
          "associationrules",
          function (object) {
            cat("set of",nrow(object@antecedent), "rules")
          })

setMethod("summary",
          "associationrules",
          function (object) {
            

            idx_storage_a <- lapply(1:dim(object@antecedent)[1],
                      FUN = function(z) {which(object@antecedent[z,])}
                      )

            item_storage_a <- unlist(
                  lapply(
                  1:length(idx_storage_a),
                  FUN = function(z)
                  paste(object@items[idx_storage_a[[z]]], collapse = ", ")))


            idx_storage_c <- sapply(1:dim(object@consequent)[1],
                        FUN = function(z) {which(object@consequent[z,])})

            item_storage_c <- sapply(1:length(idx_storage_c), 
                         FUN = function(z) object@items[idx_storage_c[z]])

            output <- data.frame(item_storage_a,
                    rep("~>", length(item_storage_a)),
                    item_storage_c,
                    rep("||",length(item_storage_a)),
                    object@measurements[,1],
                    object@measurements[,2],
                    object@measurements[,3]
                    )

            colnames(output) <- c("antecedent","~>","consequent","||",
                                  "support","confidence","lift")

            return(output)
            })
