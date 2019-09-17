##############################################################################
######################## CLASS: associationrules #############################
##############################################################################

##############################################################################
#### class

#' associationrules
#' @description The S4 class \code{associationrules} characterizes the generated
#' association rules using four different slots. An object of class
#' \code{associationrules} can be analysed using \code{show} and \code{summary}
#' @slot items Character vector containing the itemnames of the transactionmatrix
#' @slot antecedent Each antecedent of every generated association rule 
#' in the shape of an sparsematrix (ngCMatrix) 
#' @slot consequent Each consequent of every generated association rule 
#' in the shape of an sparsematrix (ngCMatrix) 
#' @slot measurements Quality measurements (support, confidence, lift)
#' for every generated association rule 
#' @export

setClass("associationrules",
         slots = list(
           items = "character",
           antecedent = "ngCMatrix",
           consequent = "ngCMatrix",
           measurements = "matrix"
         ))


#############################################################################
#### methodes


#' @describeIn associationrules Shows the number of the generated association rules

setMethod("show",
          "associationrules",
          function (object) {
            cat("set of",nrow(object@antecedent), "rules")
          })



#' @describeIn associationrules returns data.frame of all generated
#' association rules and their respective
#' quality measurements

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
