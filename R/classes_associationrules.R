##############################################################################
######################## Class: AssociationRules #############################
##############################################################################

##############################################################################
#### class

#' AssociationRules
#' @description The S4 class \code{AssociationRules} characterises the generated
#' association rules using four different slots. An object of class
#' \code{AssociationRules} can be analysed using \code{show} and \code{summary}.
#' @slot items Character vector containing the item names of the transaction matrix
#' @slot antecedent Each antecedent of every generated association rule
#' in the shape of a sparse matrix (ngCMatrix)
#' @slot consequent Each consequent of every generated association rule
#' in the shape of a sparse matrix (ngCMatrix)
#' @slot measurements Quality measurements (support, confidence, lift)
#' for every generated association rule
#' @slot m_values User specified minimum values for support and confidence
#' @export
#' @import methods
#' @importClassesFrom Matrix ngCMatrix

setClass("AssociationRules",
         slots = list(items = "character",
                      antecedent = "ngCMatrix",
                      consequent = "ngCMatrix",
                      measurements = "matrix",
                      m_values = "numeric"
                      ))


#############################################################################
#### methodes


#' @describeIn AssociationRules Shows the number of the generated association rules
#' @param object Object of class \code{AssociationRules}

setMethod("show",
          "AssociationRules",
          function (object) {

            cat(nrow(object@antecedent), "rules can be generated\n\n")
            cat(paste0("(m_sup = ", object@m_values[1], ","), paste0("m_conf = ", object@m_values[2], ")"))
          })

#' @describeIn AssociationRules Returns a data.frame of all generated
#' association rules and their respective quality measurements
#' @export

setMethod("summary",
          "AssociationRules",
          function (object) {

            # store the indices of the antecedents in a list

            idx_storage_a <- lapply(1:dim(object@antecedent)[1],
                                    FUN = function(z) which(object@antecedent[z,])
                                    )

            # use idx_storage_a to get the respective item names
            # from object@items and unlist the generated list
            # so it can be directly integrated in the data.frame

            item_storage_a <- lapply(1:length(idx_storage_a),
                                     FUN = function(z) {
                                       paste(object@items[idx_storage_a[[z]]],
                                                             collapse = ", ")
                                       })

            item_storage_a <- unlist(item_storage_a)


            # store the indices of the consequents in a vector

            idx_storage_c <- sapply(1:dim(object@consequent)[1],
                                    FUN = function(z) {
                                      which(object@consequent[z,])
                                    })

            # since the consequent is always one item, it is not
            # necessary to create a list

            item_storage_c <- sapply(1:length(idx_storage_c),
                                     FUN = function(z) object@items[idx_storage_c[z]]
                                     )

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
