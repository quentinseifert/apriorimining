#############################################################################
######################## Function: create_transaction #######################
#############################################################################

#' Transform the transaction matrix
#' @description Transforms a given transaction matrix into an object
#' of class TransactionData, enabling the user to examine the TransactionData
#' with the provided methods.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE (0 or FALSE if item is not bought). Columns should be
#' named.
#' @return Returns an object of class \code{TransactionData}
#' @export
#' @import Matrix
#' @include classes_transactiondata.R


create_transaction <- function(input) {

  if (class(input) != "ngCMatrix" && class(input) != "matrix") {
    stop("Input has to be of class 'TransactionData', 'matrix' or 'ngCMatrix'")
  }

  transaction_mat <- new("TransactionData",
                         data = as(input,"ngCMatrix"),
                         items = colnames(input)
                         )

  colnames(transaction_mat@data) <- NULL

  return(transaction_mat)
}



