#############################################################################
######################## Function: create_transaction #######################
#############################################################################

#' Transform transactionmatrix
#' @description Transforms given transactionmatrix into an object
#' of class transactiondata, enabling the user to examine the transactiondata
#' with provided methodes.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE.
#' (0 / FALSE if item is not bought)
#' Columns should be named.
#' @return Object of class transactiondata
#' @export
#' @import Matrix
#' @include classes_transactiondata.R


create_transaction <- function(input) {


  transaction_mat <- new("transactiondata",
                      data = as(input,"ngCMatrix"),
                      items = colnames(input)
  )

  colnames(transaction_mat@data) <- NULL

  return(transaction_mat)

}


