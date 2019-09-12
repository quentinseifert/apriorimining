#R script containing the first step in apriorimining

create_transaction <- function(input) {


  transaction_mat <- new("transactiondata",
                      data = as(input,"ngCMatrix"),
                      items = colnames(input)
  )

  colnames(transaction_mat@data) <- NULL

  return(transaction_mat)

}



