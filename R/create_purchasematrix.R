#R script containing the first step in apriorimining

create_purchasematrix <- function(input) {
  
  
  purchase_mat <- new("purchase",
                      data = as(input,"ngCMatrix"),
                      items = colnames(input)
  )
  
  colnames(purchase_mat@data) <- NULL
  
  return(purchase_mat)
  
}



  