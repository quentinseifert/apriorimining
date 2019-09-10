#R script containg the apriori algortihm
#currently more of a placeholder

apriorimining <- function(input, support, confidence) {
  
  
  # input is changed to an object of class "purchase" called purchase_mat
  a <- create_purchasematrix(input)
  
  # a will contain object of class "itemsets"
  a <- freq_items(purchase = a, supp = support)
  
  # pass a on in order to determine association rules
  a <- rules(a, support, confidence)
  
  return(a)
}

