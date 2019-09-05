#R script containg the apriori algortihm
#currently more of a placeholder

apriorimining <- function(input, support, confidence) {


  # input is changed to an object of class "purchase" called purchase_mat
  purchase_mat <- new("purchase",
         data = as(input,"ngCMatrix"),
         items = colnames(input)
  )

  colnames(purchase_mat@data) <- NULL

  # a will contain object of class "itemsets"
  a <- freq_items(purchase = purchase_mat, supp = support)

  # pass a on in order to determine association rules
  a <- rules(a, support, confidence)

  return(a)
}


######## notes

library(arules)
data("Groceries")
bsp<-as(Groceries,"matrix")
abc<-apriorimining(input = bsp,support = 0.01,confidence = 0.5)
class(abc)

show(abc)

