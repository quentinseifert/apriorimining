#

apriorimining <- function(input, support, confidence) {


  # input is changed to an object of class "transactiondata" called transaction_mat
  a <- create_transaction(input)

  # a will contain object of class "itemsets"
  a <- freq_items(transactions = a, supp = support)
  if (all(rowSums(a@sets) == 1 ) || nrow(a@sets) == 0) {
    empty <- new("associationrules",
                 antecedent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 consequent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 measurements = matrix(),
                 items = character(0))
    return(empty)
  }
  # pass a on in order to determine association rules
  a <- rules(a, support, confidence)


  return(a)
}

