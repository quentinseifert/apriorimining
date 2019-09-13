#############################################################################
######################## Function: apriorimining ############################
#############################################################################

#' Determine association rules
#' @description Mines association rules using the Apriori Algorithm.
#' Demands a transactionmatrix, user specified minimum
#' support and minimum confidence and returns generated association rules.
#' First it transforms the data using \code{create_transactionmatrix}, then
#' it uses \code{freq_items} to find frequent itemsets and finally
#' generates association rules using \code{rules}. These function can also be
#' accessed seperately.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE.
#' (0 / FALSE if item is not bought)
#' Columns should be named.
#' @param m_sup User specified minimum support
#' @param m_conf User specified minimum confidence
#' @return Object of class associationrules
#' @export
#' @import Matrix
#' @import methods
#' @include classes_frequentsets.R classes_transactiondata.R
#' classes_associationrules.R

apriorimining <- function(input, m_sup, m_conf) {


  # input is changed to an object of class "transactiondata" called transaction_mat
  a <- create_transaction(input)

  # a will contain object of class "itemsets"
  a <- freq_items(transactions = a, supp = m_sup)
  if (all(rowSums(a@sets) == 1 ) || nrow(a@sets) == 0) {
    empty <- new("associationrules",
                 antecedent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 consequent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 measurements = matrix(),
                 items = character(0))
    return(empty)
  }
  # pass a on in order to determine association rules
  a <- rules(a, m_sup, m_conf)


  return(a)
}


