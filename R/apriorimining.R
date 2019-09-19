#############################################################################
######################## Function: apriorimining ############################
#############################################################################

#' Determine association rules
#' @description Mines association rules using the Apriori Algorithm.
#' The function demands a transactionmatrix, user specified minimum
#' support and minimum confidence and returns generated association rules.
#' First it transforms the data using \code{create_transactionmatrix}, then
#' it uses \code{freq_items} to find frequent itemsets and finally
#' generates association rules using \code{rules}. These functions can also
#' be accessed seperately.
#' The function \code{apriorimining} returns an object of class
#' associationrules, which can be analysed using \code{show} and \code{summary}.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE (0 or FALSE if item is not bought). Columns should be
#' named.
#' @param m_sup User specified minimum support
#' @param m_conf User specified minimum confidence
#' @return Returns an object of class \code{associationrules}
#' @export
#' @include classes_frequentsets.R classes_transactiondata.R
#' classes_associationrules.R
#' @importFrom Matrix sparseMatrix
#' @importClassesFrom Matrix ngCMatrix
#' @importFrom methods new

apriorimining <- function(input, m_sup, m_conf) {


  if (m_sup <= 0 || m_sup > 1) {
    stop("m_sup has to be between 0 and 1")
  }

  if (m_conf <= 0 || m_conf > 1) {
    stop("m_conf has to between 0 and 1")
  }



  # input is changed to an object of class "transactiondata" called transaction_mat
  if (class(input) != "transactiondata") {
    input <- create_transaction(input)
  }

  # a will contain object of class "frequentsets"
  a <- freq_items(input = input, m_sup = m_sup)

  if (all(rowSums(a@sets) == 1 ) || nrow(a@sets) == 0) {
    empty <- new("associationrules",
                 antecedent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 consequent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 measurements = matrix(ncol = 3),
                 items = character(0))
    return(empty)
  }
  # pass a on in order to determine association rules
  a <- rules(a, m_conf)


  return(a)
}


