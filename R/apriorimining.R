#############################################################################
######################## Function: apriorimining ############################
#############################################################################

#' Determine association rules
#' @description Mines association rules using the Apriori Algorithm.
#' The function demands a transaction matrix, user specified minimum
#' support and minimum confidence and returns generated association rules.
#' First \code{apriorimining} transforms the data into an
#' object of class \code{TransactionData} using \code{create_transactionmatrix},
#' then the function passes this object
#' to \code{freq_items} in order to find frequent item sets. Finally \code{apriorimining}
#' generates association rules from those frequent item sets using \code{rules}.
#' These functions can also be accessed seperately.
#' The function \code{apriorimining} returns an object of class
#' AssociationRules, which can be analysed using \code{show} and \code{summary}.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE (0 or FALSE if item is not bought). Columns should be
#' named.
#' @param m_sup User specified minimum support
#' @param m_conf User specified minimum confidence
#' @return Returns an object of class \code{AssociationRules}
#' @examples
#' \dontrun{
#' ## Load data
#' data(Groceries)
#' ## Mine rules
#' x <- apriorimining(Groceries, m_sup = 0.05, m_conf = 0.3)
#' ## use methods
#' summary(x)
#' }
#' @export
#' @include classes_frequentsets.R classes_transactiondata.R
#' classes_associationrules.R
#' @importFrom Matrix sparseMatrix
#' @importClassesFrom Matrix ngCMatrix
#' @importFrom methods new

apriorimining <- function(input, m_sup, m_conf) {

  # check whether m_sup and m_conf are valid

  if (m_sup <= 0 || m_sup > 1) {
    stop("m_sup has to be between 0 and 1")
  }

  if (m_conf <= 0 || m_conf > 1) {
    stop("m_conf has to between 0 and 1")
  }

  # input is changed to an object of class "TransactionData"

  if (class(input) != "TransactionData") {
    input <- create_transaction(input)
  }

  # find frequent item sets

  a <- freq_items(input = input, m_sup = m_sup)

  # if no rules cn be found, return empty object

  if (all(rowSums(a@sets) == 1 ) || nrow(a@sets) == 0) {
    empty <- new("AssociationRules",
                 antecedent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 consequent = t(sparseMatrix(i = {}, j = {}, dims = c(0, 0))),
                 measurements = matrix(ncol = 3),
                 items = character(0)
                 )

    return(empty)
  }

  # find rules

  a <- rules(a, m_conf)

  return(a)
}


