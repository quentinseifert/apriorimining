#############################################################################
######################## Function: rules ####################################
#############################################################################



#' Generate association rules
#' @description The function \code{rules} generates all possible association rules.
#' The function demands the frequently occuring item sets and a user specified
#' minimum confidence.
#' @param itemsets Object of class FrequentSets
#' @param m_conf User specified minimum confidence
#' @return Returs an object of class \code{AssociationRules}
#' @export
#' @include classes_frequentsets.R classes_associationrules.R
#' @importFrom methods new
#' @importFrom Matrix colSums rowSums

rules <- function(itemsets, m_conf) {

  # check m_conf

  if (m_conf <= 0 || m_conf > 1) {
    stop("m_conf hast to be between 0 and 1")
  }


  # possible_rules returns list with lhs and rhs of all possible rules

  candidates <- possible_rules(itemsets)

  lhs <- candidates[[1]]
  rhs <- candidates[[2]]
  both <- lhs + rhs

  # pass matrices on to function map_sup to map each set to its
  # respective support

  supp_lhs <- apply(lhs, 1, map_sup, frqitm = itemsets)
  supp_rhs <- apply(rhs, 1, map_sup, frqitm = itemsets)
  supp_both <- apply(both, 1, map_sup, frqitm = itemsets)

  # divide support_both by support_lhs in order to receive confidence

  conf <- supp_both / supp_lhs

  # check which confidence is >= m_conf

  ind <- which(conf >= m_conf)

  # throw out rules with confidence below threshold

  lhs <- lhs[ind,]
  rhs <- rhs[ind,]

  # prepare content for object of class association rules

  supp_both <- supp_both[ind]
  supp_lhs <- supp_lhs[ind]
  supp_rhs <- supp_rhs[ind]
  conf <- conf[ind]

  # calculate lift

  lift <- supp_both / (supp_lhs * supp_rhs)

  measurements <- cbind(supp_both, conf, lift)

  a_rules <- new("AssociationRules",
                 antecedent = lhs,
                 consequent = rhs,
                 measurements = measurements,
                 items = itemsets@items,
                 m_values = c(itemsets@minsup, m_conf)
                 )

  return(a_rules)
}
