####### Function 'rules': Determines association rules


rules <- function(itemsets, support, confidence) {

  # possible_rules returns list with lhs and rhs of all possible rules

  candidates <- possible_rules(itemsets)

  lhs <- candidates[[1]]
  rhs <- candidates[[2]]
  both <- lhs + rhs

  # pass matrices on to function map_sup to map each set to its
  # respective support

  support_lhs <- apply(lhs, 1, map_sup, b = itemsets)
  support_rhs <- apply(rhs, 1, map_sup, b = itemsets)
  support_both <- apply(both, 1, map_sup, b = itemsets)

  # divide support_both by support_lhs in order to receive confidence

  conf <- support_both / support_lhs

  # check which confidence is >= minconfidence

  ind <- which(conf >= confidence)

  # throw out rules with confidence below threshold

  lhs <- lhs[ind,]
  rhs <- rhs[ind,]


}
