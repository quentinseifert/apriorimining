####### Function 'rules': Determines association rules


rules <- function(itemsets, support, confidence) {

  # possible_rules returns list with lhs and rhs of all possible rules

  candidates <- possible_rules(itemsets)

  lhs <- candidates[[1]]
  rhs <- candidates[[2]]
  both <- lhs + rhs

  # pass matrices on to function map_sup to map each set to its
  # respective support

  supp_lhs <- apply(lhs, 1, map_sup, b = itemsets)
  supp_rhs <- apply(rhs, 1, map_sup, b = itemsets)
  supp_both <- apply(both, 1, map_sup, b = itemsets)

  # divide support_both by support_lhs in order to receive confidence

  conf <- supp_both / supp_lhs

  # check which confidence is >= minconfidence

  ind <- which(conf >= confidence)

  # throw out rules with confidence below threshold

  lhs <- lhs[ind,]
  rhs <- rhs[ind,]

  # prepare content for object of class ass_rules

  supp_both <- supp_both[ind]
  supp_lhs <- supp_lhs[ind]
  supp_rhs <- supp_rhs[ind]
  conf <- conf[ind]

  # calculate lift

  lift <- supp_both / (supp_lhs * supp_rhs)

  measurements <- cbind(supp_both, conf, lift)

  ass_rules <- new("associationrules",
                   antecedent = lhs,
                   consequent = rhs,
                   measurement = measurements,
                   items = itemsets@items)

  return(ass_rules)

}
