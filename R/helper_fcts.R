##########################################################################
########################### helper functions #############################
##########################################################################

#' @include classes_transactiondata.R classes_frequentsets.R classes_associationrules.R
#' @import methods
#' @importFrom Matrix rowSums colSums sparseMatrix
#' @importFrom utils combn



generate_sets <- function(sets, k) {

  # find positions of items in matrix and generate all possible combinations

  ind <- t(apply(sets, 1, which))
  if (k == 2) {
    ind <- as.matrix(ind)
    new_sets <- t(combn(1:nrow(sets), 2))
  } else {
    un_item <- unique(as.vector(ind))
    new_sets <- cbind(apply(ind,
                            FUN = rep,
                            MARGIN = 2,
                            each = length(un_item)),
                      rep(un_item, times = nrow(sets))
                      )
  }

  # throw out item sets in which the same item occurs more than once

  out <- apply(new_sets,
               FUN = function(a) return(any(duplicated(a))),
               MARGIN = 1
               )

  if (any(out)) {
    new_sets <- new_sets[-which(out),, drop = FALSE]
  }

  # throw out duplicated item sets

  new_sets <- t(apply(new_sets, 1, sort))
  new_sets <- unique(new_sets)

  cols <- ncol(sets)
  rows <- nrow(new_sets)

  new_sets <- t(sparseMatrix(i = as.vector(t(new_sets)),
                             j = rep(1:rows, each = ncol(new_sets)),
                             dims = c(cols, rows)
                             )
                )

  # Multiply old and new set matrices in order to sort out item sets that
  # have non-frequent subsets. Each item set of size k has k subsets of
  # size k - 1. If a new set of size k consists of only frequent subsets,
  # the respective line of the product matrix should have k elements which
  # equal k - 1.

  if (k > 2) {
    prod <- (new_sets * 1) %*% t(sets * 1)
    ind <- which(apply(prod, 1, function(a) return(sum(a == k - 1) == k)))
    new_sets <- new_sets[ind,, drop = FALSE]
  }
  return(new_sets)
}






# Function currently used for counting occurences of itemsets

count_freq <- function(set_mat, dat, k) {

  # Each line of set_mat is multiplied with each column transposed
  # transaction matrix. If the product equals k (number of items
  # in the set), the set occurs in that transaction. The function
  # in the apply wrapper returns boolean values indicating whether
  # this is the case or not and sums them up. The function then returns
  # how often each itemset occured in the transaction matrix.

  set_mat <- set_mat * 1
  dat <- dat * 1

  count <- rowSums(set_mat %*% t(dat) == k)

  return(count)
}


# 'prune': Throws out itemsets below the minimum support based on the return
# value of count

prune <- function(sets, count, support, n) {

  # If all supports of the itemsets are equal or greater than the minimum support,
  # the function returns the sets unchanged. Otherwise it selects the sets for which
  # this is not the case and throws them out

  if (!all(count / n >= support)){
    out <- which((count / n) < support)
    sets <- sets[-out,, drop = FALSE]
    return(sets)
  } else {
    return(sets)
  }
}




#########################################################################
######################### Rules helper functions ########################
#########################################################################



possible_rules <- function(itemsets, k = 1) {

  # extract set matrix, set sizes and number of items

  sets <- itemsets@sets
  sums <- unique(rowSums(sets))
  cols <- ncol(sets)

  # create empty matrices for antecedents and consequents

  lhs <- t(sparseMatrix(i = {}, j = {}, dims = c(ncol(sets), 0)))
  rhs <- t(sparseMatrix(i = {}, j = {}, dims = c(ncol(sets), 0)))

  # find indices of itemsets

  vecs <- apply(itemsets@sets, 1, which)


  # loop through set sizes and create all possible combinations for each set
  # where the antecedent has the size k - 1 and the consequent has the size 1

  for (i in 2:max(sums)) {
    sub_set <- sets[rowSums(sets) == i,]

    if (is.vector(sub_set)) {
      sub_set <- t(as.matrix(sub_set) * 1)
      sub_set <- as(sub_set, "ngCMatrix")
    }

    ind <- t(apply(sub_set, 1, which))

    index_lhs <- matrix(nrow = 0, ncol = i - 1)
    index_rhs <- NULL

    for (j in 1:nrow(sub_set)) {

      # for each itemsets find all combinations of size k - 1, save those as tmp_lhs
      # save consequent as tmp_rhs
      # bind both to index_lhs/index_rhs

      tmp_lhs <- t(combn(ind[j,], m = i - 1))


      index <- apply(tmp_lhs,
                     FUN = function(a, b) {
                       which(!is.element(b, a))},
                     MARGIN = 1,
                     b = ind[j,])

      tmp_rhs <- ind[j,][index]

      index_lhs <- rbind(index_lhs, tmp_lhs)
      index_rhs <- c(index_rhs, tmp_rhs)

    }
    rows <- nrow(index_lhs)

    # use index_lhs and index_rhs to create new sparse matrices and bind them to
    # lhs/rhs

    new_lhs <- t(sparseMatrix(i = as.vector(t(index_lhs)),
                              j = rep(1:rows, each = ncol(index_lhs)),
                              dims = c(cols, rows)))

    new_rhs <- t(sparseMatrix(i = as.vector(t(index_rhs)),
                              j = 1:rows,
                              dims = c(cols, rows)))

    lhs <- rbind(lhs, new_lhs)
    rhs <- rbind(rhs, new_rhs)
  }
  return(list(lhs, rhs))
}



map_sup <- function(mat, frqitm) {
  sets <- frqitm@sets
  ind <- which(apply(sets, 1,FUN = function(a, b) all(a == b), b = mat))
  sup <- frqitm@supports[ind]
  return(sup)
}




