

################# Count individual item frequencies ###########################

item_count <- function(dat){
  count <- Matrix::colSums(transactions)
  return(count)
}


# 'is_in' Check whether a set is already in a matrix of sets
# currently not used, most likely unnecessary

is_in <- function(mat, new_set){
  if(nrow(mat) == 0){return(FALSE)}
  bool <- apply(mat, FUN = function(a) all(is.element(new_set, a)), MARGIN = 1)
  return(bool)
}

# 'set_to_matrix': Transform a set of items (vector of strings)
# into a vector of 0s and 1s

set_to_matrix <- function(sets, items) {
  set_mat <- t(sparseMatrix(i = as.vector(t(sets)),
                            j = rep(1:nrow(sets), each = ncol(sets)),
                            dims = c(length(items), nrow(sets))))
  return(set_mat)
}



# 'clean_sets': Removes all duplicated sets from the set-matrix
clean_sets <- function(sets) {
  sets <- t(apply(sets, 1, sort))
  sets <- unique(sets)
  return(sets)
}



# Function currently used for counting occurences of itemsets

count_freq <- function(set_mat, dat, k) {

  # Each line of set_mat is multiplied with each column transposed
  # transaction matrix. If the product equals k (number of items
  # in the set), the set occurs in that transaction. The function
  # in the apply wrapper returns a boolean values indicating whether
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
    sets <- sets[-out,]
    return(sets)
  } else {
    return(sets)
  }
}


# 'gen_sets': Generates sets of size k from sets of size k-1.
# Approach for sets with k > 2 probably could be much more efficient
# In the current state it creates many more sets than necessary, however,
# it performs quickly enough for now

gen_sets <- function(sets) {
  if (ncol(sets) == 1){
    hlp <- combn(1:nrow(sets), 2)
    new_sets <- matrix(as.vector(sets)[hlp], ncol = 2, byrow = TRUE)
    return(new_sets)
  } else {
    un_item <- unique(as.vector(sets))
    new_sets <- cbind(apply(sets,
                            FUN = rep,
                            MARGIN = 2,
                            each = length(un_item)),
                      rep(un_item, times = nrow(sets)))
    return(new_sets)
  }
}

# gen_sets produces sets containing the same item more than once, this funtion
# throws these sets out

remove_bad_sets <- function(sets) {
  out <- apply(sets,
               FUN = function(a) return(any(duplicated(a))),
               MARGIN = 1
    )
  if (any(out)) {
    sets <- sets[-which(out),]
  }
  return(sets)
}


