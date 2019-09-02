

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

set_to_matrix <- function(set, len) {
  line <- rep(0, times = len)
  line[set] <- 1
  return(line)
}


# Example of application of the set_to_matrix function. It would probably make
# sense to wrap the set_to_matrix using apply

set_mat <- t(apply(
  test,
  FUN = set_to_matrix,
  names = colnames(transactions),
  MARGIN = 1
))




# 'clean_sets': Removes all duplicated sets from the set-matrix

clean_sets <- function(set_mat) {
  sets <- t(apply(
    set_mat,
    FUN = function(a)
      return(which(a == 1)),
    MARGIN = 1
  ))
return(sets)
}

# 'count_freq: counts the number of occurences of sets in the transaction matrix'

# currently unused version, should be explored further

#count_freq <- function(set, transactions) {
# freq <- sum(apply(
#    transactions,
#    FUN = function(a, set)
#      return(all(a >= set)),
#    MARGIN = 1,
#    set = set
#    )
#  )
#  return(freq)
#}

# Function currently used for counting occurences of itemsets

count_freq <- function(set_mat, dat, k) {

  # Each line of set_mat is multiplied with each column transposed
  # transaction matrix. If the product equals k (number of items
  # in the set), the set occurs in that transaction. The function
  # in the apply wrapper returns a boolean values indicating whether
  # this is the case or not and sums them up. The function then returns
  # how often each itemset occured in the transaction matrix.

  count <- apply(
    set_mat,
    FUN = function(a, b)
      return(sum(a %*% t(b) == k)),
    b = dat,
    MARGIN = 1
  )
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
    new_sets <-
      cbind(apply(
        sets,
        FUN = rep,
        MARGIN = 2,
        each = length(un_item)),
      rep(un_item, times = nrow(sets)))
    return(new_sets)
  }
}

# 'remove_bad_sets': Currently, the function gen_sets generates sets where one item
# may occur more than once. This function throws these sets out.

remove_bad_sets <- function(sets) {
  out <- apply(
      sets,
      FUN = function(a)
        return(any(duplicated(a))),
      MARGIN = 1
    )
  if (any(out)) {
    sets <- sets[-which(out),]
  }
  return(sets)
}


