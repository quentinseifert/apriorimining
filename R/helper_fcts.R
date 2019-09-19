##########################################################################
##################### freq_items helper functions ########################
##########################################################################

#' @include classes_transactiondata.R classes_frequentsets.R classes_associationrules.R
#' @import methods
#' @import Matrix
#' @import utils



generate_sets <- function(sets, k) {
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
                      rep(un_item, times = nrow(sets)))
  }


  out <- apply(new_sets,
               FUN = function(a) return(any(duplicated(a))),
               MARGIN = 1
  )


  if (any(out)) {
    new_sets <- new_sets[-which(out),]
  }


  new_sets <- t(apply(new_sets, 1, sort))
  new_sets <- unique(new_sets)


  cols <- ncol(sets)
  rows <- nrow(new_sets)

  new_sets <- t(sparseMatrix(i = as.vector(t(new_sets)),
                             j = rep(1:rows, each = ncol(new_sets)),
                             dims = c(cols, rows)
  )
  )

  if (k > 2) {
    prod <- (new_sets * 1) %*% t(sets * 1)
    ind <- which(apply(prod, 1, function(a) {
      return(sum(a == k - 1) == k)
    }))

    new_sets <- new_sets[ind,, drop = FALSE]
  }
  return(new_sets)
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
    sets <- sets[-out,, drop = FALSE]
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


#########################################################################
######################### Rules helper functions ########################
#########################################################################


possible_rules <- function(itemsets, k = 1) {
  sets <- itemsets@sets
  sums <- unique(rowSums(sets))
  cols <- ncol(sets)

  lhs <- t(sparseMatrix(i = {}, j = {}, dims = c(ncol(sets), 0)))
  rhs <- t(sparseMatrix(i = {}, j = {}, dims = c(ncol(sets), 0)))

  vecs <- apply(itemsets@sets,
                FUN = which,
                MARGIN = 1
  )

  for (i in 2:max(sums)) {

    sub_set <- sets[rowSums(sets) == i,]
    if (is.vector(sub_set)) {
      sub_set <- t(as.matrix(sub_set) * 1)
      sub_set <- as(sub_set, "ngCMatrix")
    }
    ind <- t(apply(sub_set,
                   FUN = which,
                   MARGIN = 1))

    index_lhs <- matrix(nrow = 0, ncol = i - 1)
    index_rhs <- NULL

    for (j in 1:nrow(sub_set)) {

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
  ind <- which(apply(sets, 1,
                     FUN = function(a, b) {
                       all(a == b)
                     }, b = mat))

  sup <- frqitm@supports[ind]
  return(sup)
}


map_support <- function(mat, freq) {
  sums <- unique(rowSums(mat))

  for (i in sums) {
    prod <- (mat[which(rowSums(mat) == i),] * 1) %*%
      (t(freq@sets[which(rowSums(freq@sets) == i),]) * 1)

    ind <- apply(prod, 1, FUN = function(a) which(a == i))
    if (i == 1) {
      support <- freq@support[ind]
    } else {
      support <- c(support, freq@support[ind])
    }
  }
  return(support)
}

