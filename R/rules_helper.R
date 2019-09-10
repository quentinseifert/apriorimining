######### Find candidates for Rules

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

  sup <- frqitm@support[ind]
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





