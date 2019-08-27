

################# Count individual item frequencies ###########################
item_count <- function(transactions){
  count <- Matrix::colSums(transactions)
  return(count)
}


################ Check whether a set is already in a matrix of sets ###########
is_in <- function(mat, new_set){
  if(nrow(mat) == 0){return(FALSE)}
  bool <- apply(mat, FUN = function(a) all(is.element(new_set, a)), MARGIN = 1)
  return(bool)
}
#######



set_to_matrix <- function(set, names){
  line <- rep(0, length(names))
  for (i in 1:length(set)){
    for (j in 1:length(names)){
      if (names[j] == set[i]){
        line[j] <- 1
      }
    }
  }
  return(line)
}
x <- c("Bananas", "Milk")


set_to_matrix <- function(set, names){
  line <- rep(0, length(names))
  for (i in 1:length(set)){
    for (j in 1:length(names)){
      if (names[j] == set[i]){
        line[j] <- 1
      }
    }
  }
  return(line)
}




set_mat <- t(apply(
  test,
  FUN = set_to_matrix,
  names = colnames(transactions),
  MARGIN = 1
))



prune()




sets <- get_sets(as.matrix(candidates))
set_mat <- t(apply(
  sets,
  FUN = set_to_matrix,
  names = colnames(transactions),
  MARGIN = 1
))

prune(set_mat, transactions, 0.1)




count_freq <- function(set, transactions) {
  freq <- sum(apply(
    transactions,
    FUN = function(a, set)
      return(all(a >= set)),
    MARGIN = 1,
    set = set
    )
  )
  return(freq)
}
set_mat2[1,]
count
count2 <- apply(set_mat3, FUN = count_freq, MARGIN = 1, transactions = dat)
count == count2


is_unique <- function(set, all_sets) {

}



clean_sets <- function(names, set_mat) {
  index <- which(as.vector(unique(set_mat)) == 1)
  y <- rep(colnames(dat), times = nrow(unique(set_mat)))
  sets <- matrix(y[index], nrow = nrow(unique(set_mat)), byrow = TRUE)
  return(sets)
}

dim(clean_sets(colnames(dat), unique(set_mat3)))




index <- which(as.vector(unique(set_mat3)) == 1)
y <- rep(colnames(dat), times = nrow(unique(set_mat3)))
sets3_1 <- matrix(y[index], nrow = nrow(unique(set_mat3)), byrow = TRUE)
dim(sets3_1)


count_freq <- function(set_mat, transactions, k) {
  count <- apply(
    set_mat,
    FUN = function(a, b)
      return(sum(a %*% t(b) == k)),
    b = dat,
    MARGIN = 1
  )
  return(count)
}

prune <- function(sets, count, support, n) {
  if (!all(count / nrow(dat) >= support)){
    out <- which((count / n) < support)
    sets <- sets[-out,]
    return(sets)
  } else {
    return(sets)
  }
}

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

remove_bad_sets <- function(sets) {
  out <- apply(
      sets,
      FUN = function(a)
        return(any(duplicated(a))),
      MARGIN = 1
    )
  sets <- sets[-which(out),]
  return(sets)
}


