####### FREQUENT SETS #########

freq_sets <- function(data, support) {
  k <- 1
  n <- nrow(dat)
  print("CHECK 1!")
  # Find the frequent single items
  sets <- as.matrix(colnames(data)[which(colMeans(data) >= support)])
  sumsets <- nrow(sets)
  # use combn to generate pairs
  hlp <- combn(1:nrow(sets), 2)
  sets <- matrix(as.vector(sets)[hlp], ncol = 2, byrow = TRUE)
  k <- 2
  # convert sets into matrix representation using set_to_matrix
  set_mat <-  t(apply(
    sets,
    FUN = set_to_matrix,
    names = colnames(dat),
    MARGIN = 1
  ))

  # count frequencies of sets
  print("CHECK 2!")
  count <- count_freq(set_mat, data, k)
  print("CHECK 3!")
  # throw out sets with support below minimal support
  sets <- prune(sets, count, support, n)
  sumsets <- sumsets + nrow(sets)
  # set condition true
  # generate k+1 sets until no new sets can be generated
  condition <- TRUE

  while (condition) {
    sets <- gen_sets(sets)
    sets <- remove_bad_sets(sets)

    if (nrow(sets) != 0) {
      k <- k + 1
      set_mat <-  t(apply(
        sets,
        FUN = set_to_matrix,
        names = colnames(dat),
        MARGIN = 1
      ))

      # The method which is currently used will create duplicate sets
      # This step purges the sets
      set_mat <- unique(set_mat)
      sets <- clean_sets(sets, set_mat)

      # count frequencies and throw out sets below the minimum support
      print("CHECK!!")
      count <- count_freq(set_mat, data, k)
      print("CHECKCHECK!!")
      print(sumsets)
      sets <- prune(sets, count, support, n)
      print("CHECKCHECK!!!")
      sumsets <- sumsets + nrow(sets)
    } else {
      condition <- FALSE
    }
  }
  return(sumsets)
}



number <- freq_sets(data = dat, support = 0.01)


number



library(arules)
data("Groceries")
data <- as(Groceries, "matrix")
support <- 0.01


k <- 1
n <- nrow(dat)
print("CHECK 1!")
# Find the frequent single items
sets <- as.matrix(colnames(data)[which(colMeans(data) >= support)])
sumsets <- nrow(sets)
# use combn to generate pairs
hlp <- combn(1:nrow(sets), 2)
sets2 <- matrix(as.vector(sets)[hlp], ncol = 2, byrow = TRUE)
k <- 2
# convert sets into matrix representation using set_to_matrix
set_mat2 <-  t(apply(
  sets2,
  FUN = set_to_matrix,
  names = colnames(dat),
  MARGIN = 1
))

# count frequencies of sets
print("CHECK 2!")
count <- count_freq(set_mat2, data, k)
print("CHECK 3!")
# throw out sets with support below minimal support
sets2 <- prune(sets2, count, support, n)
sumsets <- sumsets + nrow(sets2)
# set condition true
# generate k+1 sets until no new sets can be generated
condition <- TRUE

while (condition) {
  sets3 <- gen_sets(sets2)
  sets3 <- remove_bad_sets(sets3)
  dim(sets3)
  if (nrow(sets) != 0) {
    k <- k + 1
    set_mat3 <-  t(apply(
      sets3,
      FUN = set_to_matrix,
      names = colnames(dat),
      MARGIN = 1
    ))
    dim(set_mat3)
    # The method which is currently used will create duplicate sets
    # This step purges the sets
    set_mat3 <- unique(set_mat3)
    sets3 <- clean_sets(sets3, set_mat3)
    dim(sets3)
    # count frequencies and throw out sets below the minimum support
    print("CHECK!!")
    count <- count_freq(set_mat3, data, 3)
    print("CHECKCHECK!!")
    print(sumsets)
    sets3 <- prune(sets3, count, support, n)
    dim(sets3)


    if (!all(count / nrow(dat) >= support)){
      out <- which((count / nrow(dat)) < support)
      sets <- sets[-out,]
      return(sets)
    } else {
      return(sets)
    }




y <- t(as(set_mat3, "ngCMatrix"))
z <- t(as(data, "ngCMatrix"))
countyz <- count_freq(set_mat = y, transactions = z, k = 2)
dim(z)

countyz <- apply(
  y,
  FUN = function(a, b)
    return(sum(a %*% t(b) == k)),
  b = z,
  MARGIN = 2
)

as.matrix(y[, 1]) %*% z
sum(t(as.matrix(y[,1])) %*% z == 2)

dim(set_mat3)

apply(
  y,
  FUN = function(a, b)
    return(sum(t(a) %*% b == 2)),
  b = z,
  MARGIN = 2
)

dim(set_mat3)
