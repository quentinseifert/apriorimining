####### FREQUENT SETS #########
# In its current state, this function only returns only the sum of frequent itemsets.
# Once the necessary classes are established, this will be changed.
# Pretty slow


freq_items <- function(dat, supp) {


  n <- nrow(dat)
  sets <- which(colMeans(dat) >= supp)
  items <- colnames(dat)[sets]
  names(sets) <- NULL
  dat <- dat[, sets]
  sets <- as.matrix(sets)
  colnames(dat) <- NULL
  set_mat <- diag(length(items))
  sets <- as.matrix(1:length(items))

  k <- 2

  sup <- colMeans(dat)

  itmsets <- new("itmsets",
                 sets = as(set_mat, "ngCMatrix"),
                 support = sup,
                 items = items)

  condition <- TRUE

  while (condition) {

    sets <- gen_sets(sets)
    sets <- remove_bad_sets(sets)

    if (nrow(sets) > 0) {
      unique_items <- unique(as.vector(sets))
      out <- which(!is.element(1:length(items), unique_items))
      set_mat <- t(apply(
        sets,
        FUN = set_to_matrix,
        len = length(items),
        MARGIN = 1
      ))


      set_mat <- unique(set_mat)
      sets <- clean_sets(set_mat)
      set_mat <- as(set_mat, "ngCMatrix")


      if (length(out) > 0) {
        count <- count_freq(set_mat[,-out], dat[,-out], k)
      } else {
        count <- count_freq(set_mat, dat, k)
      }
      sets <- prune(sets, count, supp, n)
      set_mat <- prune(set_mat, count, supp, n)
      set_mat <- as(set_mat, "ngCMatrix")
      k <- k + 1
      sup <- (count / n)[count / n >= 0.01]


      itmsets <- new("itmsets",
                     sets = rbind(itmsets@sets, set_mat),
                     support = c(itmsets@support, sup),
                     items = items)

    } else {
      condition <- FALSE
    }
  }
  return(itmsets)
}






############# END

