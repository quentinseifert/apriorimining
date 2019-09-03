####### FREQUENT SETS #########
# In its current state, this function only returns only the sum of frequent itemsets.
# Once the necessary classes are established, this will be changed.
# Pretty slow


freq_items <- function(dat, supp) {


  # Preparing data, much of this won't be necessary as soon as class for transaction
  # is properly integrated

  n <- nrow(dat)
  sets <- which(colMeans(dat) >= supp)
  items <- colnames(dat)[sets]
  names(sets) <- NULL

  # throwing out items out of the data that are not frequent as they will not matter
  # for larger itemsets

  dat <- dat[, sets]
  sets <- as.matrix(sets)
  colnames(dat) <- NULL
  set_mat <- diag(length(items))
  sets <- as.matrix(1:length(items))


  k <- 2

  # save support of frequent items

  sup <- colMeans(dat)

  # create object of class itmsets saving all relevant information

  itmsets <- new("itmsets",
                 sets = as(set_mat, "ngCMatrix"),
                 support = sup,
                 items = items)

  # condition for while loop

  condition <- TRUE

  while (condition) {

    # generate sets with gen_sets
    # as the method used will create sets with duplicate items, these sets have to
    # removed with remove_bad_sets

    sets <- gen_sets(sets)
    sets <- remove_bad_sets(sets)

    if (nrow(sets) > 0) {

      # find the items that are in the sets
      # items that are not included in new sets will be saved in 'out'

      unique_items <- unique(as.vector(sets))
      out <- which(!is.element(1:length(items), unique_items))

      set_mat <- t(apply(sets,
                         FUN = set_to_matrix,
                         len = length(items),
                         MARGIN = 1))


      set_mat <- unique(set_mat)
      sets <- clean_sets(set_mat)
      set_mat <- as(set_mat, "ngCMatrix")


      # if there are any items saved in out (which there probably will be for k > 3),
      # these can be temporarily excluded for the counting step. However, they will
      # still be needed later and therefore can be excluded completely.

      if (length(out) > 0) {
        count <- count_freq(set_mat[,-out], dat[,-out], k)
      } else {
        count <- count_freq(set_mat, dat, k)
      }

      # Throw out sets with support below minium

      sets <- prune(sets, count, supp, n)
      set_mat <- prune(set_mat, count, supp, n)
      set_mat <- as(set_mat, "ngCMatrix")
      k <- k + 1
      sup <- (count / n)[count / n >= 0.01]

      # Append sets saved in this iteration to sets saved in previous iterations
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

