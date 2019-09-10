####### FREQUENT SETS #########
# In its current state, this function only returns only the sum of frequent itemsets.
# Once the necessary classes are established, this will be changed.
# Pretty slow


freq_items <- function(purchase, supp) {


  # Preparing data, much of this won't be necessary as soon as class for transaction
  # is properly integrated

  dat <- purchase@data
  item_names <- purchase@items

  n <- nrow(dat)
  supps <- colMeans(dat)
  sets <- which(supps >= supp)
  items <- item_names[sets]
  supps <- supps[which(supps >= supp)]
  # throwing out items out of the data that are not frequent as they will not
  # matter for larger itemsets

  dat <- dat[, sets]
  sets <- as.matrix(sets)
  set_mat <- diag(length(items))
  sets <- as.matrix(1:length(items))




  # save support of frequent items


  # create object of class frequentsets saving all relevant information

  itemsets <- new("frequentsets",
                 sets = as(set_mat, "ngCMatrix"),
                 support = supps,
                 items = items,
                 minsup = supp)

  # condition for while loop

  condition <- TRUE
  k <- 2

  if (nrow(set_mat) == 1) {
    return(itemsets)
  }

  while (condition) {

    # generate sets with gen_sets
    # as the method used will create sets with duplicate items, these sets have to
    # removed with remove_bad_sets

    sets <- gen_sets(sets)
    sets <- remove_bad_sets(sets)
    sets <- clean_sets(sets)
    n_set_mat <- set_to_matrix(sets, items)

    if (k > 2) {
      prod <- (n_set_mat * 1) %*% t(old_set_mat * 1)
      ind <- which(apply(prod, 1, function(a) {
        return(sum(a == k - 1) == k)
      }))

      n_set_mat <- n_set_mat[ind,]
      sets <- sets[ind,]

      if (is.vector(n_set_mat)) {
        n_set_mat <- t(as.matrix(n_set_mat)) * 1
        n_set_mat <- as(n_set_mat, "ngCMatrix")
        sets <- as.matrix(sets)
      }

    }


      # find the items that are in the sets
      # items that are not included in new sets will be saved in 'out'

      unique_items <- unique(as.vector(sets))
      out <- which(!is.element(1:length(items), unique_items))



      # if there are any items saved in out (which there probably will be for k > 3),
      # these can be temporarily excluded for the counting step. However, they will
      # still be needed later and therefore can be excluded completely.

      if (length(out) > 0) {
        count <- count_freq(n_set_mat[,-out], dat[,-out], k)
      } else {
        count <- count_freq(n_set_mat, dat, k)
      }

      # Throw out sets with support below minium

      sets <- prune(sets, count, supp, n)
      n_set_mat <- prune(n_set_mat, count, supp, n)

      sup <- (count / n)[count / n >= supp]


      # k + 1 for next iteration
      k <- k + 1
      # Append sets saved in this iteration to sets saved in previous iterations
      # (if there are any)
      # otherwise, condition is set to false, which stops the loop, as there are
      # no new frequent itemsets to be found
      if (is.vector(n_set_mat)) {
        n_set_mat <- t(as.matrix(n_set_mat)) * 1
        n_set_mat <- as(n_set_mat, "ngCMatrix")
        sets <- t(as.matrix(sets))
      }

      if (nrow(n_set_mat) > 0) {
        itemsets <- new("frequentsets",
                        sets = rbind(itemsets@sets, n_set_mat),
                       support = c(itemsets@support, sup),
                       items = items,
                       minsup = supp)

        old_set_mat <- n_set_mat
      } else {
        condition <- FALSE
      }
  }
  return(itemsets)
}






############# END

