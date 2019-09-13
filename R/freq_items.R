#############################################################################
######################## Function: freq_items ###############################
#############################################################################



#' Find frequent itemsets
#' @description Demands transactionsmatrix and minimum support.
#' \code{freq_item} finds every frequent itemset depending
#' on the chosen minimum support.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE. (0 / FALSE if item is not bought). Columns should be
#' named. Also takes an object class \emph{transactiondata}-
#' @param m_sup User specified minimum support
#' @return Returns an object of class \emph{frequentsets}
#' @export
#' @import Matrix
#' @include classes_frequentsets.R classes_transactiondata.R


freq_items <- function(input, m_sup) {


  # Preparing data, much of this won't be necessary as soon as class for transaction
  # is properly integrated

  dat <- input@data
  item_names <- input@items

  n <- nrow(dat)
  supports <- colMeans(dat)
  sets <- which(supports >= m_sup)
  items <- item_names[sets]
  supports <- supports[which(supports >= m_sup)]
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
                 supports = supports,
                 items = items,
                 minsup = m_sup)

  # condition for while loop

  condition <- TRUE
  k <- 2

  if (nrow(set_mat) == 1 || nrow(set_mat) == 0) {
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

      sets <- prune(sets, count, m_sup, n)
      n_set_mat <- prune(n_set_mat, count, m_sup, n)

      sup <- (count / n)[count / n >= m_sup]


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
                       supports = c(itemsets@supports, sup),
                       items = items,
                       minsup = m_sup)

        old_set_mat <- n_set_mat
      } else {
        condition <- FALSE
      }
  }
  return(itemsets)
}






############# END

