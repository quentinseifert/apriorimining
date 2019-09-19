#############################################################################
######################## Function: freq_items ###############################
#############################################################################



#' Find frequent itemsets
#' @description \code{freq_item} finds every frequent itemset depending
#' on the chosen minimum support. The function demands a transactionsmatrix
#' and a user specified minimum support.
#' @param input Binary matrix containing transaction data, with rows
#' representing transactions and columns representing items. Can be
#' either logical or numeric, every value has to be either 0 / 1 or
#' FALSE / TRUE (0 or FALSE if item is not bought). Columns should be
#' named. Also takes an object class \code{transactiondata}.
#' @param m_sup User specified minimum support
#' @return Returns an object of class \code{frequentsets}
#' @export
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
  sets <- as(diag(length(items)), "ngCMatrix")





  # save support of frequent items


  # create object of class frequentsets saving all relevant information

  itemsets <- new("frequentsets",
                  sets = as(sets, "ngCMatrix"),
                  supports = supports,
                  items = items,
                  minsup = m_sup)

  # condition for while loop

  condition <- TRUE
  k <- 2

  if (nrow(sets) <= 1) {
    return(itemsets)
  }

  while (condition) {

    # generate sets with gen_sets

    sets <- generate_sets(sets, k)


    # find the items that are in the sets
    # items that are not included in new sets will be saved in 'out'

    unique_items <- which(colSums(sets) > 0)
    out <- which(!is.element(1:length(items), unique_items))



    # if there are any items saved in out (which there probably will be for k > 3),
    # these can be temporarily excluded for the counting step. However, they will
    # still be needed later and therefore can be excluded completely.

    if (length(out) > 0) {
      count <- count_freq(sets[,-out], dat[,-out], k)
    } else {
      count <- count_freq(sets, dat, k)
    }

    # Throw out sets with support below minium

    sets <- prune(sets, count, m_sup, n)
    sup <- (count / n)[count / n >= m_sup]


    # k + 1 for next iteration
    k <- k + 1
    # Append sets saved in this iteration to sets saved in previous iterations
    # (if there are any)
    # otherwise, condition is set to false, which stops the loop, as there are
    # no new frequent itemsets to be found


    if (nrow(sets) > 0) {
      itemsets <- new("frequentsets",
                      sets = rbind(itemsets@sets, sets),
                      supports = c(itemsets@supports, sup),
                      items = items,
                      minsup = m_sup)

    } else {
      condition <- FALSE
    }
  }
  return(itemsets)
}






############# END

