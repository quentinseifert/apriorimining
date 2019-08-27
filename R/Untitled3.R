freq_sets <- function(dat, support) {
  k <- 1
  n <- nrow(dat)
  print("CHECK 1!")


  # Find the frequent single items
  sets <- as.matrix(colnames(dat)[which(colMeans(dat) >= 0.01)])
  sumsets <- nrow(sets)


  #Throw out columns of not frequent items
  out <- which(!is.element(colnames(dat), sets))
  dat <- dat[, -out]

  # use combn to generate pairs
  # can be replaced with 'gen_sets'

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
  count <- count_freq(set_mat, dat, k)
  print("CHECK 3!")


  # throw out sets with support below minimal support
  sets <- prune(sets, count, support, n)
  sumsets <- sumsets + nrow(sets)
  print("CHECK 4!")

  #### throw out cols of items that are no longer needed
  items <- unique(as.vector(sets))
  out <- which(!is.element(colnames(dat), items))
  dat <- dat[,-out]

  # set condition true
  # generate k+1 sets until no new sets can be generated

  condition <- TRUE

  while (condition) {
    sets <- gen_sets(sets)
    sets <- remove_bad_sets(sets)
    print("CHECK 5!")
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
      sets <- clean_sets(sets, set_mat, dat)

      # count frequencies and throw out sets below the minimum support

      count <- count_freq(set_mat, dat, k)

      print(sumsets)
      sets <- prune(sets, count, support, n)

      sumsets <- sumsets + nrow(sets)
    } else {
      condition <- FALSE
    }
  }
  return(sumsets)
}
