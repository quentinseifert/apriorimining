

context("Test internal functions")

test_that(
  "Test case: Single itemset",
  {
    # generate sets receives single itemset. It should return a matrix with 0 rules,
    # no new larger itemsets can be generated from one itemset.
    set <- matrix(c(TRUE, FALSE, FALSE, TRUE, FALSE), nrow = 1)
    new_sets <- generate_sets(set, k = 3)

    expect_equal(nrow(new_sets), 0)
  })

test_that(
  "Test case: Two sets, no legitimate combinations possible",
  {
    sets <- matrix(c(TRUE, FALSE, FALSE, FALSE, TRUE,
                     FALSE, TRUE, FALSE, TRUE, FALSE), nrow = 2, byrow = TRUE)

    # generate_sets receives two sets of size k = 2. There are possible combination for
    # sets of size k = 3, however, these sets would not consist of frequent subsets of
    # size k - 1. Therefore, generate_sets should filter out the sets consisting of
    # non-frequent subsets out and return an empty matrix.

    new_sets <- generate_sets(sets, k = 3)

    expect_equal(nrow(new_sets), 0)
  }
)

test_that(
  "Test case: Example from paper",
  {
    sets <- matrix(c(TRUE, TRUE, FALSE, FALSE, FALSE,
                     TRUE, FALSE, TRUE, FALSE, FALSE,
                     TRUE, FALSE, FALSE, TRUE, FALSE,
                     FALSE, TRUE, TRUE, FALSE, FALSE), nrow = 4, byrow = TRUE)

    new_sets <- generate_sets(sets, k = 3)


    expect <- matrix(c(1, 1, 1, 0, 0), nrow = 1)
    expect_true(all(new_sets == expect))
  }
)




# Data
mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1, 0,
                1, 1, 1, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 1, 0, 1, 0, 1, 0, 0, 1,
                0, 1, 1, 0, 1, 0, 0, 0, 0, 1,
                0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
                1, 0, 0, 1, 0, 0, 0, 1, 1, 1), nrow = 10, byrow = FALSE)

colnames(mat) <- c("Bananas", "Bread", "Butter", "Sugar", "Diapers", "Cheese")
mat <- create_transaction(mat)

itemsets <- freq_items(mat, 0.1)

test_that(
  "Test possible_rules",
  {
    p_rules <- possible_rules(itemsets)
    antecedent <- p_rules[[1]]
    consequent <- p_rules[[2]]
    both <- antecedent + consequent

    sums <- unique(rowSums(itemsets@sets))[-1]

    occurences <- sapply(sums,
                         FUN = function(a) {
                           return(sum(rowSums(itemsets@sets) == a))}
    )

    for (i in 1:length(sums)) {
      expect_equal(sum(rowSums(both) == sums[i]), sums[i] * occurences[i])
    }

  }
)






