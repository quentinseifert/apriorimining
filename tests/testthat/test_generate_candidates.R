

context("Test internal function generate_sets")

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


