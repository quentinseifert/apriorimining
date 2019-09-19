context("Test possible rules")

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
