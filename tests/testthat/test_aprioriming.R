###############################################################################
######################### Comparison to arules ################################
###############################################################################

# In this files we test whether our implementation yields the same results as
# arules

context("Comparison to arules")



data("Groceries")
groc <- Groceries@data
colnames(groc) <- Groceries@items
library(arules)


rules_1 <- apriorimining(groc, 0.01, 0.3)

rules_2 <- apriori(t(groc), parameter = list(support = 0.01,
                                             confidence = 0.3,
                                             minlen = 2),
                   control = list(verbose = FALSE))



test_that("test if both packages find the same amount of rules",
  {

  expect_equal(nrow(rules_1@antecedent), nrow(rules_2@lhs))
  expect_equal
  }
)





test_that(
  "same quality measurements",
          {
          sorted_sup1 <- sort(rules_1@measurements[,1])
          sorted_conf1 <- sort((rules_1@measurements[,2]))
          sorted_lift1 <- sort((rules_1@measurements[,3]))

          sorted_sup2 <- sort(rules_2@quality[,1])
          sorted_conf2 <- sort((rules_2@quality[,2]))
          sorted_lift2 <- sort((rules_2@quality[,3]))

          expect_equal(sorted_sup1, sorted_sup2)
          expect_equal(sorted_conf1, sorted_conf2)
          expect_equal(sorted_lift1, sorted_lift2)
          }
)





# Both



test_that("same rules",
          {

            lhs <- t(rules_2@lhs@data)
            rhs <- t(rules_2@rhs@data)

            lhs <- lhs[, -which(colSums(lhs) == 0)]
            rhs <- rhs[, -which(colSums(rhs) == 0)]

            ind1 <- order(rules_2@quality[,3])

            lhs <- lhs[ind1,]
            rhs <- rhs[ind1,]


            antecedent <- rules_1@antecedent
            consequent <- rules_1@consequent

            antecedent <- antecedent[, -which(colSums(antecedent) == 0)]
            consequent <- consequent[, -which(colSums(consequent) == 0)]

            ind2 <- order(rules_1@measurements[,3])

            antecedent <- antecedent[ind2,]
            consequent <- consequent[ind2,]

           expect_equal(antecedent, lhs)
           expect_equal(consequent, rhs)

          }
)

length(unique(rules_1@measurements[,3]))
nrow(rules_1@antecedent)




test_that("same frequent itemsets as in reference package",
          {
            # get a set of rules from arules::apriori,
            # confidence = 0 so all possible rules are included
            arules <- apriori(t(groc), parameter = list(support = 0.05,
                                                     confidence = 0),
                              control = list(verbose = FALSE))


            # adding lhs@data rhs@data yields matrix of frequent itemsets
            sets <- t(as(arules@lhs@data + arules@rhs@data, "matrix"))


            # sort and throw out duplicate rows
            ind <- order(arules@quality[, 1])
            sets <- sets[ind, ]
            sets <- unique(sets)

            # find sets using own function with same m_sup
            freqsets <- freq_items(Groceries, 0.05)
            setmat <- as(freqsets@sets, "matrix") * 1


            # sort
            ind <- order(freqsets@supports)
            setmat <- setmat[ind, ]


            # after throwing out unnecessary columns, both matrices should
            # be identical
            expect_true(all(sets[, -which(colSums(sets) == 0)] == setmat))
          }
)


test_that(
  "invalid m_sup/mconf",
  {
    expect_error(apriorimining(mat, 1.5, 0.3))
    expect_error(apriorimining(mat, -1.5, 0.3))
    expect_error(apriorimining(mat, 0.2, 1.5))
    expect_error(apriorimining(mat, 0.2, -1.5))
  }
)



