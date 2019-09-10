#R script containing tests

# Mock data set

mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1, 0,
                1, 1, 1, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 1, 0, 1, 0, 1, 0, 0, 1,
                0, 1, 1, 0, 1, 0, 0, 0, 0, 1,
                0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
                1, 0, 0, 1, 0, 0, 0, 1, 1, 1), nrow = 10, byrow = FALSE)

colnames(mat) <- c("Bananas", "Bread", "Butter", "Sugar", "Diapers", "Cheese")


############# running the code

library(arules)
data("Groceries")
bsp<-as(Groceries,"matrix")


############ testing the output


install.packages("testthat")
library("testthat")
sort(colnames(def@lhs))

supps <- seq(0, 0.9, by = 0.1)

for (i in 1:length(supps)) {



  test_that("test if both packages generate the same amount of rules",
            {
              abc <- apriorimining(mat,support = i,confidence = 0.3)
              def <- apriori(mat, parameter = list(support = i , confidence = 0.3,
                                                   minlen = 2),
                             control = list(verbose = FALSE))
              expect_equal(dim(abc@antecedent)[1], length(def@lhs))
              expect_equal(sort(abc@items), sort(colnames(def@lhs)))

  })

}

for (i in 1:length(supps)) {
  j <- apriorimining(mat, supps[i], 0.1)
  show(j)
  cat("\n")
}




test_that("test if both packages generate the same confidence for the rules",
          {
            expect_equal(x, y)
          })


test<-inspect(def)
x <- sort(test[,4])
y <- sort(abc@measurements[,1])



test_that("test if both packages generate the same support for the rules",
          {
            expect_equal(x, y)
          })










