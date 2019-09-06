#R script containing tests

############# running the code 

library(arules)
data("Groceries")
bsp<-as(Groceries,"matrix")

abc<-apriorimining(input = bsp,support = 0.01,confidence = 0.3)
def <- apriori(bsp, parameter = list(support = 0.01 , confidence = 0.3))

############ testing the output


install.packages("testthat")
library("testthat")

test_that("test if both packages generate the same amount of rules",
          {
            expect_equal(dim(abc@antecedent)[1], length(def@lhs))
            
          })


test<-inspect(def)
x <- sort(test[,5])
y <- sort(abc@measurements[,2])


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










