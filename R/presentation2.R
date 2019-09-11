# second presentation script

mat <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1, 0,
                1, 1, 1, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 1, 0, 1, 0, 1, 0, 0, 1,
                0, 1, 1, 0, 1, 0, 0, 0, 0, 1,
                0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
                1, 0, 0, 1, 0, 0, 0, 1, 1, 1), nrow = 10, byrow = FALSE)

colnames(mat) <- c("Bananas", "Bread", "Butter", "Sugar", "Diapers", "Cheese")



x <- create_purchasematrix(mat)

show(x)
summary(x)

# second step function for generating the frequent itemsets

y <- freq_items(x,0.01)

summary(y)


# third step function for generating the rules

z <- apriorimining(mat, 0.01, 0.3)

summary(z)


##############################################











