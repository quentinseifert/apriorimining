# presentation
library("arules")
data("Groceries")
bsp <- as(Groceries,"matrix")
bsp[5000:5015,1:10] * 1 # fenster gro? genug machen damit man es sehen kann







groceries <- create_purchasematrix(bsp)

show(groceries)
summary(groceries)
rareitems(groceries, support = 0.01)
rareitems(groceries, support = 0.01, absolute = TRUE)
plot(groceries)

# second step function for generating the frequent itemsets

itemsets <- freq_items(groceries, 0.01)
show(itemsets)
summary(itemsets)
plot(itemsets)

# third step function for generating the rules

a_rules <- apriorimining(bsp, 0.01, 0.3)

show(a_rules)
summary(a_rules)


##############################################



