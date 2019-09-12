par(mfrow = c(1, 1))


groc <- t(as(Groceries, "ngCMatrix"))


groc <- create_purchasematrix(groc)
x <- apriorimining(groc, 0.01, 0.3)
y <- freq_items(groc, 0.01)
y

### Barplot
sums <- unique(rowSums(y@sets))
x <- sapply(sums, FUN = function(a) sum(rowSums(y@sets) == a))
barplot(x)
barplot(x / 333)


### Show
sums <- unique(rowSums(y@sets))
for (i in 1:length(sums)) {
  cat("There are", sums[i], "itemsets containing", i, "itemsets\n")
}
cat("The longest set(s) contain(s)", length(sums), "items")


### show.itemsets
show.itemsets(y)

ind <- apply(y@sets, 1, which)
for (i in 1:length(ind)) {
  cat(y@items[ind[[i]]],"\n", sep = ",")
}

##









