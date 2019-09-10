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




### plot frequent items (from purchase)
par(mfrow = c(1,1))
freqs <- colSums(groc@data)
order(freqs, decreasing = TRUE)
ordered_ind <- order(freqs, decreasing = TRUE)
names <- groc@items[ordered_ind]
freqs <- freqs[ordered_ind]
barplot(freqs[1:20], names.arg = names[1:20], las = 2, cex.names = 0.75)

### show frequent items (from purchase)
means <- colMeans(groc@data)
names <- groc@items[means >= 0.01]
cat(names, sep ="\n")
# with support
cbind.data.frame(names, supp = means[means >= 0.01])

### show rare items (from purchase)
means <- colMeans(groc@data)
names <- groc@items[means < 0.01]
cat(names[1:50], sep ="\n")
# with support
cbind.data.frame(names, supp = round(means[means < 0.01], digits = 4))




