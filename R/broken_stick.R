# Niche apportionment models, aka. broken stick models

# Dominance decay
stick <- function() {
	n <- 10
	x <- 1
	v <- numeric(n)
	r <- runif(n)
	for (i in 1:n) {
	    b <- x*c(r[i], 1-r[i])
	    x <- max(b)
	    v[i] <- min(b)
	}
	v
}
# set.seed(1)
# re <- replicate(1e4, f())
# rm <- rowMeans(re)
# rd <- apply(re, 1, sd)

# plot(rm)

