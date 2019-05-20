fraction <- function(x, n=10, d=n) {
	if (length(n) == 1) {
		n <- 1:n
	}
	if (length(d) == 1) {
		d <- 1:d
	}
	o <- outer(n, d, "/")
	a <- arrayInd(which.min(abs(o - x)), dim(o))
	f <- o[a]
	names(f) <- paste0(n[a[1]], "/", d[a[2]])
	f
}

# x <- 0.643
# fraction(x)
# fraction(x, 15)
# fraction(x, primes(15))
# fraction(x, primes(100))
# fraction(x, smoothrange(50, k=5))
# fraction(x, smoothrange(20, k=3), primes(20))

smoothrange <- function(from, to, k=7) {
	if (missing(to)) {
		to <- from
		from <- 1
	}
	x <- seq.int(from, to, by=1)
	d <- unique(nextn(x, primes(k)))
    d[d <= to]
}

# # 7-smooth numbers up to and including 24
# smoothrange(24)

# # All numbers between 50 and 82 divisible by only 2 or 3
# sr <- smoothrange(50, 82, 3)

