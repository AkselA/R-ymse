rbates <- function(n, a=0, b=1, nr=5, const.var=FALSE) {
	q <- trunc(nr)
	r <- nr - q
	
	rr <- replicate(q, runif(n, a, b))
	
	if (r != 0) {
	    dd <- rowSums(rr) + runif(n, a, b)*r
	} else {
        dd <- rowSums(rr)
	}

	if (const.var) {
		dd <- dd/sqrt(nr)
	} else {
		dd <- dd/nr
	}
}

rfac <- function(x, n) {
	if (n == 0) return(1)
	prod(x + seq_len(n)-1)
}

ffac <- function(x, n) {
	if (n == 0) return(1)
	prod(x - seq_len(n)-1)
}
# rfac(1, 5)

kummer <- function(a=2, b=2, z=1.5, n=3) {
	f <- function(n) {
		(rfac(a, n)*z^n) / (rfac(b, n)*factorial(n))
	}
	sum(sapply(0:n, f))
}
# kummer(2, 1, 2, 3)

laguerre <- function(x, q=1/2, a=0.1, n=5) {
	choose(q+a, q)*kummer(a=-q, b=a+1, z=x, n=n)
}
# laguerre(0.5, 1)
