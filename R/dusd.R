#' Discrete (Uniform) Sum Distributions
#' 
#' Generate distributions of the sum of discrete (uniform) random variables.
#' Two different approaches.
#' 
#' @param xr numeric vector; a vector of equiprobable values
#' @param xi numeric vector; a vector of probabilities, with indices representing
#' values
#' @param n integer; the number of distributions to be summed
#' @param FUN function passed on to \code{outer}
#' @param round integer; number of digits to round to after each convolution 
#' @param bix logical; where does the index of xi start?
#' @param limit numeric; values (frequencies or counts) less than this will be
#' omitted.
#' 
#' @details \code{dusd1} works by recursively taking the outer sum of xr, while
#' \code{dusd2} recursively convolves xi. Although convolution is more efficient,
#' it can introduce small errors, and with repeated convolutions those errors can
#' compound. By rounding to a slightly lower precision after each convolution the
#' generation of spurious singletons and general imprecicions can be mitigated.
#' 
#' @return \code{dusd1} returns an array of size length(xr)^n representing every
#' possible outcome. \code{dusd2} returns a probability mass function in the form
#' of a table.
#' 
#' @seealso \code{\link{combodice}} for a more flexible implementation of the 
#' same ideas
#' 
#' @examples
#' # five coin flips
#' plot(table(dusd1(0:1, 5)))
#' plot(dusd2(c(1, 1), 5, bix=0))
#' plot(as.table(dbinom(0:5, 5, 0.5)))
#' 
#' # ten flips with a loaded coin
#' plot(table(dusd1(c(1, 1, 2), 10)))
#' plot(dusd2(c(2, 1), 10))
#' plot(dbinom(0:10, 10, 1/3), type="h", lwd=2)
#' 
#' # sample from a multi-roll d4 distribution
#' sample(dusd1(1:4, 5), 20, replace=TRUE)
#' plot(ecdf(dusd1(1:4, 5)))
#' 
#' tt <- dusd2(xi=rep(1, 4), n=3)
#' plot(tt)
#' tt <- tt/sum(tt)
#' rr <- replicate(50000, sample(names(tt), prob=tt))
#' barplot(apply(rr, 1, table), beside=TRUE)
#' 
#' # distribution of the sum of three d6 rolls
#' plot(table(dusd1(xr=1:6, 3)))
#' plot(dusd2(xi=rep(1, 6), n=3))
#' 
#' # d6 die with faces 2, 3, 5, 7, 11, 13 (prime numbers)
#' plot(table(dusd1(xr=c(2, 3, 5, 7, 11, 13), 3)))
#' 
#' # Probalility of getting 7 or 8 with an 8-sided die in n out of 5 throws
#' l <- 6/8
#' h <- 1-l
#' d <- as.dice(c(l, h), bix=0)
#' 
#' dusd2(d, 5)
#' # need integer "probabilities" for dusd1
#' table(dusd1(d*4, 5))/(4^5)
#' # or an equivalent die
#' table(dusd1(c(0, 0, 0, 1), 5))/(4^5)
#' 
#' # Loaded die
#' p <- c(0.5, 1, 1, 1, 1, 1.5); sum(p)
#' plot(dusd2(xi=p, n=2))
#' 
#' # A loaded die with prime number faces
#' s <- vector(length=13)
#' s[c(2, 3, 5, 7, 11, 13)] <- c(0.5, 1, 1, 1, 1, 1.5)
#' plot(dusd2(xi=s, n=3))
#' 
#' # tricky to do with dusd2
#' plot(table(dusd1(xr=c(0.1105, 2, exp(1)), 10)))
#' 
#' # Demonstrating CLT
#' # dusd1 struggles with many iterations
#' # remember it returns an array of size length(xr)^n
#' plot(table(dusd1(xr=c(1, 2, 9), 12)))
#' 
#' s <- vector(length=9)
#' s[c(1, 2, 9)] <- 1
#' plot(dusd2(xi=s, 12, round=9)) # much quicker
#' plot(dusd2(xi=s/sum(s), 12)) # for frequencies instead of counts
#' 
#' # Impossible with dusd1
#' clt <- dusd2(xi=s, 15, round=9)
#' plot(clt, lwd=0.5, col="#00000088")
#' 
#' # small floating-point errors from convolution.
#' tail(dusd2(xi=s, 15))
#' 
#' # dusd2 isn't always quicker
#' \dontrun{
#' plot(table(dusd1(xr=c(1, 220, 3779), 12)), lwd=1)
#' s2 <- vector(length=3779)
#' s2[c(1, 220, 3779)] <- 1
#' plot(dusd2(xi=s2, 12, round=8), lwd=1)
#' 
#' # making sure the length of xi is highly composite (or more precicely 'smooth')
#' # improves speed
#' # 3779 is prime, 3780 == 2*2*3*3*3*5*7
#' s3 <- vector(length=3780)
#' s3[c(1, 220, 3779)] <- 1
#' plot(dusd2(xi=s3, 12, round=9), lwd=1)
#' }
#' 
#' @name dusd

NULL

#' @rdname dusd
#' @export dusd1

dusd1 <- function(xr=1:6, n=2, FUN="+") {
	UseMethod("dusd1")
}

#' @export

dusd1.default <- function(xr=1:6, n=2, FUN="+") {
    FUN <- match.fun(FUN)
	li <- replicate(n, xr, simplify=FALSE)
	fu <- function(a, b) outer(a, b, FUN)
    Reduce(fu, li)
}

#' @export

dusd1.dice <- function(xr, ...) {
	dusd1.default(expand(xr), ...)
}

#' @export

dusd1.table <- function(xr, ...) {
	dusd1.default(expand(xr), ...)
}

#' @rdname dusd
#' @export dusd2

dusd2 <- function(xi=rep(1, 6), n=2, bix=1, round, limit=1e-13) {
	UseMethod("dusd2")
}

#' @export

dusd2.default <- function(xi=rep(1, 6), n=2, bix=1, round, limit=1e-13) {
    if (!missing(round)) {
	    li <- replicate(n, xi, simplify=FALSE)
        re <- Reduce(function(a, b) {
            co <- convolve(a, rev(b), type="open")
            round(co, round)
          }, li)
    } else {
	    li <- replicate(n, xi, simplify=FALSE)
        re <- Reduce(function(a, b) convolve(a, rev(b), type="open"), li)
    }
    re <- as.table(re)
    names(re) <- seq.int(bix*n, length.out=length(re))
    re[re > limit]
}

#' @export

dusd2.dice <- function(xi, n=2, ...) {
	dusd2.default(xi=xi, n=n, bix=bix(xi), ...)
}

#' @export

dusd2.table <- function(xi, n=2, ...) {
    xi <- as.dice(xi)
	dusd2.default(xi=xi, n=n, bix=bix(xi), ...)
}