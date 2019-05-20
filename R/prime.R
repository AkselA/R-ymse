#' Prime number generator
#' 
#' Prime generator based on the sieve of Eratosthenes
#' 
#' @param n integer; all prime numbers up to this will be returned
#' 
#' @details Effective for primes up to ~100,000,000. \cr
#' On my lightweight laptop:
#' 1e7 -> 0.32s, 5e7 -> 1.7s, 1e8 -> 3.7s, 2e8 -> 7.6s, 3e8 -> 15s
#' 
#' @source 
#' \url{https://stackoverflow.com/questions/3789968/
#' generate-a-list-of-primes-up-to-a-certain-number/3791284#3791284}
#' 
#' @seealso \code{\link{is_prime}}
#' 
#' @export

primes <- function(n) {
    n <- as.integer(n)
    if (n > 1e8 + 7) stop("n very large.\nEdit function code to run on large n")
    primes <- rep(TRUE, n)
    primes[1] <- FALSE
    last.prime <- 2L
    fsqr <- floor(sqrt(n))
   
    while (last.prime <= fsqr) {
        primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
        sel <- which(primes[(last.prime+1):(fsqr + 1)])
        if (any(sel)) {
            last.prime <- last.prime + min(sel)
        } else {
        	last.prime <- fsqr + 1
        }
    }
    which(primes)
}

#' Primality check
#' 
#' Test integers for whether they are prime or not
#' 
#' @param x vector of integers
#' 
#' @seealso \code{\link{primes}}
#' 
#' @export

is_prime <- function(x) {
	if (length(x) > 1) {
    	pr <- primes(max(x))
	    x %in% pr
	} else {
	 	if (x == 2) {
			TRUE
		} else {
	        div <- 2:ceiling(sqrt(x))
	        !any(x %% div == 0)
	    }
    }
}

#' Greatest common divisor
#' 
#' Find the largest integer, that when two numbers are divided by it,
#' returns an integer in both cases
#' 
#' @param x,y integers whose greates common divisor is to be found
#' 
#' @examples
#' gcd(sequence(10:16), rep(10:16, 10:16))
#' 
#' @export

gcd <- function(x, y) {
	if ((lx <- length(x)) != (ly <- length(y))) {
		stop("x and y need to be of equal length.\n",
		  sprintf("\tWas %i and %i, respectively", lx, ly))
	}
	gcd_ <- function(x, y) {
	    r <- x %% y
	    ifelse(r, Recall(y, r), y)
	}
	gcd_(x, y)
}

#' Coprimality check
#' 
#' Test whether to integers are coprime, that is, have no factors in common
#' 
#' @param x,y integers to be tested for coprimality
#' 
#' @return
#' A logical vector
#' 
#' @examples
#' is_coprime(sequence(10:16), rep(10:16, 10:16))
#' is_coprime(2*3*5*7, 11*13)
#' 
#' @export

is_coprime <- function(x, y) {
	if (length(x) == 1 & length(y) == 1) {
	    !any(duplicated(c(
	      factors(x, prime=TRUE), factors(y, prime=TRUE)
	    )))
	}
	gcd(x, y) == 1
}

#' Factors
#' 
#' Find the integers a given number is divisible by
#' 
#' @param x an integer
#' @param prime should only prime factors be returned?
#' 
#' @return
#' An integer vector
#' 
#' @seealso \code{\link{factorise}} for prime factorisation
#' 
#' @section Note:
#' The trivial factors \code{1} and \code{x} itself are not included.
#' 
#' @examples
#' factors(210)
#' factors(210, prime=TRUE)
#' 
#' @export

factors <- function(x, prime=FALSE) {
    x <- as.integer(x)
    if (prime) {
        div <- primes(x)
    } else {
        div <- seq_len(x %/% 2)[-1]
    }
    div[! x %% div]
}

#' Factorise
#' 
#' Find the prime factors of a given integer
#' 
#' @param x integer
#' 
#' @return
#' An integer vector
#' 
#' @seealso \code{\link{factors}} for unique prime factors or all integer factors
#' 
#' @examples
#' x <- 2 * 2 * 2 * 3 * 3 * 5
#' factorise(x)
#' 
#' prod(factorise(5641324))
#' 
#' factorise(nextn(60000000, c(2, 3)))
#' factorise(72*999983)
#' 
#' @export

factorise <- function(x) {
	if (is_prime(x)) {
		return(x)
	}
    fa <- NULL
    while (x > 1) {
        f <- factors(x, TRUE)
        fa <- c(fa, f)
        x <- x/prod(f)
    }
    sort(fa)
}

# ftp://stat.ethz.ch/U/maechler/R/prime-numbers-fn.R
##- From: Paul A Tukey <paul@bellcore.com>
##- Date: Wed, 16 Sep 1998 18:27:15 -0400 (EDT)

Tfac <- function(n) {
    p <- n/(z <- 1:floor(sqrt(n)))
    z <- z[trunc(p) == p]
    c(z, rev(n/z))[2]
}

Tpfac <- function(n, nn = 0) {
    if (nn == 0) {
        Recall(n, Tfac(n))
    } else {
        if (n <= nn) {
            nn
        } else {
            c(nn, Recall(n/nn))
        }
    }
}
