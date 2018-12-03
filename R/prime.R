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
#' @seealso \code{\link{isPrime}}
#' 
#' @export

primes <- function(n) {
    n <- as.integer(n)
    if(n > 1e8 + 7) stop("n very large.\nEdit function code to run on large n")
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
#' Test an integer for whether it is prime or not
#' 
#' @param x integer; one or more prime candidates
#' 
#' @seealso \code{\link{primes}}
#' 
#' @export

isPrime <- function(x) {
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