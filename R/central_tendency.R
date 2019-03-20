clamp <- function(x, lower, upper) {
	pmax(lower, pmin(upper, x))
}

#' Central tendency measures
#' 
#' @param x numeric vector
#' @param na.rm remove \code{NA}s before starting calculations
#' @param ... send further arguments to underlying function, e.g. \code{density}
#' for cmode
#' 
#' @seealso \code{\link{means}}
#' 
#' @examples
#' xx <- c(1, 3, 4, 5, 7, 8, 9, 9, 7, 5, 4, 5, 3, 8)
#' median(xx)#' 
#' pseudomedian(xx)
#' 
#' @name central.tendency

NULL

#' @rdname central.tendency
#' @export pseudomedian

# aka. Hodges–Lehmann estimator
pseudomedian <- function(x, na.rm=TRUE) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
    median(c(x, colMeans(combn(x, 2))))
}

#' @rdname central.tendency
#' @export cmode

# continuous mode
cmode <- function(x, ...) {
    den <- density(x, ...)
    den$x[den$y==max(den$y)]
}

midrange <- function(x, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
     }
	(min(x)+max(x))/2
}

# slightly robust mean
srm <- function(x, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
	mr <- (min(x)+max(x))/2
	(sum(x)-mr)/(length(x)-1)
}

# set.seed(1)
# r <- round(rexp(10)*c(-10, 10))
# srm(r)
# weighted.mean(sort(r), c(0.5, rep(1, 8), 0.5))

# midhinge

# trimean

# winsorized