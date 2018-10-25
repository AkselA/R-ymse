#' Central tendency measures
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

# aka. Hodges–Lehmann estimator
pseudomedian <- function(x) {
    median(c(x, colMeans(combn(x, 2))))
}

#' @rdname central.tendency

cmode <- function(x, ...) {
    den <- density(x, ...)
    den$x[den$y==max(den$y)]
}

# midhinge

# midrange

# trimean

# winsorized