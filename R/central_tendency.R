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

cmode <- function(x, ...) {
    den <- density(x, ...)
    den$x[den$y==max(den$y)]
}

# midhinge

# midrange

# trimean

# winsorized