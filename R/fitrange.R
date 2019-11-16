#' Normalize
#' 
#' Linearly shift and scale a numeric vector so that it has a given range, about
#' a given centre.
#' 
#' @param x a numeric vector
#' @param c the centre (as in the midrange) for the new vector
#' @param r the range of the new vector
#' 
#' @seealso \code{\link{fitrange}}
#' 
#' @export
#' 
#' @examples
#' range(norma(runif(9, -2, 0.1), 0, 2))

norma <- function(x, c=0, r=2) {
    (x - min(x, na.rm=TRUE)) * 
    (r/(max(x, na.rm=TRUE) - min(x, na.rm=TRUE))) - 
    r/2 + c
}


#' Fit to a range
#' 
#' Linearly shift and scale a numeric vector so that it fits to a given range.
#' 
#' @param x a numeric vector
#' @param lower the lower bound of the new vector
#' @param upper the upper bound of the new vector
#' 
#' @seealso \code{\link{norma}}
#' 
#' @export
#' 
#' @examples
#' range(fitrange(runif(10, -2, 1.5), 0, 1))
#' 
#' fitrange(c(2, 3, 5, 7, 4), 1, 0)
#' # same, but without warning
#' 1 - fitrange(c(2, 3, 5, 7, 4), 0, 1)

fitrange <- function(x, lower=-1, upper=1) {
	if (lower > upper) {
		warning("upper bound is smaller than lower bound")
	}
	if (length(x) == 0) {
		return(numeric(0))
	}
	newrange <- upper - lower
	mi <- min(x, na.rm=TRUE)
	oldrange <- max(x, na.rm=TRUE) - mi
	if (oldrange == 0) {
		d <- abs(x - lower) < abs(x - upper)
		ifelse(d, lower, upper)
	} else {
	    (x - mi) * (newrange/oldrange) + lower
    }
}