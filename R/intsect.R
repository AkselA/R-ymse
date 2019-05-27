.intsct <- function(x, y) unique(y[match(x, y, 0L)])

#' Intersect
#' 
#' Performs set intersection on a list of vectors
#' 
#' @param x list of sets (vectors of same mode or factors)
#' 
#' @details
#' The intersection between the sets in the list is found. This means no
#' duplicate values are returned, whether or not there were any in the
#' input.
#' 
#' @return
#' A vector of same mode as input, or a single factor object if input was factor.
#' 
#' @export
#' 
#' @examples
#' intsect(list(0:6, c(2, 4, 6, 8), 3:8))
#' 
#' fc <- factor(LETTERS[sample(1:5, 20, rep=TRUE)])
#' fcl <- split(fc, sample(1:3, 20, rep=TRUE))
#' 
#' intsect(fcl)

intsect <- function(x) {
	Reduce(.intsct, x)
}

