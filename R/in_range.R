#' Test if values is in a given range
#' 
#' Checks either whether both extrema are in the given range, or if each individual
#' value is.
#' 
#' @param x any atomic or vector-like object
#' @param lower lower range
#' @param upper upper range
#' @param inc logical vector of length one or two; should the lower and upper ranges, 
#' respectively, be considered inclusive?
#' @param na should \code{NA}s in \code{x} return \code{TRUE}, \code{FALSE},
#' \code{NA}, or something else?
#' 
#' @export
#' 
#' @examples
#' in_range(c(1:3, NA), 1, 4, na=NA)
#' 
#' in_range(1:4, 1, 4)
#' in_range(matrix(1:4, 2), 1, 4)
#' 
#' in_range(1:4, 1, 4, inc=1:0)
#' in_range(1:4, 1, 4, inc=0:1)
#' in_range(1:4, 1, 4, inc=0)
#' 
#' x <- as.Date(0:3, origin="2000-01-01")
#' in_range(x, "2000-01-01", "2000-01-04")
#' 
#' in_range(letters[1:4], "a", "d", inc=1:0)
#' in_range(letters[1:4], "a", "da", inc=1:0)
#' 
#' # no upper range
#' in_range(c(10^rnorm(9), NA), 0, NA)
#' in_range(c(10^rnorm(9), NA), 0, NA, na=TRUE)

in_range <- function(x, lower, upper, inc=c(TRUE, TRUE), na=NA) {
	inc <- inc+c(1, 3)
    fun <- c(`>`, `>=`, `<`, `<=`)[inc]
    l <- fun[[1]](x, lower)
    u <- fun[[2]](x, upper)
    o <- (l|is.na(l)) & (u|is.na(u))
    if (!isTRUE(na) & any(ina <- is.na(x))) {
    	o[ina] <- na
    }
    o
}
