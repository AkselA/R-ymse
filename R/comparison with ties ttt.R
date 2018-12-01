#' Tied triple test
#' 
#' Compare numeric values, returning an inbetween value for ties
#' 
#' @param x,y numeric values to be compared
#' 
#' @seealso \code{\link{Comparison}}, \code{\link{comparison_with_ties}}
#' 
#' @examples
#' 1:5 %ttt% 3
#' 
#' ttt(1:3, 2)
#' print(ttt(1:3, 2), FALSE)
#' 
#' c(1, 6, 3, 0) %ttt% c(1, 3, 3, 2)
#' 
#' # Equivalent
#' as.integer(c(1, 6, 3, 0) %ttt% c(1, 3, 3, 2))
#' sign(c(1, 6, 3, 0) - c(1, 3, 3, 2))
#'
#' # Demonstrating table method 
#' dtf <- data.frame(x=1:5, y=3)
#' dtf$`?` <- ttt(dtf$x, dtf$y)
#' dtf
#' 
#' x <- c(8, 4, 6, 8, 9, 6, 5, 7, 0, 3, 2, 1, 5, 6, 4, 7, 6,
#'        3, 1, 9, 5, 6, 7, 7, 4, 5, 8, 6, 2, 5, 9, 5, 4, 8)
#' y <- c(1, 3, 2, 4, 6, 0, 5, 3, 7, 5, 7, 4, 5, 6, 0, 1, 4,
#'        2, 4, 3, 1, 5, 3, 9, 2, 2, 4, 7, 5, 6, 8)
#' 
#' ou <- outer(sort(x), sort(y), "%ttt%")
#' ta <- table(ou)
#' 
#' pa <- capture.output(ta)
#' 
#' par(mar=c(1, 2, 3, 2), family="PT Mono")
#' image(ou, col=topo.colors(length(ta)), axes=FALSE)
#' title(pa)
#' box()
#' 
#' @name tied_triple_test

NULL

#' @rdname tied_triple_test
#' @export %ttt%

`%ttt%` <- function(x, y) {
	v <- x != y
	v[x < y] <- -1
	v[x > y] <- 1
	class(v) <- c("ttt", "integer")
	v
}

#' @rdname tied_triple_test
#' @export ttt

ttt <- function(x, y) {
	v <- x != y
	v[x < y] <- -1
	v[x > y] <- 1
	class(v) <- c("ttt", "integer")
	v
}

#' @rdname tied_triple_test
#' @export is.ttt

is.ttt <- function(x) {
    inherits(x, "ttt")
}

#' @rdname tied_triple_test

print.ttt <- function(x, symbol=TRUE, ...) {
	if (symbol) x <- c("<", "=", ">")[x + 2]
	print(unclass(x), ...)
}

#' @rdname tied_triple_test

table.ttt <- function(...) {
	ta <- table.default(...)
	names(ta) <- c("<", "=", ">")
	ta
}

# table() and the rest can be found in 0_support.R
