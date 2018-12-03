#' Comparison with ties
#' 
#' Compare numeric values, returning an inbetween value for ties
#' 
#' @param x,y numeric values to be compared
#' @param bias what bias should be given to ties? 0.5, the default, is 
#' considered neutral
#' as it's halfway between 1 and 0 (true and false).
#' 
#' @seealso \code{\link{Comparison}}, \code{\link{tied_triple_test}}
#' 
#' @examples
#' 1:5 %tlt% 3
#' 1:5 %tgt% 3
#' 
#' c(1, 4, 3, 1) %tlt% c(1, 3, 3, 2)
#' c(1, 4, 3, 1) %tgt% c(1, 3, 3, 2)
#' 
#' @name comparison_with_ties

NULL

#' @rdname comparison_with_ties
#' @export %tgt%

`%tgt%` <- function(x, y) {
	v <- x > y
	v[x == y] <- 0.5
	v
}

#' @rdname comparison_with_ties
#' @export tgt

tgt <- function(x, y, bias=0.5) {
	v <- x > y
	v[x == y] <- bias
	v
}


#' @rdname comparison_with_ties
#' @export %tlt%

`%tlt%` <- function(x, y) {
	v <- x < y
	v[x == y] <- 0.5
	v
}

#' @rdname comparison_with_ties
#' @export tlt

tlt <- function(x, y, bias=0.5) {
	v <- x < y
	v[x == y] <- bias
	v
}