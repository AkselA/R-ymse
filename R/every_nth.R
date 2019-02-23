modn <- function(x, n) {
	n - ((n - x) %% n)
}

#' Select every n'th element
#' 
#' Select every second, third, fourth etc. element (or slice/hyperplane) of an
#' object
#' 
#' @param x an object to be selected from
#' @param n selection "step size"
#' @param start integer in [1:n] specifying the start of selection
#' @param margin what margin to select along
#' @param ... further arguments passed to methods
#' 
#' @export
#' 
#' @examples
#' m <- matrix(1:64, 8)
#' every_nth(m, n=3, start=3, margin=2)
#' 
#' d <- data.frame(A=1:8, B=2:9, Q=letters[rep(1:3, length.out=8)])
#' every_nth(d, start=2)
#' 
#' a <- array(1:6^4, rep(6, 4))
#' every_nth(a)
#' 
#' l <- list(a=1:3, b=2:6, c=8:5, d=9:7, e=list(ea=1:2, eb=1), f=2:6)
#' every_nth(l, n=2, start=2)

every_nth <- function(...) {
	UseMethod("every_nth")
}


#' @rdname every_nth
#' @export

every_nth.default <- function(x, n=2, start=1, ...) {
	x[modn(seq_along(x), n) == start]
}


#' @rdname every_nth
#' @export

every_nth.matrix <- function(x, n=2, start=1, margin=1, ...) {
	d <- dim(x)[margin]
	if (margin == 1) x[modn(1:d, n) == start, ]
	else
	if (margin == 2) x[, modn(1:d, n) == start]
    else stop("invalid margin")
}


#' @rdname every_nth
#' @export

every_nth.array <- function(x, n=2, start=1, margin=1, ...) {
	d <- dim(x)
	ndim <- length(d)
	if (margin > ndim) {
		stop("invalid margin")
	}
	s <- modn(1:d[margin], n) == start
	
	mar <- rep("", ndim)
	mar[margin] <- "s"
	mar <- paste(mar, collapse=",")
	ext <- paste0("x[", mar, "]")
	
	eval(parse(text=ext))
}


#' @rdname every_nth
#' @export

every_nth.data.frame <- function(x, n=2, start=1, margin=1, ...) {
	if (margin == 1) x[modn(1:nrow(x), n) == start, ]
	else
	if (margin == 2) x[, modn(1:ncol(x), n) == start]
    else stop("invalid margin")
}


#' @rdname every_nth
#' @export

every_nth.list <- function(x, n=2, start=1, ...) {
	x[modn(1:length(x), n) == start]
}