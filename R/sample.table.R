#' @export

sample <- function(...) {
	UseMethod("sample")
}

#' @export

sample.default <- function(...) {
	base::sample(...)
}

#' @export

sample.table <- function(x, size, replace=TRUE, ...) {
	prob <- as.vector(x)
	x <- type.convert(names(x), as.is=TRUE)
	base::sample(x, size=size, replace=replace, prob=prob)
}

# t1 <- as.table(c("1.2"=1, "4"=2.1, "5.8"=2.6))
# sample(tt)

# t2 <- as.table(c("A"=1, "H"=2, "Q"=2))
# sample(t2, 8, TRUE)
