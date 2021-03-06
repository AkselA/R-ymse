#' @export

table <- function(...) {
	UseMethod("table")
}

#' @export

table.default <- function(...) {
	base::table(...)
}

#' @export

print.data.frame <- function(x, ...) {
	ittt <- sapply(x, is.ttt)
	x[ittt] <- lapply(x[ittt], function(x) c("<", "=", ">")[x + 2])
	base::print.data.frame(x, ...)
}

#' @export

as.table.data.frame <- function(x, ...) {
	rownames(x) <- colnames(x)
	as.table(as.matrix(x), ...)
}
