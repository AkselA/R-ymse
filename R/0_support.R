table <- function(...) {
	UseMethod("table")
}

table.default <- function(...) {
	base::table(...)
}

print.data.frame <- function(x, ...) {
	ittt <- sapply(x, is.ttt)
	x[ittt] <- lapply(x[ittt], function(x) c("<", "=", ">")[x + 2])
	base::print.data.frame(x, ...)
}