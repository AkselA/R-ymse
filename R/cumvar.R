


cumvar <- function (x, na.rm=FALSE) {
	if (na.rm) {
		na <- is.na(x)
	    x <- x - x[match(FALSE, na)]
		x[na] <- 0
		n <- cumsum(!na)
        cv <- (cumsum(x ^ 2) - cumsum(x) ^ 2 / n) / (n - 1)
	} else {
        x <- x - x[1]
        n <- seq_along(x)
        cv <- (cumsum(x ^ 2) - cumsum(x) ^ 2 / n) / (n - 1)
    }
    # cv[1] <- 0
    cv
}

cumsd <- function(x, na.rm=FALSE) sqrt(cumvar(x, na.rm=na.rm))

