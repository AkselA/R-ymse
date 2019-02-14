qtrim <- function(x, lower=0.1, upper=1-lower, keep.length=FALSE, ...) {
	q <- quantile(x, c(lower, upper), ...)
	if (!keep.length) {
        x[(x > q[1]) & (x < q[2])]
    } else {
        x[(x < q[1]) | (x > q[2])] <- NA
        x
    }
}

qwinsor <- function(x, lower=0.1, upper=1-lower, ...) {
	q <- quantile(x, c(lower, upper), ...)
    x[(x < q[1])] <- q[1]
    x[(x > q[2])] <- q[2]
    x
}

olympic <- function(x, keep.length=FALSE, strict=TRUE) {
	if (strict) {
		if (keep.length) {
			x[c(which.min(x), which.max(x))] <- NA
			x
		} else {
			x[-c(which.min(x), which.max(x))]
		}
	} else {
		if (keep.length) {
			x[(x %in% range(x))] <- NA
			x
		} else {
			x[!(x %in% range(x))]
		}
	}
}

# set.seed(1)
# x <- rnorm(40)^3
# xr <- sort(round(x, 1)*10)
# xr <- c(xr[1], xr)

# olympic(xr)
# qwinsor(xr)
# qtrim(xr)
