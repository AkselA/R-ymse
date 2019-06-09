qtrim <- function(x, ...) {
	UseMethod("qtrim")
}

qtrim.default <- function(x, q=0.1, keep.length=FALSE) {
    q <- quantile(x, c(q, 1-q), names=FALSE)
    if (!keep.length) {
        x[(x > q[1]) & (x < q[2])]
    } else {
        x[(x < q[1]) | (x > q[2])] <- NA
        x
    }
}

qtrim.data.frame <- function(x, q=0.1, keep.length=TRUE) {
    l <- lapply(x, qtrim, q=q, keep.length=keep.length)
    do.call(cbind, l)
}

winsor <- function(x, ...) {
	UseMethod("winsor")
}

winsor.default <- function(x, q=0.1, sd) {
	if (!missing(sd)) {
		sd <- sd(x)*sd
	    m <- mean(x)
	    l <- c(m-sd, m+sd)
	} else {
		l <- quantile(x, probs=c(q, 1-q), names=FALSE)
	}
	x[x < l[1]] <- l[1]
	x[x > l[2]] <- l[2]
	x
}

winsor.data.frame <- function(x, ...) {
    do.call(cbind, lapply(x, winsor, ...))
}


# set.seed(1)
# r <- sort(round(rt(100, 4)*20, 1))

# plot(r, pch="|", cex=0.6, col="darkgrey")
# lines(winsorize(r, sd=1.96), col="red")
# lines(winsorize(r, q=0.05), col="blue")

olympic <- function(x, ...) {
	UseMethod("olympic")
}

olympic.default <- function(x, keep.length=FALSE, strict=TRUE) {
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

olympic.data.frame <- function(x, keep.length=TRUE, ...) {
    l <- lapply(x, olympic, keep.length=keep.length, ...)
    do.call(cbind, l)
}

# set.seed(1)
# x <- rnorm(40)^3
# xr <- sort(round(x, 1)*10)
# xr <- c(xr[1], xr)

# olympic(xr)
# qwinsor(xr)
# qtrim(xr)
