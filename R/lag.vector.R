lag_cycle <- function(x, k) {
	lx <- length(x)
	if (length(k > 1)) {
		l <- sapply(k, function(ki) x[(((1:lx)-1-ki) %% lx)+1])
		colnames(l) <- paste0("L.", k)
	} else {
	    l <- x[(((1:lx)-1-k) %% lx)+1]
	}
	l
}

lag_fill <- function(x, k, fill=NA) {
	if ((m <- min(k)) < 0) {
		k <- k - m
	}
	lx <- length(x)
	if (length(k) > 1) {
		mk <- max(k)
		l <- sapply(k, function(ki) c(rep(fill, ki), x, rep(fill, mk-ki)))
		colnames(l) <- paste0("L.", k)
	} else {
	    l <- c(rep(fill, k), x)
	}
	l
}

lag_trim <- function(x, k) {
	if ((m <- max(k)) > 0) {
		k <- k - m
	}
	k <- abs(k)
	if (length(k) > 1) {
        sl <- length(x)-diff(range(k))
		l <- sapply(k, function(ki) x[(1:sl)+ki])
		colnames(l) <- paste0("L.-", k)
	} else {
	    l <- x[-(1:k)]
	}
	l
}


#' @export
lag.vector <- function(x, k, type=c("cycle", "na.fill", "trim"), ...) {
	type <- match.arg(type)
	switch(type,
	  cycle=lag_cycle(x, k),
	  na.fill=lag_fill(x, k, NA),
	  trim=lag_trim(x, k))
}


