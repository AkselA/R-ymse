lag_cycle <- function(x, k) {
	lx <- length(x)
	l <- lapply(k, function(ki) x[(((1:lx)-1-ki) %% lx)+1])
	l <- data.frame(l, stringsAsFactors=FALSE)
	colnames(l) <- paste0("L.", k)
	l
}


lag_fill <- function(x, k, fill=NA) {
	m <- min(k)
    k <- k - m
	lx <- length(x)
	mk <- max(k)
	l <- lapply(k, function(ki) c(rep(fill, ki), x, rep(fill, mk-ki)))
	l <- data.frame(l, stringsAsFactors=FALSE)
	colnames(l) <- paste0("L.", k+m)
	rownames(l) <- as.numeric(rownames(l)) + m
	l
}

lag_trim <- function(x, k) {
	o <- k
	k <- -k
	m <- min(k)
	k <- k - m
	k <- abs(k)
    sl <- length(x) - diff(range(k))
	l <- lapply(k, function(ki) x[(1:sl)+ki])
	l <- data.frame(l, stringsAsFactors=FALSE)
	colnames(l) <- paste0("L.", o)
	rownames(l) <- as.numeric(rownames(l)) - m
    l
}


#' Lag an arbitrary vector
#' 
#' @param x vector to be lagged
#' @param k integer vector specifying the number of lags
#' @param type how to deal with non-overlapping sections
#' @param ... further arguments passed to methods
#' 
#' @export
#' 
#' @examples
#' x <- 1:9
#' 
#' lag_vector(x, c(0, 1, -2, 3))
#' lag_vector(x, c(0, 1, -2, 3), "na")
#' lag_vector(x, c(0, 1, -2, 3), "trim")

lag_vector <- function(x, k, type=c("cycle", "na.fill", "trim"), ...) {
	type <- match.arg(type)
	switch(type,
	  cycle=lag_cycle(x, k),
	  na.fill=lag_fill(x, k, NA),
	  trim=lag_trim(x, k))
}


