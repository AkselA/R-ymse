

ext_range <- function(x, f=1.1, FUN="*", ...) {
	if (length(x) > 2) {
	    x <- range(x)
	} else {
	    if (length(x) == 1) {
	    	x <- c(-x/2, x/2)
	    }
	}
	FUN <- match.fun(FUN)
	d <- diff(x)/2
	m <- x[1]+d
	
	d2 <- FUN(d, f, ...)
	
	c(m-d2, m+d2)
}

#' Generate a sequence spanning a given range
#' 
#' @param x a single numeric, a range, or a sequence
#' @param ... further arguments passed to \code{seq}
#' @param spread use \code{spread_seq} to spread out the range by a given factor
#' 
#' @details
#' If \code{x} is a single number, the range is interpreted to be \code{[0, x]}.
#' If \code{x} is length two, the numbers are interpreted as the left and right
#' extrema of the sequence interval.
#' If \code{x} is longer than two, the sequence is based upon its \code{range}.
#' 
#' @export
#' 
#' @examples
#' 
#' seq_range(c(1, 4), by=0.5)
#' seq_range(c(1, 4), by=0.5, spread=2)
#' seq_range(4)
#' 
#' x <- sample(1:10, 3)
#' seq_range(x)

seq_range <- function(x, ..., spread) {
    x <- x[!is.na(x)]
    if (length(x) == 1) {
    	x <- c(0, x)
    }
    if (length(x) < 2) {
    	x <- range(x)
    }
    if (!missing(spread)) {
    	x <- spread_seq(x, f=spread)
    }
	seq(from=x[1], to=x[2], ...)
}

#' Spread sequence
#' 
#' Spread out the values of a numeric vector 
#' 
#' @param x a numeric vector
#' @param f numeric or item matching a function. If numeric, a multiplicative
#' factor applied to the distance between points, otherwise a function to be
#' applied to differences
#' @param ... further arguments passed to \code{f} if matching a function
#' @param node the location of \code{x} that will remain unchanged
#' 
#' @export
#' 
#' @examples
#' x <- c(-1, 0, 2, NA, 4, 5, 6, 8, 9)
#' 
#' spread_seq(c(-1, 1))
#' spread_seq(x, 1.5, "midrange")
#' spread_seq(x, 1.5, "first")
#' spread_seq(x, 1.5, "last")
#' spread_seq(x, 1.5, "mean")
#' spread_seq(x, 1.5, "median")
#' 
#' spread_seq(c(3, 4, 1, 9, 6))
#' spread_seq(c(3, 4, 1, 9, 6), 2, "min")
#' spread_seq(c(3, 4, 1, 9, 6), 2, "max")
#' 
#' spread_seq(c(3, 4, 1, 9, 6), "/", 2)
#' spread_seq(c(3, 4, 1, 9, 6), 0.5)
#' 
#' y <- sort(c(3, 4, 1, 9, 6))
#' plot(y)
#' lines(spread_seq(y, "log1p"))
#' 
#' f <- function(x) sqrt(abs(x)*2)*sign(x)
#' y <- c(3, 4, 1, 9, 6)
#' plot(y)
#' lines(spread_seq(y, f=f))

spread_seq <- function(x, f=1.1, ...,
  node=c("midrange", "first", "last", "mean", "median", "min", "max")) {
  	xo <- x
    naix <- is.na(x)
    x <- x[!naix]
    if (is.numeric(f)) {
        d <- diff(x)*f
    } else {
  	    FUN <- match.fun(f)
  	    d <- FUN(diff(x), ...)
  	}
  	node <- match.arg(node)
  	cs <- switch(node,
  	  midrange={
          cs <- cumsum(c(x[1], d))
          midrange(x) + cs - midrange(cs)
  	  },
  	  first={
          cumsum(c(x[1], d))
  	  },
  	  last={
          cs <- cumsum(c(x[1], d))
          x[length(x)] + cs - cs[length(cs)]
  	  },
  	  mean={
          cs <- cumsum(c(x[1], d))
          mean(x) + cs - mean(cs)
  	  },
  	  median={
          cs <- cumsum(c(x[1], d))
          median(x) + cs - median(cs)
  	  },
  	  min={
          cs <- cumsum(c(x[1], d))
          min(x) + cs - min(cs)
  	  },
  	  max={
          cs <- cumsum(c(x[1], d))
          max(x) + cs - max(cs)
  	  }
  	)
  	xo[!naix] <- cs
    xo
}