#' Simplified Local Polynomial Regression Fitting
#' 
#' A simplified interface to the \code{loess} and \code{predict.loess} combo.
#' 
#' @param y the response values to be regressed
#' @param df a data.frame with x-values in the first column and y-vlues in 
#' the second
#' @param x the regressor, by default an integer sequence along y
#' @param xout values used for prediction, unless it is an integer of length 1.
#' In that case xout specifies the number of equally spaced values on the interval 
#' of x to be used. By default the same as x
#' @param span parameter controlling the degree of smoothing
#' @param periodic should the input be treated as periodic?
#' @param ... further arguments passed to \code{\link{loess}}
#' 
#' @return
#' A data.frame with columns \emph{xout} and \emph{y.predicted}
#'
#' @examples
#' # Simple equally spaced vector
#' h <- c(-0.63, 0.2, -0.44, 1.6, 0.33, -0.74, -0.82, 0.29, 0.74, 0.58, -0.3)
#' 
#' plot(h)
#' lines(simple_loess(h))
#' 
#' # More complicated unequally space x-values
#' x <- c(4, 3, 2, 5, 6, 7, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19)
#' y <- c(3, 2, 4, 5, 6, 5, 5, 3, 4, 7, 10, 10, 8, 9, 7, 8)
#' 
#' plot(x, y)
#' lines(simple_loess(y, x), col="gray40")
#' points(simple_loess(y=y, x=x, xout=5L), col=2, cex=2)
#' points(simple_loess(y=y, x=x, xout=17), col=3, cex=2)
#' points(simple_loess(y=y, x=x, xout=seq(8, 12, 0.3)), col=3, pch=16)
#' lines(simple_loess(y=y, x=x, xout=50L), col=4, lty=2)
#' 
#' # data.frame input
#' dtf <- data.frame(x, y)
#' simple_loess(dtf)
#'
#' @export

simple_loess <- function(...) {
	UseMethod("simple_loess")
}

#' @rdname simple_loess
#' @export
simple_loess.default <- function(y, x=seq_along(y), xout=sort(x), span=0.75,
  periodic=FALSE, ...) {
    if (length(xout) == 1 & is.integer(xout)) {
    	xout <- seq(min(x), max(x), length.out=xout)
    }
    if (periodic) {
    	ol <- length(y)
  	  	or <- order(x)
  	  	x <- x[or] 
  	  	y <- y[or] 
    	xbef <- x - diff(range(x))-1
    	xaft <- x + diff(range(x))+1
    	if (span < 1) {
    		bef <- floor(ol*(1-span)):ol
    		aft <- 1:ceiling(ol*span+1)
    		xbef <- xbef[bef]
    		xaft <- xaft[aft]
    		ybef <- y[bef]
    		yaft <- y[aft]
    	}
    	x <- c(xbef, x, xaft)
    	y <- c(ybef, y, yaft)
    	span <- span/(length(y)/ol)
    }
    l <- loess(y ~ x, span=span, ...)
    d <- data.frame(x=xout)
    data.frame(xout, y.predicted=predict(l, d))
}

#' @rdname simple_loess
#' @export
simple_loess.data.frame <- function(df, xout=sort(df[,1]), ...) {
	x <- df[,1]
	y <- df[,2]
	simple_loess.default(y, x, xout, ...)
}
