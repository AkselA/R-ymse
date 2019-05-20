#' Maximum ACF, PACF and CCF
#' 
#' Find lag that maximizes correlation
#' 
#' @param x,y univariate numeric vector or time series
#' @param plot logical; return a plot
#' @param show indicate on the plot the maximum correlation
#' @param ci confidence interval used, by default 95\%
#' @param ma.ci should the confidence limits assume an MA input (\code{TRUE}, 
#' the default), or white noise as is default for \code{plot.acf}?
#' @param max.type what maximum should be returned, the positive (default),
#' negative, or absolute maximum?
#' @param most.signif should the most significant correlation be returned.
#' Only applicable if \code{ma.ci=TRUE}
#' @param ... further arguments passed to \code{acf}, \code{pacf}, \code{ccf}
#' 
#' 
#' @examples
#' x <- c(5, 5, 3, 6, 3, 6, 9, 6, 3, 1, 3, 2, 8, 9, 4, 3, 6, 6,
#'        6, 7, 5, 2, 5, 1, 5, 5, 0, 3, 7, 3, 6, 6, 2, 2, 6, 5)
#' y <- c(8, 9, 7, 5, 3, 5, 6, 9, 6, 3, 4, 5, 9, 7, 8, 5, 5, 7, 
#'        4, 7, 7, 2, 5, 6, 5, 7, 5, 3, 5, 6, 7, 0, 5, 3, 8, 4)
#' 
#' acf_max(x, plot=TRUE, max.type="abs")
#' acf_max(x, max.type="neg")
#' acf_max(x, max.type="neg", most.signif=TRUE)
#' 
#' pacf_max(x, plot=TRUE)
#' pacf_max(x, max.type="abs")
#' 
#' ccf_max(x, y, plot=TRUE)
#' ccf_max(x, y, max.type="neg")
#' 
#' # Same plot
#' plot(acf(x, plot=FALSE), ci.type="ma")
#' acf_max(x, plot=TRUE)
#' 
#' 
#' acf_max(x, ci=0.99, plot=TRUE)
#' ccf_max(x, y, ci=0, max.type="pos", plot=TRUE)
#' 
#' @export

acf_max <- function(x, ..., plot=FALSE, show=plot, ci=0.95, ma.ci=TRUE, 
  max.type=c("pos", "neg", "abs"), most.signif=FALSE) {
  	type <- match.arg(max.type)
  	tfun <- switch(type,
  	  pos=function(x) x,
  	  neg=function(x) -x,
  	  abs=function(x) abs(x)
  	)
  	
	x.obj <- acf(x, ..., plot=FALSE)
	x.lag <- c(x.obj$lag)
	x.acf <- c(x.obj$acf)
	x.acf[1] <- 0
	
	ci.type <- "white"
	
	if (ci > 0) {
		clim0 <- qnorm((1 + ci)/2)/sqrt(x.obj$n.used)
	    if (ma.ci & x.obj$type == "correlation") {
	        ci.type <- "ma"
		    clim <- clim0 * sqrt(cumsum(c(1, 2 * x.acf[-1]^2)))
		    clim <- c(NA, clim[-length(clim)])
            x.acfw <- tfun(x.acf)
            if (most.signif) x.acfw <- x.acfw - clim
    	    wh <- which.max(x.acfw)
	        max.acf <- c(x.acf[wh], x.lag[wh], clim[wh])
	    } else {
	    	clim <- clim0
    	    wh <- which.max(tfun(x.acf))
	        max.acf <- c(x.acf[wh], x.lag[wh], clim)
	    }
	    names(max.acf) <- c("acf", "lag", paste0(ci, "ci"))
	} else {
	    wh <- which.max(tfun(x.acf))
	    max.acf <- c(x.acf[wh], x.lag[wh])
	    names(max.acf) <- c("acf", "lag")
	}
	if (plot) {
		plot(x.obj, ci=ci, ci.type=ci.type)
	    if (show) {
	    	points(max.acf[2], max.acf[1], 
	    	  col=c(2, 2, 1)[match(type, c("pos", "neg", "abs"))])
	    }
	}
	return(max.acf)
}

#' @rdname acf_max
#' @export

pacf_max <- function(x, ..., plot=FALSE, show=plot, ci=0.95, 
  max.type=c("pos", "neg", "abs")) {
  	type <- match.arg(max.type)
  	tfun <- switch(type,
  	  pos=function(x) x,
  	  neg=function(x) -x,
  	  abs=function(x) abs(x)
  	)
  	
	x.obj <- pacf(x, ..., plot=FALSE)
	x.lag <- c(x.obj$lag)
	x.pacf <- c(x.obj$acf)
	
	if (ci > 0) {
		clim <- qnorm((1 + ci)/2)/sqrt(x.obj$n.used)
	    
	    wh <- which.max(tfun(x.pacf))
	    max.pacf <- c(x.pacf[wh], x.lag[wh], clim)
	    names(max.pacf) <- c("pacf", "lag", paste0(ci, "ci"))
	} else {
	    wh <- which.max(tfun(x.pacf))
	    max.pacf <- c(x.pacf[wh], x.lag[wh])
	    names(max.pacf) <- c("pacf", "lag")
	}
	if (plot) {
		plot(x.obj, ci=ci)
	    if (show) {
	    	points(max.pacf[2], max.pacf[1], 
	    	  col=c(2, 2, 1)[match(type, c("pos", "neg", "abs"))])
	    }
	}
	return(max.pacf)
}


#' @rdname acf_max
#' @export

ccf_max <- function(x, y, ..., plot=FALSE, show=plot, ci=0.95, 
  max.type=c("pos", "neg", "abs")) {
  	
  	if (missing(y)) {
  		if (NCOL(x) > 1) {
  			xo <- x
  			x <- xo[,1]
  			y <- xo[,2]
  		} else {
  			stop("argument \"y\" is missing and not enough columns in \"x\"")
  		}
  	}
  	
  	type <- match.arg(max.type)
  	tfun <- switch(type,
  	  pos=function(x) x,
  	  neg=function(x) -x,
  	  abs=function(x) abs(x)
  	)
  	
	x.obj <- ccf(x, y, ..., plot=plot)
	x.lag <- c(x.obj$lag)
	x.ccf <- c(x.obj$acf)
	
	if (ci > 0) {
		clim <- qnorm((1 + ci)/2)/sqrt(x.obj$n.used)
	    
	    wh <- which.max(tfun(x.ccf))
	    max.ccf <- c(x.ccf[wh], x.lag[wh], clim)
	    names(max.ccf) <- c("ccf", "lag", paste0(ci, "ci"))
	} else {
	    wh <- which.max(tfun(x.ccf))
	    max.ccf <- c(x.ccf[wh], x.lag[wh])
	    names(max.ccf) <- c("ccf", "lag")
	}
	if (plot) {
		plot(x.obj, ci=ci)
	    if (show) {
	    	points(max.ccf[2], max.ccf[1], 
	    	  col=c(2, 2, 1)[match(type, c("pos", "neg", "abs"))])
	    }
	}
	return(max.ccf)
}
