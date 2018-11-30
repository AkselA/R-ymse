#' Average shifted histogram
#' 
#' Create a smoothed histogram by averaging several histograms shifted by fractions 
#' of a bin-width
#' 
#' @param x a vector of values for which the histogram is desired
#' @param n.breaks an integer giving the number of bins to be used
#' @param n.shifts an integer giving the number of shifts to be performed
#' @param type if \code{plot=TRUE}, the type of plot to be used
#' @param freq should frequency counts be used, or density (default)
#' @param plot logical; if \code{TRUE} (default), a graphical output will be 
#' returned
#' @param add logical; if \code{TRUE} the plot will be added to the current plot 
#' @param ... further graphical parameters to \code{ymse::plot.histogram}, 
#' \code{polygon}, or \code{lines}
#' 
#' @return
#' an object of class \code{"histogram"}
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' n <- 6
#' 
#' x <- sample(sample(0:20, 8), 6*n, replace=TRUE) + rnorm(6*n, -8, 0.5)
#' x <- c(x, rgamma(5*n, 3, 0.5), rnorm(4*n, 15, 2))
#' x <- round(x*5)/5
#' 
#' hist(x, freq=FALSE, breaks="FD", col="lightblue")
#' hist.avg(x, type="hist", border=2, col=7, freq=FALSE, lwd=2)
#' hist.avg(x, type="poly", border=2, col=7, freq=FALSE, lwd=2)
#' hist.avg(x, type="line", col=2, freq=FALSE, lwd=2)
#' hist.avg(x, type="table", col=2, freq=FALSE, lwd=2)
#' hist.avg(x, plot=FALSE)

# https://stats.stackexchange.com/questions/51718
# http://www.stat.cmu.edu/~rnugent/PCMI2016/papers/ScottASH

hist.avg <- function(x, n.breaks=nclass.FD(x), n.shifts=3, 
  type=c("histogram", "polygon", "line", "table"), freq=FALSE, 
  plot=TRUE, add=FALSE, ...) {

	nbr <- n.breaks - 2
	nsh <- n.shifts + 1
	
	r <- range(x)
	s <- diff(r) / nbr
	br <- seq(r[1]-s, r[2], s)
	
	shift <- s * seq(0, 1, length.out=nsh)[-nsh]
	
	l <- lapply(shift, function(y) hist(x, breaks=br+y, plot=FALSE))
	
	comb <- l[[1]]
	for (i in 1:4) {
		comb[[i]] <- Reduce(c, lapply(l, "[[", i))
	}
	
	comb <- t(do.call(rbind, comb[2:4]))
	comb <- comb[order(comb[,"mids"]),]
	nr <- nrow(comb)
	d <- diff(comb[1:2, "mids"])
	br.comb <- seq(comb[1, "mids"] - d/2, comb[nr, "mids"] + d/2, len=nr+1)
	
	comb0 <- l[[1]]
	comb0$breaks <- br.comb
	comb0$counts <- comb[, "counts"] / n.shifts
	comb0$density <- comb[, "density"]
	comb0$mids <- comb[, "mids"]
	comb0$xname <- match.call()$x
	
	if (plot) {
		type <- match.arg(type)
		switch(type,
		    histogram=ymse:::plot.histogram(comb0, freq=freq, add=add, ...),
		    polygon=with(comb0, {
		    	if (freq) {
		    		val <- counts
		    	} else {
		    		val <- density
		    	}
		    	if (!add) {
		    	    plot(comb0, border="#00000000", col="#00000000", freq=freq)
		    	}
		        polygon(c(breaks[1], mids, breaks[nr+1]), 
		                c(0, val, 0), ...)
		    }),
		    line=with(comb0, {
		    	if (freq) {
		    		val <- counts
		    	} else {
		    		val <- density
		    	}
		    	if (!add) {
		    	    plot(comb0, border="#00000000", col="#00000000", freq=freq)
		    	}
		        lines(c(breaks[1], mids, breaks[nr+1]), 
		                c(0, val, 0), ...)
		    }),
		    table=with(comb0, {
		    	if (freq) {
		    		val <- counts
		    	} else {
		    		val <- density
		    	}
		    	if (!add) {
		    	    plot(comb0, border="#00000000", col="#00000000", freq=freq)
		    	}
		    	val[val == 0] <- NA
		        lines(mids, val, type="h", ...)
		    })
		)
    }
    if (plot) {
        invisible(comb0)
    } else comb0
}
