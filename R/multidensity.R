#' Plot multiple kernel density estimates
#' 
#' Plot multiple kernel density estimates in the same window, together with a
#' legend
#' 
#' @param x a list or data.frame of numeric values
#' @param main a main title for the plot. Defaults to the call made
#'        to \code{density}
#' @param xlab,ylab labels for the x and y axes
#' @param xlim,ylim the x and y limits of the plot
#' @param col,lty,lwd the line colours, types and widths for lines appearing
#'        in plot and legend
#' @param add if \code{TRUE}, add to the current plot
#' @param frame.plot a logical indicating whether a box should be drawn around
#'        the plot.
#' @param legend logical; if \code{TRUE} (the default) a legend is included with 
#'        the plot
#' @param x.legend,y.legend the x and y co-ordinates to be used to position the
#'        legend. They can be specified by keyword or in any way which is 
#'        accepted by xy.coords
#' @param bty legend box type
#' @param box.col line colour for the legend box
#' @param bg.legend background colour for the legend box
#' @param cex.legend character expanson faftor for legend
#' @param x.intersp,y.intersp horizontal and vertical character interspacing for
#'        legend
#' @param inset the legends inset distance from the margins as a fraction of 
#'        the plot region
#' @param xpd.legend the value of \code{xpd} to be used while drawing the legend
#' @param horiz logical; if \code{TRUE}, set the legend horizontally rather than
#'        vertically
#' @param ... further arguments passed to \code{density}
#' 
#' @return
#' An invisible list of the \code{"density"} objects the plot is based on.
#' 
#' @seealso \code{\link{density}}, \code{\link{ahist}}
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' dl <- list("Unif-1"=runif(80, -2.1, 2.1), 
#'            "Unif-2"=runif(70, -1.5, 1.5), 
#'          "Normal-1"=rnorm(50, 0, 0.866), 
#'          "Normal-2"=rnorm(90, 0, 1))
#' 
#' # sqrt((sd^2)*12) # sd to unif range
#' 
#' md <- multidensity(dl)
#' head(md, 2)
#' 
#' multidensity(dl, adj=1.2, x.leg="topright", frame=FALSE, inset=-0.02, lty=1)
#' multidensity(dl, x.legend="top", horiz=TRUE, cex.legend=0.5,
#'   inset=-0.05, bg.legend="white")

multidensity <- function(x, main, xlab="", ylab="Density", xlim, ylim,
  col=1:9, lty=1:2, lwd=1, add=FALSE, frame.plot=TRUE, legend=TRUE, 
  x.legend="topleft", y.legend=NULL, bty="o", box.col="#FFFFFF00", 
  bg.legend="#FFFFFF88", cex.legend=0.7, x.intersp=1, y.intersp=1.5,
  inset=0, xpd.legend=NA, horiz=FALSE, ...) {
      
    xn <- deparse(substitute(x))
    de <- lapply(x, density, ...)
    ll <- length(x)
    
    if (missing(xlim)) {
        xlim <- range(sapply(de, function(x) range(x$x)))
    }
    if (missing(ylim)) {
        ylim <- c(0, max(sapply(de, function(x) max(x$y))))
    }
    if (missing(main)) {
        call <- de[[1]]$call
        call[[1]] <- quote(density)
        call[[2]] <- as.symbol(xn)
        main <- deparse(call)
    }

    col <- rep(col, length=ll)
    lty <- rep(lty, length=ll)    
    lwd <- rep(lwd, length=ll)
    
    if (!add) {
        plot(de[[1]][1:2], xlim=xlim, ylim=ylim, cex=0, xlab="", 
          ylab="Density", main=main, frame.plot=FALSE)
    }
    q <- lapply(1:ll, 
      function(x) {
          lines(de[[x]][1:2], col=col[x], lty=lty[x], lwd=lwd[x])
      }
    )
    if (frame.plot) box()
    if (legend) {
        leg <- sprintf("%s\nN = %s, bw = %s", 
          names(x),
          lengths(x),
          signif(sapply(de, "[[", "bw"), 3))
        legend(x.legend, y.legend, leg, col=col, lty=lty, lwd=lwd, 
          bty=bty, box.col=box.col, bg=bg.legend, cex=cex.legend,
          x.intersp=x.intersp, y.intersp=y.intersp, inset=inset, 
          xpd=xpd.legend, horiz=horiz)
    }
    invisible(de)
}