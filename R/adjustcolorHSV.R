#' Adjust Colors in One or More Directions Conveniently.
#' 
#' Adjust or modify a vector of colors by "turning knobs" on one or more coordinates
#' in (h,s,v,\eqn{\alpha}) space, typically by up or down scaling them.
#' 
#' @param col vector of colors, in any format that col2rgb() accepts
#' @param alpha.f,h.f,s.f,v.f factors scaling the opacity, hue, saturation 
#' and value of \code{col}
#' @param offset a length 4 numeric vector specifying the linear offset applied
#' to the \emph{hue}, \emph{saturation}, \emph{value} and \emph{alpha} values
#' @param transform a 4x4 diagonal matrix specifying the scaling applied to the
#' \emph{hue}, \emph{saturation}, \emph{value} and \emph{alpha} values
#' 
#' @details
#' Essentially an HSV version of the RGB-based \code{\link{adjustcolor}}. One
#' important distinction is that the \code{h.f} value wraps around to fit the [0, 1]
#' range, rather than simply "clamping" it between 0 and 1.
#' 
#' @return
#' A character vector the same length as \code{col} contaning color data in standard
#' hexadeximal RGBA format.
#' 
#' @examples
#' # Halve the saturation and value of the default palette colours
#' plot(2:8, cex=5, lwd=4, pch=21, bg=2:8, 
#'   col=adjustcolorHSV(2:8, s.f=0.5, v.f=0.6))
#' 
#' # Offset the hue of the default palette colours by 0.5, inverting the colours
#' plot(2:8, cex=5, lwd=4, pch=21, bg=2:8, 
#'   col=adjustcolorHSV(2:8, offset=c(0.5, 0, 0, 0)))
#' 
#' @export

adjustcolorHSV <- function(col, alpha.f=1, h.f=1, s.f=1, v.f=1, 
  offset=c(0, 0, 0, 0), transform=diag(c(h.f, s.f, v.f, alpha.f))) {
    stopifnot(exprs = {
        length(offset) %% 4L == 0L
        !is.null(d <- dim(transform))
        d == c(4L, 4L)
    })
    cc <- col2rgb(col, alpha=TRUE)
    x <- rbind(rgb2hsv(cc[1:3,]), cc[4,]/255)
    xt <- transform %*% x + matrix(offset, nrow=4L, ncol=ncol(x))
    xt[1,] <- xt[1,] %% 1L
    x[] <- pmax(0, pmin(1, xt))
    hsv(x[1L, ], x[2L, ], x[3L, ], x[4L, ])
}
