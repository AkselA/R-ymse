#' HCL rainbow palette
#' 
#' HCL version of \code{rainbow}. Create a vector of \code{n} contiguous colours
#' by specifying a range of Hues, and fixed Chroma and Luminance
#' 
#' @param n number of colours
#' @param c,l the ‘chroma’ and ‘luminance’ to be used to complete the HSV 
#'        color descriptions
#' @param start,end the hue in [0,1] at which the rainbow begins/ends
#' @param alpha the alpha transparency, a number in [0,1], see argument alpha
#'        in \code{\link{hsv}}
#' @param s,v 'saturation' and 'value' passed to \code{\link{adjustcolorHSV}}.
#'        Overrides 'chroma' and 'luminance' if specified.
#' 
#' @export
#' 
#' @examples
#' mat2grid <- function(x) {
#' 	eg <- expand.grid(1:NCOL(x), NROW(x):1)
#' 	gd <- data.frame(eg, c(t(x)), stringsAsFactors=FALSE)
#' 	colnames(gd) <- c("x", "y", "z")
#' 	gd
#' }
#' 
#' n <- 25
#' hcl0 <- rainbowHCL(n)
#' hcl1 <- rainbowHCL(n, c=150, l=85)
#' hcl2 <- rainbowHCL(n, s=1, v=1)
#' hsv0 <- rainbow(n)
#' 
#' cols <- rbind(hcl0, hcl1, hcl2, hsv0)
#' 
#' pos <- mat2grid(cols)
#' plot(pos[,1:2], pch=17, cex=3.5, col=pos[,3], ylim=c(0.5, 4.5))

rainbowHCL <- function (n, c=100, l=75, start=0, end=max(1, n - 1)/n, 
  alpha=1, s=NULL, v=NULL) {
    if ((n <- as.integer(n[1L])) > 0) {
        if (start == end || 
          any(c(start, end) < 0) || 
          any(c(start, end) > 1)) {
            stop("'start' and 'end' must be distinct and in [0, 1].")
        }
        h <- seq.int(start, ifelse(start > end, 1, 0) + end, length.out = n)%%1
        col <- hcl(h*360, c, l, alpha)
        if (is.null(s) + is.null(v) < 2) {
        	col <- adjustcolorHSV(col, s=s, v=v)
        }
        col
    }
    else character()
}
