% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rainbowHCL.R
\name{rainbowHCL}
\alias{rainbowHCL}
\title{HCL rainbow palette}
\usage{
rainbowHCL(n, c = 100, l = 75, start = 0, end = max(1, n - 1)/n,
  alpha = 1, s = NULL, v = NULL)
}
\arguments{
\item{n}{number of colours}

\item{c, l}{the ‘chroma’ and ‘luminance’ to be used to complete the HSV 
color descriptions}

\item{start, end}{the hue in [0,1] at which the rainbow begins/ends}

\item{alpha}{the alpha transparency, a number in [0,1], see argument alpha
in \code{\link{hsv}}}

\item{s, v}{'saturation' and 'value' passed to \code{\link{adjustcolorHSV}}.
Overrides 'chroma' and 'luminance' if specified.}
}
\description{
HCL version of \code{rainbow}. Create a vector of \code{n} contiguous colours
by specifying a range of Hues, and fixed Chroma and Luminance
}
\examples{
mat2grid <- function(x) {
	eg <- expand.grid(1:NCOL(x), NROW(x):1)
	gd <- data.frame(eg, c(t(x)), stringsAsFactors=FALSE)
	colnames(gd) <- c("x", "y", "z")
	gd
}

n <- 25
hcl0 <- rainbowHCL(n)
hcl1 <- rainbowHCL(n, c=150, l=85)
hcl2 <- rainbowHCL(n, s=1, v=1)
hsv0 <- rainbow(n)

cols <- rbind(hcl0, hcl1, hcl2, hsv0)

pos <- mat2grid(cols)
plot(pos[,1:2], pch=17, cex=3.5, col=pos[,3], ylim=c(0.5, 4.5))
}
