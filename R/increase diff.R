#' Increase difference
#' 
#' Rearrange a sorted numeric sequence so that the difference between subsequent 
#' elements is increased
#' 
#' @param x a numeric sequence
#' @param step how long a step the difference is considered for. 
#' 
#' @details
#' With \code{step=2} (default) only the difference between immediate neighbours are
#' considered; the difference between every second element will remain small, 
#' or rather reduced, compared to the original sequence. With \code{step=3} say, 
#' differences of both lag 1 and 2 is increased, but the difference of lag 1 will
#' be less than if a step of 2 was used.
#' 
#' @export
#' 
#' @examples
#' x <- 1:100
#' diff(x)
#' 
#' diff(incdiff(x, 2))
#' diff(incdiff(x, 3))
#' 
#' diff(incdiff(x, 2), 2)
#' diff(incdiff(x, 3), 2)
#' 
#' # incdiff will introduce a periodicity equal to the step length
#' acf(incdiff(x, 10))
#' 
#' # useful for making a sequence of colours more distinct
#' y <- seq(0.4, 1, l=18)
#' cols1 <- hsv(y, 1, y)
#' cols2 <- hsv(y, 1, incdiff(y, 3))
#' 
#' plot(y, col=cols1, pch=16, cex=5, ylim=c(0.4, 1.5))
#' points(y+0.5, col=cols2, pch=16, cex=5)

incdiff <- function(x, step=2) {
	len <- length(x)
	length(x) <- len + (-len %% step)
	mat <- matrix(x, nrow=step, byrow=TRUE)
	y <- c(mat)
	y[!is.na(y)]
}