#' Centre weighted mean
#' 
#' Offering a continuous link between median and arithmetic mean
#' 
#' @param x numeric vector
#' @param p positive numeric narrowness of the weight. \code{1} gives triangular 
#' weighting. Higher values gives narrower weights, approching meadian, lower values 
#' gives broader weights, approaching arithmetic mean
#' @param rank logical. Should should ranks or numeric values determine relative 
#' weights?
#' @param na.rm logical. Should missing values be removed?
#' 
#' @details A weighted arithmetic mean is calculated over the input vector, where
#' most weight is given to the median value(s), and monotonically less towards either
#' extreme. Faloff depends on \code{p}, with small values resulting in a gentler 
#' falloff and less difference between minimum and maximum weights.
#' 
#' @export
#' 
#' @examples
#' x <- c(0, 8, 8, 8, 9)
#' 
#' aug_median(x)
#' 
#' # 0 and 9 are considered equidistant from 8
#' aug_median(x, rank=TRUE)
#' 
#' # Nearly a point weight placed at the median
#' aug_median(x, 100)
#' median(x)
#' 
#' # Nearly uniform weights
#' aug_median(x, 0.001)
#' mean(x)

aug_median <- function(x, p=1, rank=FALSE, na.rm=FALSE) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	r <- x
	if (rank) {
		r <- rank(r)
	}
	r <- -abs(median(r) - r)
	mi <- min(r)
    r <- (r - mi) * (0.9/(max(r) - mi)) + 0.1
    r <- r^p
    sum(x*(r/sum(r)))
}