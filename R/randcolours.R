#' Random colours
#' 
#' Generate a randomly selected colour palette
#' 
#' @param n number of colours
#' @param l lightness range
#' @param c1 colour channel one range
#' @param c2 colour channel two range
#' @param alpha alpha channel range
#' @param space should the parameters be interpreted as Luv or Lab components?
#' 
#' @details
#' The range of \code{l}, \code{c1}, \code{c2} and \code{alpha}, will be 
#' interpreted as the wanted range of each colour component, whether their length
#' is 1, 2, or more. Although they all should nominally lie within [0, 1], only 
#' \code{alpha} must do so to achieve a valid output. The others can exeed this
#' range, at an icreased risk of clipping.
#' 
#' @export
#' 
#' @examples
#' set.seed(3)
#' n <- 20
#' plot(1:n, col=randcolors(n), pch=16, cex=5)

randcolours <- function(n, l=c(0.2, 1), c1=c(0, 1), c2=c(0, 1), alpha=1, 
  space=c("Luv", "Lab")) {
  	
  	space <- match.arg(space)
	
	p0 <- list(l, c1, c2, alpha)
	p <- sapply(p0, function(x) runif(n, min(x), max(x)))
	
	p[,1] <- fitrange(incdiff(sort(p[,1]), max(3, n %/% 4)), min(l), max(l))
    p[,2] <- fitrange(sort(p[,2]), min(c1), max(c1))
    p[,3] <- fitrange(incdiff(sort(p[,3]), n %/% 2), min(c2), max(c2))
    p[,4] <- fitrange(incdiff(sort(p[,4]), max(2, n %/% 3)), min(alpha), max(alpha))
    
    p[,1] <- p[,1]*100
    p[,2] <- (p[,2] - 0.5)*200
    p[,3] <- (p[,3] - 0.5)*200
	
	rgb.c <- convertColor(p[,1:3], from=space, to="sRGB")
    rgb(rgb.c[, 1], rgb.c[, 2], rgb.c[, 3], p[,4])
}