#' Caleidoscopic effect on a matrix
#' 
#' Flip a matrix vertically and horizontally before recombining into a new large matrix
#' 
#' @param m a matrix
#' @param odd logical; should the resulting matrix have odd dimensions?
#' 
#' @details Three copies of \code{m} will be made. One flipped horizontally, one flipped
#' vertically, and one flipped both horizontally and vertically. Then they are recombined
#' with the original matrix in the upper right corner, and the flipped copies in the
#' upper left, lower righ and lower left corners, respectively.
#' 
#' @return A matrix of either \eqn{2\times}{2×} or \eqn{2\times - 1}{2× - 1} the number
#' of rows and columns of the input matrix.
#' 
#' @export
#' 
#' @examples
#' caleidoscope(matrix(1:4, 2), odd=FALSE)
#' 
#' image(caleidoscope(1:9 %o% 1:9))
#' 
#' image(caleidoscope(matrix(runif(180*200)^2, 180)), col=rainbow(256, start=0.58))

caleidoscope <- function(m, odd=TRUE) {
	nc <- ncol(m)
	nr <- nrow(m)
	if (odd) {
    	cn2 <- m[, c(1:nc, (nc - 1):1)]
	    cn2[c(1:nr, (nr - 1):1), ]
	} else {
    	cn2 <- m[, c(1:nc, nc:1)]
	    cn2[c(1:nr, nr:1), ]
	}
}

