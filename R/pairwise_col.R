#' Apply function to columns pairwise
#' 
#' Pairwise application of a function to the columns of a matrix or data.frame
#' 
#' @param x a matrix or data.frame
#' @param FUN any function that takes two vectors as input and returs a single 
#' value
#' @param ... further arguments passed to FUN
#' @param comm logical; is FUN commutative? If true, only the lower
#' triangle, including the diagonal, is computed
#' 
#' @return An \eqn{n\times n}{n×n} square matrix with \eqn{n} the number of columns
#' of \code{x}.
#' 
#' @examples
#' dtf <- data.frame(aa=c(1, 1, 2, 2, 3, 2, 4), 
#'                   bb=c(1, 1, 2, 3, 3, 3, 4),
#'                   cc=c(3, 3, 2, 1, 1, 1, 1),
#'                   dd=c(1, 2, 2, 2, 1, 1, 2))
#' 
#' # Root Mean Square Deviation
#' pairwise_col(dtf, function(x, y) sqrt(mean((x-y)^2)))
#' 
#' # using with cor.test() to accompany cor()                  
#' pv <- pairwise_col(dtf, function(x, y) cor.test(x, y)$p.val)
#' pvn <- 6^(1.1-pv)-5
#' pvn[pvn<1] <- 1
#' 
#' set_mar(1, 1, 1, 1)
#' plot(0, xlim=c(0.5, 4.5), ylim=c(0.5, 4.5), cex=0, ann=FALSE, xaxt="n", yaxt="n")
#' text(rep(1:4, 4), rep(4:1, each=4), t(round(cor(dtf), 2)), cex=pvn, 
#'   col=c("black", "darkgrey")[(pv>0.1)+1])
#' 
#' # Jaccard index
#' jacc <- function(x, y) length(intersect(x, y))/length(union(x, y))
#' pairwise_col(dtf, jacc)
#' 
#' @export

pairwise_col <- function(x, FUN, ..., comm=FALSE) {
	
	nc <- ncol(x)
	cnames <- colnames(x)
	FUN <- match.fun(FUN)
	
	if (comm) {
		cb <- t(combn(nc, 2))
		nr <- nrow(cb)
		v <- vector(length=nr)
        for (i in 1:nr) {
        	cc <- FUN(x[, cb[i,1]], x[, cb[i,2]], ...)
	        v[i] <- cc
        }
		m <- matrix(1, nc, nc)
		m[lower.tri(m)] <- v
		m <- m*t(m)
		diag(m) <- sapply(1:nc, function(j) FUN(x[,j], x[,j], ...))
	} else {
		eg <- expand.grid(1:nc, 1:nc)
		nr <- nrow(eg)
		v <- vector(length=nr)
		for (i in 1:nr) {
			cc <- FUN(x[,eg[i,1]], x[,eg[i,2]], ...)
			v[i] <- cc
		}
		m <- matrix(v, nc, byrow=TRUE)
	}
	dimnames(m) <- list(cnames, cnames)
	m
}
