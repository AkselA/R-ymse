#' Latin square
#' 
#' Generate latin squares, either randomly or ordered
#' 
#' @param n integer; number of unique values (aka. symbols)
#' @param random logical; should the square be generated randomly?
#' @param redude logical; should the square be in reduced form?
#' 
#' @details
#' Computation time increses rapidly with \code{n}. On my computer generating a
#' random square with \code{n=12} takes about ten minutes, marking the upper 
#' limit of practicability, or even stretching it a little.
#' A latin square in \code{reduced} form will have elements in the first row
#' and the first column in a sorted order. By setting \code{reduced=TRUE} the first
#' row and the first column will always be \code{1:n}.
#' 
#' @return
#' A square integer matrix of size n^2
#' 
#' @seealso \code{\link{indexvalue}}
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' ls <- latin.sq(10, reduce=TRUE)
#' image(ls, col=randcolours(ncol(ls)))
#' 
#' # The more "classic" representation with latin capital letters
#' ls[] <- LETTERS[ls]
#' ls

latin.sq <- function(n, random=TRUE, reduce=TRUE) {
	n <- as.integer(n)
	x <- 1:n
	if (!reduce) {
		x <- sample(x)
	}
	if (!random) {
        return(embed(rep(rev(x), 2), n)[1:n, ])
    }
    m <- matrix(, n, n)
    m[1, ] <- x

    np1 <- n+1

    for (i in 2:n) {
    	j <- 1L
    	tries <- 1L
    	ca <- sample(x)
	    while (j < np1) {
	        m[i, j] <- ca[j]
	        if (!any(m[i, j] - m[1:(i - 1), j] == 0L)) {
	        	j <- j + 1L
	        } else {
	        	if (tries > n - j) {
	        	   j <- 1L
	        	   tries <- 1L
	        	}
	        	ca[j:n] <- sample(ca[j:n])
	        	tries <- tries + 1L
	        }
	    }
    }
    if (reduce) {
    	m <- m[order(m[, 1]), ]
    }
    m
}

#' Index–value representation of arrays
#' 
#' Represent an array as columns of dimensional indices and value
#' 
#' @param x an array or something that can be coerced into an array
#' @param reverse logical; convert from Index–value representation to regular
#' array representation?
#' 
#' @details
#' An n-dimensional array will be unfolded to a n+1-column data.frame where
#' the first n columns represent the indices of the n dimensions, and the last 
#' column gives the value found at each index tuple. The reverse process can
#' also be performed.
#' 
#' @seealso \code{\link{latin.sq}}
#' 
#' @export
#' 
#' @examples
#' arr <- array(1:(2*3*4), dim=c(2, 3, 4))
#' arr.is <- indexvalue(arr)
#' 
#' # can be used to permutate an array
#' indexvalue(arr.is[,c(2, 1, 3, 4)], rev=TRUE)
#' aperm(arr, c(2, 1, 3))
#' 
#' # can interpret values (symbols) as dimensional indices and permute them as well
#' arr2 <- array(rep(1:6, 4), dim=c(2, 3, 4))
#' arr2.is <- indexvalue(arr2)
#' indexvalue(arr2.is[,c(1, 2, 4, 3)], rev=TRUE)
#' 
#' # a latin square will produce an "orthogonal array"
#' set.seed(1)
#' lsq <- latin.sq(5)
#' iv <- indexvalue(lsq)
#' iv
#' 
#' # any permutation of a latin square is also a latin square
#' indexvalue(iv[, c(1, 3, 2)], reverse=TRUE)

indexvalue <- function(x, reverse=FALSE) {
	if (reverse) {
		x <- as.data.frame(x)
		nc <- ncol(x)
		ord <- do.call(order, as.list(x[, (nc-1):1]))
		x <- x[ord, ]
		dm <- apply(x[,-nc], 2, function(y) length(unique(y)))
		array(x[, nc], dm)
	} else {
        if (is.data.frame(x)) {
		    x <- as.matrix(x)
	    }
        l <- lapply(dim(x), function(y) seq.int(1, y))
        eg <- do.call(expand.grid, l)
        colnames(eg) <- sub("Var", "dim", colnames(eg))
        cbind(eg, val=c(x))
    }
}