# Cramer's V

cramer <- function(x, y) {
	if (!is.factor(x)) {
		levels <- sort(unique.default(x))
        f <- match(x, levels)
        levels(f) <- as.character(levels)
        class(f) <- "factor"
		x <- f
	} 
	if (!is.factor(y)) {
		levels <- sort(unique.default(y))
        f <- match(y, levels)
        levels(f) <- as.character(levels)
        class(f) <- "factor"
		y <- f
	} 
    K <- nlevels(y)
    L <- nlevels(x)
    n <- length(y)
    chi2 <- chisq.test(x, y, correct=FALSE)
    st <- sqrt(chi2$statistic/(n*min(K-1, L-1)))
    st[[1]]
}

#' Similarity matrix
#' 
#' Create a similarity matrix
#' 
#' @param x an object containing the values the similarity matrix should be
#' computed for
#' @param y same as \code{x}. If given the union of values in \code{x} and
#' \code{y} are used, if not the unique values of \code{x} are used
#' @param s a vector for filling the matrix. By default producing an identity
#' matrix
#' @param byrow should s fill the matrix by row?
#' 
#' @return
#' A square matrix with the values of \code{s} and row-/colnames of the unique
#' values in \{x, y\}.
#' 
#' @seealso \code{\link{similarity}}
#' 
#' @examples
#' smat(1:3)
#' 
#' smat(c("f", "e", "d"), s=c(
#' 	4, 1, 1,
#' 	1, 3, 2,
#' 	1, 2, 3
#' ))
#' 
#' @export

smat <- function(x, y, s, byrow=FALSE) {
	if (missing(y)) {
	    u <- unique.default(unlist(x))
	} else {
	    u <- union(x, y)
	}
	lu <- length(u)
	if (!missing(s)) {
		m <- matrix(s, lu, lu, dimnames=list(u, u), byrow=byrow)
	} else {
	    m <- diag(length(u))
	    dimnames(m) <- list(u, u)
	}
	m
}

#' Similarity measure
#' 
#' Calculate the similarity between two character vectors based on a
#' similarity matrix
#' 
#' @param x a character vecor or two-column data.frame/matrix
#' @param y a character vector. Ignored if \code{x} is data.frame/matrix
#' @param sm a similarity matrix. By default a unit matrix
#' @param sfun function used to summarise the elementwise similarities
#' @param ... further arguments passed to \code{sfun}
#' 
#' @seealso \code{\link{smat}}
#' 
#' @examples
#' # In its most basic form similarity() gives the Hamming distance
#' similarity(c(1, 0, 1, 0), c(1, 1, 0, 0))
#' 
#' 
#' # Symmetry not required. 
#' bef <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
#' aft <- c(0, 2, 2, 1, 2, 2, 1, 1, 2)
#' 
#' # Here a decrease in value of 1 is considered
#' # more similar than an increase in value of 1.
#' sm1 <- t(structure(c(
#' 3, 0, 0, 0, 
#' 2, 3, 0, 0, 
#' 0, 2, 3, 0,
#' 0, 0, 2, 3), 
#' .Dim=c(4L, 4L), 
#' .Dimnames=list(c("0", "1", "2","3"), c("0", "1", "2", "3"))))
#' 
#' # Symmetric version
#' sm2 <- t(structure(c(
#' 3, 1, 0, 0, 
#' 1, 3, 1, 0, 
#' 0, 1, 3, 1,
#' 0, 0, 1, 3), 
#' .Dim=c(4L, 4L), 
#' .Dimnames=list(c("0", "1", "2","3"), c("0", "1", "2", "3"))))
#' 
#' similarity(bef, aft, sm1)
#' similarity(bef, aft, sm2)
#' 
#' # Pre-aligned fragments of insulin genes
#' data(insulin)
#' 
#' # Transition-transversion matrix
#' data(smt)
#' 
#' # Using pairwise() to run similarity() over all column pairs
#' pairwise(insulin, similarity, smt, sfun=mean)
#' 
#' # Imagined result from questionnaire
#' qu <- data.frame(
#'   Alice=c("happy", "sad", "angry", "unsure", "happy", "sad", "happy", "angry"),
#'   Bob=c("happy", "sad", "angry", "angry", "happy", "angry", "angry", "sad"),
#'   Charlie=c("sad", "sad", "unsure", "unsure", "happy", "sad", "angry", "sad"),
#'   stringsAsFactors=FALSE
#' ) 
#' 
#' # Similarity matrix describing the relative similitudes of the moods
#' emsm <- as.matrix(read.table(text="
#'        happy  sad  angry unsure
#'  happy   5     0     1     1
#'    sad   0     5     2     1
#'  angry   1     2     4     2
#' unsure   1     1     2     3",
#' header=TRUE))
#' 
#' pairwise(qu, similarity, sm=emsm/5, sfun=mean)
#' 
#' @export

similarity <- function(x, y, sm=smat(x, y), sfun=sum, ...) {
	sfun <- match.fun(sfun)
	if (NCOL(x) > 1) {
		xo <- x
	    x <- xo[,1]
	    y <- xo[,2]
	}
    sfun(sm[cbind(x, y)], ...)
}

# https://en.wikipedia.org/wiki/Needleman–Wunsch_algorithm


