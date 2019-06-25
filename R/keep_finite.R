#' Keep finite values
#' 
#' Remove \code{NA}s code{NaN}s and code{Inf}s from data
#' 
#' @param x a vector or matrix
#' @param margin if \code{x} is matrix, which margin to keep finites by
#' @param keep if \code{x} is matrix, keep rows/columns with any finite
#' values, or keep only complete rows/columns.
#' @param ... further arguments passed to methods
#' 
#' @return
#' If \code{x} is a matrix and \code{margin} is 1 or 2, a matrix is returned.
#' Else a vector.
#' 
#' @export
#' 
#' @examples
#' m1 <- matrix(c(10, 20, 30, 43,
#'                10, NA, 32, 50,
#'                NA, NA, NA, NA,
#'                13, 22, 70, 81,
#'                NA, 29, NA, 41), 5, byrow=TRUE,
#'                dimnames=list(letters[1:5], LETTERS[1:4]))
#' 
#' keep_finite(m1) 
#' matplot(keep_finite(apply(m1, 2, sort, na.last=TRUE)), type="l")
#' 
#' m1[complete.cases(m1),]
#' keep_finite(m1, 1, "c") #same
#' keep_finite(m1, 2, "complete") #no complete columns
#' 
#' m1.df <- as.data.frame(t(m1))
#' keep_finite(m1.df, 2, "complete")

keep_finite <- function(x, ...) {
    UseMethod("keep_finite")
}

#' @rdname keep_finite
#' @export

keep_finite.default <- function(x, ...) {
    x[is.finite(x)]
}

#' @rdname keep_finite
#' @export

keep_finite.matrix <- function(x, margin=1, keep=c("any", "complete"), ...) {
    if (!((length(margin) == 1) & margin %in% 1:2)[1]) {
        stop("margin must be one of 1 or 2")
    }
    nf <- !is.finite(x)
    keep <- match.arg(keep)
    if (margin == 1) {
        r <- switch(keep,
          any=x[rowSums(!nf) != 0, , drop=FALSE],
          complete=x[rowSums(nf) == 0, , drop=FALSE])
        return(r)
    }
    if (margin == 2) {
        r <- switch(keep,
          any=x[, colSums(!nf) != 0, drop=FALSE],
          complete=x[, colSums(nf) == 0, drop=FALSE])
        return(r)
    }
}

#' @rdname keep_finite
#' @export

keep_finite.data.frame <- function(x, margin=1, keep=c("any", "complete"), ...) {
    if (!((length(margin) == 1) & margin %in% 1:2)[1]) {
        stop("margin must be one of 1 or 2")
    }
    nf <- !sapply(x, is.finite)
    keep <- match.arg(keep)
    if (margin == 1) {
        r <- switch(keep,
          any=x[rowSums(!nf) != 0, ],
          complete=x[rowSums(nf) == 0, ])
        return(r)
    }
    if (margin == 2) {
        r <- switch(keep,
          any=x[, colSums(!nf) != 0, drop=FALSE],
          complete=x[, colSums(nf) == 0, drop=FALSE])
        return(r)
    }
}
