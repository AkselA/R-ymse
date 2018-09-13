#' Information entropy
#' 
#' Computes the information entropy (also called Shannon entropy) of a set of discrete values,
#' or a tabulated such set.
#' 
#' @param x a vector, table, data.frame or matrix. In the case of table, data.frame and matrix
#' each row is treated as a separate set of counts or proportions, with columns representing
#' species, types, categories etc.
#' @param base the log base to be used.
#' 
#' @export
#' 
#' @examples
#' entropy(c(5, 5, 4, 4, 2, 3, 5))  # default is unit bits
#' entropy(c(5, 5, 4, 4, 2, 3, 5), base=exp(1))  # unit nats
#' 
#' entropy(rep(1:4, 1:4), 4)
#' entropy(rep(1:4, 1), 4)
#' 
#' entropy(as.factor(c(1, 1, 2, 3, 4, 4)))
#' entropy(as.character(c(1, 1, 2, 3, 4, 4)))
#' 
#' mtctab <- table(mtcars$cyl, mtcars$carb)
#' entropy(mtctab, 6)
#' 
#' xx <- data.frame(bee=c(0, 0, 1, 2, 3, 2, 0, 3),
#'                 wasp=c(1, 3, 2, 0, 1, 1, 2, 1),
#'                   fly=c(1, 2, 4, 2, 1, 0, 1, 0),
#'               beetle=c(1, 0, 0, 1, 2, 2, 0, 2),
#'             butterfly=c(0, 0, 0, 0, 3, 1, 0, 1))
#' 
#' entropy(xx)
#' 
#' @name entropy

NULL

entropy <- function(...) {
    UseMethod("entropy")
}

#' @rdname entropy

entropy.table <- function(x, base=2) {
    if (length(dim(x)) == 2) {
        entropy.matrix(x)
    }
    x <- x[x != 0]
    freqs <- x / sum(x)
    -sum(freqs * log(freqs, base=base))
}

#' @rdname entropy

entropy.data.frame <- function(x, base=2) {
    x <- as.matrix(x)
    sc <- x / rowSums(x)
    lg <- log(sc, base)
    lg[is.infinite(lg)] <- 0
    -rowSums(sc * lg)
}

#' @rdname entropy

entropy.matrix <- function(x, base=2) {
    sc <- x / rowSums(x)
    lg <- log(sc, base)
    lg[is.infinite(lg)] <- 0
    -rowSums(sc * lg)
}

#' @rdname entropy

entropy.default <- function(x, base=2) {
    freqs <- table(x) / length(x)
    -sum(freqs * log(freqs, base=base))
}