#' Tabulate data
#' 
#' Quick and simple function for creating contingency tables
#' 
#' @param x a vector or factor object
#' @param na.rm should \code{NA}s be included
#' @param order how should the results be ordered, if any?
#' 
#' @return
#' A \code{data.frame} with columns \code{val} (the original values and class
#' of \code{x}) and \code{freq} (the count, or frequency, of each value in
#' \code{x}, integer). The rows are sorted by frequency in descending order.
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' m <- sample(c(rep(NA, 5), rpois(45, 3)))
#' quick_table(m)
#' 
#' x <- LETTERS[c(2, 2, 2, 2, 3, 1, 1)]
#' quick_table(x, order="freq")
#' quick_table(x, order="value")
#' quick_table(x, order="none")

quick_table <- function(x, na.rm=FALSE, order=c("frequency", "value", "none")) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
    ux <- unique(x)
    freq <- tabulate(match(x, ux))
    dtf <- switch(match.arg(order), 
      frequency=data.frame(val=ux, freq, stringsAsFactors=FALSE)[order(-freq),],
      value=data.frame(val=ux, freq, stringsAsFactors=FALSE)[order(ux),],
      none=data.frame(val=ux, freq, stringsAsFactors=FALSE))
    rownames(dtf) <- NULL
    dtf
}


#' Run Length Encoding
#' 
#' Compute the lengths and values of runs of equal values in a vector
#' 
#' @param x a numeric or character vector
#' @param na.unique should every \code{NA} be conidered unique?
#' @param output what form of output
#' 
#' @return
#' Return value depends on \code{output}. 
#' \describe{
#'   \item{\code{data.frame}}{A data.frame with lengths and values columns}
#'   \item{\code{rle}}{An object of class \code{"rle"}}
#'   \item{\code{named vector}}{A vector of lengths with values as names}
#'   \item{\code{lengths}}{The lengths as a single vector}
#'   \item{\code{values}}{The values as a single vector}
#' }
#' 
#' @export
#' 
#' @examples
#' 
#' x <- c(NA, NA, 1, 2, 3, 3, NA, NA, NA, 2, 2, 2, NA, 1, 1, NA, NA)
#' rle2(x)
#' 
#' m <- matrix(c(
#'   0.7, 0.2, 0.1,
#'   0.2, 0.6, 0.2,
#'   0.1, 0.2, 0.7
#' ), 3, byrow=TRUE)
#' 
#' set.seed(1)
#' y <- LETTERS[markov_seq(n=100, m)]
#' rle2(y, out="named")
#' 
#' 
#' # Same result as rle
#' rle2(x, na.unique=TRUE, output="rle")
#' rle(x)
#' 
#' # inverse.rle works as long as output is "rle"
#' inverse.rle(rle2(x, output="rle"))

rle2 <- function (x, na.unique=FALSE, 
  output=c("data.frame", "rle", "named vector", "lengths", "values")) {
    if (!is.vector(x) && !is.list(x)) 
        stop("'x' must be a vector of an atomic type")
    n <- length(x)
    if (n == 0L) 
        return(structure(list(lengths=integer(), values=x), class="rle"))
    y <- x[-1L] != x[-n]
    if (!na.unique) {
	    yna <- !(is.na(x[-1L]) & is.na(x[-n]))
	    y[is.na(y)] <- TRUE
	    i <- c(which(y & yna), n)
    } else {
    	i <- c(which(y | is.na(y)), n)
    }
    lengths <- diff(c(0L, i))
    values <- x[i]
    if (length(output) > 1) {
    	data.frame(lengths, values, stringsAsFactors=FALSE)
    } else {
    	output <- match.arg(output)
    	switch(output,
    	  "data.frame"=data.frame(lengths, values, stringsAsFactors=FALSE),
    	  "named vector"={
    	      names(lengths) <- values
    	      lengths
    	  },
    	  "rle"=structure(list(lengths=lengths, values=values), class="rle"),
    	  "lengths"=lengths,
    	  "values"=values
    	)
    }
}

