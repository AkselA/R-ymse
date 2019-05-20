#' Flatten list
#' 
#' Flatten a (nested) list to a list of its leaves
#' 
#' @param x a list object
#' @param flatten.df should \code{data.frame}s also be flattened?
#' @param keep.order keep the order of the original list, same as seen when 
#' using \code{str}
#' 
#' @details
#' The nodes of the supplied list is traversed from root to leaf and successively
#' unlisted until no lists are left (except possibly for \code{data.frame}s).
#' 
#' @return
#' A single level list of \code{x}'s leaves.
#' 
#' @export
#' 
#' @examples
#' xl <- list(
#'   O=NA, 
#'   R=list(
#'     j=1:3,
#'     h="(a)",
#'     q=data.frame(
#'       a=1:2, 
#'       b=c("A, K", "B, L"),
#'       stringsAsFactors=FALSE
#'     )
#'   ), 
#'   N=1,
#'   L=FALSE
#' )
#' 
#' flatten(xl, flatten.df=TRUE, keep.order=FALSE)
#' flatten(xl, flatten.df=TRUE, keep.order=TRUE)
#' str(xl)

flatten <- function(x, flatten.df=FALSE, keep.order=TRUE) {
	if (!keep.order) {
		.flatten(x, flatten.df)
	} else {
		i <- 0
		f <- function(x) {
			i <<- i + 1
			i
		}
		
		ix.l <- rapply(x, f, how="replace")
		ix.f <- .flatten(ix.l, flatten.df)
		ix <- sapply(ix.f, function(x) as.numeric(x)[1])
		
        .flatten(x, flatten.df)[order(ix)]
	}
}

.flatten <- function(x, flatten.df=FALSE) {
    if (flatten.df) {
        morelists <- sapply(x, is.list)
    } else {
        morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    }
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if (sum(morelists)){ 
        Recall(out, flatten.df)
    } else {
        out
    }
}
