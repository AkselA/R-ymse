#' Write an Object to console
#' 
#' Writes an ASCII text representation of an R object to the console
#' 
#' @param x an object
#' @param width integer; column width
#' @param assign character; should assignment be included?
#' 
#' 
#' @details This is similar to the way \code{dput} is used to print ASCII representations
#' of objects to the console. The differences are that \code{dput2} lets you specify
#' the width of the resulting column, and assignment of the object to the name used in
#' the call will by default be included. Line breaks are handled by \code{strwrap}, which
#' only breaks at whitespaces. This means that some possible long construcs, like
#' "structure(list(eruptions" will remain in one piece, even though they in theory could have
#' been broken up further.
#' 
#' @export
#' 
#' @examples
#' xmpl <- faithful[sort(sample(1:nrow(faithful), 50)), ]
#' 
#' dput(xmpl)
#' cat(deparse(xmpl, width.cutoff=65), sep='\n')
#' 
#' dput2(xmpl, 65)
#' dput2(xmpl, 65, assign="end")
#' dput2(xmpl, 80, assign="none")
#' dput2(xmpl[1:10,], 10, "none")

dput2 <- function(x, width=65, assign=c("front", "end", "none")) {
	assign <- match.arg(assign)
	dep <- switch(assign,
      "front" = {
          asg <- paste(deparse(substitute(x)), "<- ")
          c(asg, deparse(x, width.cutoff=500))
      },
        "end" = {
          asg <- paste(" ->", deparse(substitute(x)))
          c(deparse(x, width.cutoff=500), asg)
      },
       "none" = {
          deparse(x, width.cutoff=500)
      })
	dep <- paste(dep, collapse="")
	
	if (width != 0) {
		dep <- strwrap(dep, width=width + 1)
	}
	
	cat(dep, sep="\n")
}