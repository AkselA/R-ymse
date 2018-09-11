#' Write an Object to console
#' 
#' Writes an ASCII text representation of an R object to the console
#' 
#' @param x r object
#' @param width width of column
#' @param assign should assignment to object go in front of or at the end of the object,
#' or none at all?
#' 
#' @details This is similar to the way \code{dput} is used to print ASCII representations
#' of objects to the console. The differences are that \code{dput2} lets you specify
#' column width, and assignment to the object is an option.
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
		dep <- strwrap(dep, width=width)
	}
	
	cat(dep, sep="\n")
}