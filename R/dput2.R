#' Write an Object to console
#' 
#' Writes an ASCII text representation of an R object to the console for easy
#' copy/paste
#' sharing
#' 
#' @param x an object
#' @param width integer; column width
#' @param assign character; should assignment be included?
#' @param breakAtParen logical; should lines break at parenthesis begins
#' @param compact remove spaces around ' = ' assignments
#' @param exdent a non-negative integer specifying the exdentation of lines
#' after the first. default 2 if assign="front", else 0.
#' 
#' @details This is similar to the way \code{dput} is used to print ASCII
#' representations
#' of objects to the console. The differences are that \code{dput2} lets you specify
#' the width of the resulting column, and assignment of the object to the name used
#' in the call will by default be included. Line breaks are by default only done on
#' whitespace,
#' but can be set to happen at parenthesis begins as well. This should not break code
#' and can make for a more compact representation, but it can also make the code
#' harder to read.
#' 
#' @seealso \code{\link{dput}}, \code{\link{deparse}}
#' 
#' @export
#' 
#' @examples
#' xmpl <- faithful[sort(sample(1:nrow(faithful), 50)), ]
#' dput(xmpl)
#' cat(deparse(xmpl, width.cutoff=65), sep='\n')
#' dput2(xmpl, compact=FALSE)
#' dput2(xmpl)
#' dput2(xmpl, assign="end")
#' dput2(xmpl, assign="none")
#' dput2(xmpl, 80)
#' 
#' # no line breaks on whitespaces or parens within character strings
#' xmpl <- mtcars[1:5, ]
#' rownames(xmpl) <- c("bbbb (hhhhhhh\u00A0hhhhhhhh)", 
#'                     " rrrrrrrr ( bbbbbbb )",
#'                     "v v v v v v v v v v",
#'                     "(  g-god, d-god, _-___)",
#'                     "100*(part)/(total)")
#' dput2(xmpl, 15)
#' dput2(xmpl, 15, breakAtParen=TRUE)

dput2 <- function(x, width=65, assign=c("front", "end", "none"), 
  breakAtParen=FALSE, compact=TRUE, exdent=NULL) {
  	x1 <- x
  	attr(x1, ".internal.selfref") <- NULL # in case data.table etc.
    assign <- match.arg(assign)
    if (is.null(exdent)) {
        exdent <- 2*(assign == "front")
    }
    dep <- switch(assign,
      "front" = {
          asg <- paste(deparse(substitute(x)), "<- ")
          c(asg, deparse(x1, width.cutoff=500))
      },
        "end" = {
          asg <- paste(" ->", deparse(substitute(x)))
          c(deparse(x1, width.cutoff=500), asg)
      },
       "none" = {
          deparse(x1, width.cutoff=500)
      })
    if (compact) {
        dep <- gsub(" = ", "=", dep)
    }
    dep <- paste(dep, collapse="")
    
    if (width > 0) {    
        if (breakAtParen) {
            dep <- gsub("\\(", "\\( ", dep)
            
            # remove spaces after parens in character strings
            # and replace the remaining spaces with nbsp
            spl <- strsplit(dep, "\"")[[1]]
            chstr <- 1:length(spl) %% 2 == 0
            spl[chstr] <- gsub("\\( ", "\\(", spl[chstr])
            spl[chstr] <- gsub(" ", "\u00A0", spl[chstr])
            dep <- paste(spl, collapse="\"")
            
            dep <- strwrap(dep, width=width + 1, exdent=exdent)
            dep <- gsub("\\( ", "\\(", dep)
        } else {
            # replace spaces in character strings with nbsp
            spl <- strsplit(dep, "\"")[[1]]
            chstr <- 1:length(spl) %% 2 == 0
            spl[chstr] <- gsub(" ", "\u00A0", spl[chstr])
            dep <- paste(spl, collapse="\"")
            dep <- strwrap(dep, width=width + 1, exdent=exdent)
        }
    }
    cat(dep, sep="\n")
}