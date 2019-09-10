#' Render R example
#' 
#' Render example from R code stored on the clipboard
#' 
#' @details
#' Similar to \code{reprex::reprex()} or \code{knitr::spin(text=, envir=new.env(), 
#' report=FALSE)}, but stripped down to the very basics. Input is plain valid R
#' code taken from the clipboard. It is run in a fresh environment and both 
#' commands and results are catured. Commands are kept as is, but results are 
#' commented out. Instead of using three backticks to indicate code for markdown, 
#' each line has four whitespaces prepended. \code{fingerknit} output is also
#' valid R code.
#' 
#' @return
#' The clipboard is used for both input and output, but the output is also
#' returned invisibly as a character string. Warnings and errors are not captured,
#' but printed to console as normal. If an error is encountered nothing is returned
#' and the clipboard data remains unchanged. 
#' 
#' @export

fingerknit <- function() {
 	cbcopy <- pipe('printf "%s\n" "$(pbpaste)"')
    cblines <- readLines(cbcopy)
    close(cbcopy)
    cblines[nchar(cblines) == 0] <- "###! EMPTY LINE PLACEHOLDER !###"
    cblines <- gsub("\t", "    ", paste(cblines, collapse="\n"))
    txtcon <- textConnection(cblines)
    input <- capture.output(
      source(txtcon, local=new.env(), echo=TRUE, spaced=FALSE,
        max.deparse.length=9999, width.cutoff=80)
    )
    close(txtcon)
    opt.pmt <- options()$prompt
    opt.cnt <- options()$continue
    pmt <- startsWith(input, opt.pmt) 
    cnt <- startsWith(input, opt.cnt) 
    out <- !(pmt | cnt)
    input[pmt] <- substring(input[pmt], nchar(opt.pmt)+1)
    input[cnt] <- substring(input[cnt], nchar(opt.cnt)+1)
    input[out] <- paste("#", input[out])
    input[input == "###! EMPTY LINE PLACEHOLDER !###"] <- ""
    input <- paste("   ", input)
    
    cbpaste <- pipe("pbcopy", "w")                           
    writeLines(input, con=cbpaste)
    close(cbpaste)
    
    invisible(paste(input, collapse="\n"))
}


#' Apply function to contents of clipboard
#' 
#' Read in clipboard contents as lines, apply a function on them, and write
#' results back to the clipboard
#' 
#' 
#' @param FUN function to be applied
#' @param ... optional arguments to \code{FUN}
#' @param collapse collapse the lines into a single string separated by newlines
#' @param write write the results back to the clipboard
#' @param eval parse and evaluate the results
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' # Copy to clipboard
#' a <- 10
#' b <- 20
#' s <- a + b
#' s
#' # end
#' 
#' # Run
#' ev <- cbapply(FUN=function(x) paste(x, "+ 2"), eval=TRUE)
#' ev; a; b; s
#' 
#' # Clipboard contents changed to
#' a <- 10 + 2
#' b <- 20 + 2
#' s <- a + b + 2
#' s + 2
#' # end
#' 
#' # Copy to clipboard
#' One Two
#' Three
#' # end
#' 
#' # Run
#' cbapply(FUN=toupper, write=FALSE)
#' 
#' # Clipboard contents unchanged
#' One Two
#' Three
#' # end
#' }

cbapply <- function(FUN, ..., collapse=FALSE, write=TRUE, eval=FALSE) {
 	cbcopy <- pipe("pbpaste")
    input <- readLines(cbcopy)
    close(cbcopy)
    if (nchar(input[(l <- length(input))]) == 0) {
    	input <- input[seq_len(l-1)]
    }
    if (collapse) {
    	input <- paste(input, collapse="\n")
    }
    
    if (!missing(FUN)) {
        FUN <- match.fun(FUN)
        output <- FUN(input, ...)
    } else {
    	output <- input
    }

    if (write) {
	    cbpaste <- pipe("pbcopy", "w")                           
	    writeLines(output, con=cbpaste)
	    close(cbpaste)
    }
        
    if (!isFALSE(eval)) {
    	if (is.environment(eval)) {
    		env <- eval
    	} else {
    		env <- parent.frame()
    	}
    	eval(parse(text=output), envir=env)
    } else {
    	if (write) {
            invisible(output)
    	} else {
           output
        }
    }
}
