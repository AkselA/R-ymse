#' Align character strings
#' 
#' @param x numeric or character vector
#' @param pattern pattern, passed to \code{regexpr}, whose match the 
#'        character strings will be aligned by
#' @param ... further arguments passed to \code{regexpr}
#' @param lpad,rpad character strings used for padding. Repeated to length
#' 
#' @seealso \code{\link{align_num}}, for alignment more aimed at numeric strings
#' 
#' @export
#' 
#' @examples
#' x <- c("Tom und Jerry", "Abbott og Costello", "Milo och Stich", "et alii")
#' cat(align_char(x, pat="[[:alpha:]]"), sep="\n")
#' cat(align_char(x, pat=" "), sep="\n")
#' cat(align_char(x, pat=" [A-Z]"), sep="\n")
#' cat(align_char(x, pat=" [a-z]"), sep="\n")
#' cat(align_char(x, pat="t", ignore.case=TRUE), sep="\n")
#' cat(align_char(x, pat="x"), sep="\n")

align_char <- function(x, pattern=".", ..., lpad=" ", rpad=" ") {
    op <- options()
    options(scipen=99)
    on.exit(options(op))
    
    x <- as.character(x)
    lpad <- unlist(strsplit(lpad, NULL))
    rpad <- rev(unlist(strsplit(rpad, NULL)))
    
    rex <- regexpr(pattern, x, ...)
    ptm <- rex != -1
    spl <- regmatches(x, rex, invert=TRUE)
    ptm[ptm] <- regmatches(x, rex, invert=FALSE)
    ptm[rex == -1] <- ""
    
    l <- lengths(spl)
    i <- l == 2
    if (sum(i) > 0) {
        spl[!i] <- lapply(spl[!i], function(x) c(x, ""))
        spl <- do.call(rbind, spl)
        nch <- nchar(spl)

        mch <- c(max(nch[,1]), max(nch[,2]))
        rch <- t(mch-t(nch))
                
        bef <- sapply(rch[,1], 
          function(x) paste(rep(lpad, length.out=x), collapse=""))
        aft <- sapply(rch[,2], 
          function(x) paste(rev(rep(rpad, length.out=x)), collapse=""))
        before <- paste0(bef, spl[,1])  
        after <- paste0(ptm, spl[,2], aft)
        cat(paste0(before, after), sep="\n")
    } else {
        message("No matches, returning right justified alignment\n")
        spl <- unlist(spl)
        nch <- nchar(spl)
        rch <- t(max(nch)-t(nch))
        bef <- sapply(rch, function(x) paste(rep(lpad, x), collapse=""))
        paste0(bef, spl)
    }
}

