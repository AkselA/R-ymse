#' Align numbers
#' 
#' Align numbers for neat vertical printing
#' 
#' @param x numeric or character vector
#' @param lpad,rpad character strings used for padding. Repeated to length
#' @param dec decimal seperator, or any other character to align by
#' @param min.dec pad with zeros to reach a minimum number of decimal points
#' @param rm.dec remove zeros at the end of whole numbers
#' 
#' @seealso \code{\link{align_char}}, for alignment more aimed at character strings
#' 
#' @export
#' 
#' @examples
#' x <- c(22100, 100, 1015, 13.018, 0.1, 0.01234)
#' cat(align_num(x), sep="\n") # Default
#' cat(format(x, scientific=FALSE), sep="\n")
#' 
#' cat(align_num(x, rpad=" "), sep="\n")
#' cat(align_num(x, rpad=" ", rm.dec=FALSE), sep="\n")
#' cat(align_num(x, rpad=" ", min.dec=1), sep="\n")
#' cat(align_num(x, rpad=" ", min.dec=2), sep="\n")
#' 
#' cat(align_num(x, rpad="\U00b7", min.dec=0), sep="\n")
#' cat(align_num(x, lpad="' ", rpad=" '", min.dec=0), sep="\n")
#' 
#' cat(align_num(c("1.000.000", "10.000.000", "1.000,85"), dec=","), sep="\n")
#' 
#' # corner cases
#' x <- c("100.", "1.2", ".1111")
#' cat(align_num(x, rpad=" ", rm.dec=TRUE), sep="\n")
#' cat(align_num(round(as.numeric(x)), rpad=" ", rm.dec=TRUE), sep="\n")
#' 
#' # matching on more than one character
#' # so far not much more advanced than this
#' # working on align_num2 more suited for character strings
#' s <- c("cataract", "hematology", "pancreatic")
#' cat(align_num(s, dec="a", rpad=" "), sep="\n")
#' cat(align_num(s, dec="at", rpad=" "), sep="\n")
#' 
#' a <- c("Tom and Jerry", "Milo and Stich", "Abbott and Costello")
#' cat(align_num(a, dec="and", rpad=" "), sep="\n")

align_num <- function(x, lpad=" ", rpad="0", dec=".",  min.dec=0, rm.dec=TRUE) {
	op <- options()
	options(scipen=99)
	on.exit(options(op))
	
	lpad <- unlist(strsplit(lpad, NULL))
	rpad <- rev(unlist(strsplit(rpad, NULL)))
	rm.dec <- rm.dec & rpad[1] != "0"
	x <- as.character(x)
	rex <- regexpr(dec, x, fixed=TRUE)
	spl <- regmatches(x, rex, invert=TRUE)
	l <- lengths(spl)
	i <- l == 2
	if (sum(i) > 0) {
		spl[!i] <- lapply(spl[!i], function(x) c(x, ""))
	    spl <- do.call(rbind, spl)
	    nch <- nchar(spl)
	    i <- nch[,2] > 0
	    if (min.dec > 0) {
	    	w <- nch[,2] < min.dec
	    	r <- min.dec - nch[w, 2]
	    	z <- sapply(r, function(x) paste(rep("0", x), collapse=""))
	    	spl[w,2] <- paste0(spl[w,2], z)
	    	nch[w,2] <- min.dec
	    	i <- TRUE
	    }
	    mch <- c(max(nch[,1]), max(nch[,2]))
	    rch <- t(mch-t(nch))
	    
	    rch[,2] <- rch[,2] + (!i & rm.dec)
	    
	    bef <- sapply(rch[,1], 
	      function(x) paste(rep(lpad, length.out=x), collapse=""))
	    aft <- sapply(rch[,2], 
	      function(x) paste(rev(rep(rpad, length.out=x)), collapse=""))
	    before <- paste0(bef, spl[,1])  
	    dcv <- ifelse(!(!i & rm.dec), dec, "")
	    after <- paste0(dcv, spl[,2], aft)
	    paste0(before, after)
    } else {
        message("No decimal points, returning right justified align_numment\n")
		spl <- unlist(spl)
	    nch <- nchar(spl)
	    rch <- t(max(nch)-t(nch))
	    bef <- sapply(rch, function(x) paste(rep(lpad, x), collapse=""))
	    paste0(bef, spl)    
    }
}

