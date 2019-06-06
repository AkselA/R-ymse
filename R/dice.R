#' Create, modify or convert from/to dice objects
#' 
#' @param dval an integer vector
#' @param x an arbitrary \code{R} object
#' @param ... further arguments passed to methods
#' 
#' @seealso \code{\link{expand}}, \code{\link{table}}
#' 
#' @examples
#' # Regular d6 dice
#' dice(6)
#' 
#' # d4 dice with sides 0, 1, 2, 4
#' dice(c(0:3))
#' 
#' # d4 dice with two 2s and two 5s
#' dice(c(2, 2, 5, 5))
#' 
#' @export

dice <- function(dval) {
	x <- dval
	if (length(x) == 1) {
		s0 <- rep.int(1L, x)
		sa <- 1
	} else {
		uta <- unique(x)
	
	    sa <- min(uta)
	    sz <- max(uta)
	
	    s <- sa:sz
	    s0 <- rep.int(0L, 1+sz-sa)
	
		tryCatch(
		  s0[s %in% uta] <- table(x),
		   warning=function(w) 
		     stop(paste0("Error in assigning values to indices.\n", 
		     "Does the input contain non-integer values?"), call.=FALSE)
		)
    }
    attr(s0, "bix") <- sa
    class(s0) <- "dice"
    s0	
}

#' @rdname dice
#' @export

is.dice <- function(x, ...) {
	inherits(x, "dice")
}

#' @rdname dice
#' @export

as.dice <- function(x, ...) {
	UseMethod("as.dice")
}

#' @export

as.dice.numeric <- function(x, bix=1, ...) {
    attr(x, "bix") <- bix
    class(x) <- "dice"
    x
}

#' @export

as.dice.table <- function(x, ...) {
	uta <- as.numeric(names(x))

    sa <- min(uta)
    sz <- max(uta)

    s <- sa:sz
    s0 <- rep.int(0, 1+sz-sa)

	tryCatch(
	  s0[s %in% uta] <- x,
	   warning=function(w) 
	     stop(paste0("Error in assigning values to indices.\n", 
	     "Does the input table contain non-integer levels?"), call.=FALSE)
	)

    attr(s0, "bix") <- sa
    class(s0) <- "dice"
    s0
}

#' @export

as.dice.list <- function(x, ...) {
	xo <- lapply(x, 
	  function(y)
	      if (is.dice(y, ...)) {
	          y
	      } else {
	          as.dice(y, ...)
	      }
	)
	xo
}

#' @rdname dice
#' @export

print.dice <- function(x, ...) {
	print(as.vector(x), ...)
	cat("begin index:", attr(x, "bix"), "\n")
}

#' @rdname dice
#' @export

as.table.dice <- function(x, ...) {
    v <- seq_along(x) + attr(x, "bix") - 1
    table(rep(v, times=x))
}

#' Bix attributes
#' 
#' \code{bix} provides access to the bix attribute of a variable. The
#' first form returns the value of the levels of its argument and the second
#' sets the attribute.
#' 
#' @param d a \code{"dice"} object
#' @param value value to begin index at
#' 
#' @export
#' 
#' @examples
#' d <- dice(6)
#' d
#' bix(d)
#' bix(d) <- 3
#' d
#' expand(d)

bix <- function(d) {
	attr(d, "bix")
}

#' @rdname bix
#' @export

`bix<-` <- function(d, value) {
	attr(d, "bix") <- value
	d
}


#' Expand
#' 
#' Expand a \code{"table"}, a \code{"table"}-like object, or a list of 
#' \code{"table"}-like objects
#' 
#' @param x an object to be expanded
#' @param ... further arguments passed to or from methods
#' 
#' @return
#' A vector with values and their repetitions specified by \code{x}
#' 
#' @seealso \code{\link{dice}}, \code{\link{table}}
#' 
#' @export
#' 
#' @examples
#' x <- c(4, 2, 2, 2, 3, 3, 2, 4, 6, 6)
#' (xt <- table(x))
#' (xd <- dice(x))
#' 
#' expand(xt)
#' expand(xd)
#' 
#' expand(list(xt, xd, x))
#' 
#' xn <- as.table(1:4)
#' names(xn) <- LETTERS[1:length(xn)]
#' expand(xn)
#' 

expand <- function(x, ...) {
	UseMethod("expand")
}

#' @export

expand.dice <- function(x, ...) {
	v <- seq_along(x) + attr(x, "bix") - 1
	rep(v, times=x)
}

#' @export

expand.table <- function(x, ...) {
	v <- type.convert(names(x), as.is=FALSE)
	rep(v, times=x)
}

#' @export

expand.list <- function(x, ...) {
	xo <- lapply(x, 
	  function(y) {
	      if (class(y) %in% c("integer", "numeric")) {
	          y
	      } else {
	          expand(y)
	      }
	  }
	)
	xo
}