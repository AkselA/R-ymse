#' @export

dice <- function(x) {
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

#' @export

bix <- function(x) {
	attr(x, "bix")
}

#' @export

`bix<-` <- function(x, value) {
	attr(x, "bix") <- value
	x
}

#' @export

is.dice <- function(x, ...) {
	inherits(x, "dice")
}

#' @export

as.dice <- function(...) {
	UseMethod("as.dice")
}

#' @export

as.dice.numeric <- function(x, bix=1, ...) {
    s0 <- x
    attr(s0, "bix") <- bix
    class(s0) <- "dice"
    s0
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
	      if (is.dice(y)) {
	          y
	      } else {
	          as.dice(y)
	      }
	)
	xo
}

#' @export

print.dice <- function(x, ...) {
	print(as.vector(x), ...)
	cat("begin index:", attr(x, "bix"), "\n")
}

#' @export

as.table.dice <- function(x, ...) {
    v <- seq_along(x) + attr(x, "bix") - 1
    table(rep(v, times=x))
}

#' @export

expand <- function(...) {
	UseMethod("expand")
}

#' @export

expand.dice <- function(x, ...) {
	v <- seq_along(x) + attr(x, "bix") - 1
	rep(v, times=x)
}

#' @export

expand.table <- function(x, ...) {
	v <- as.numeric(names(x))
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