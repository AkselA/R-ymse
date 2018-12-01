#' Generalized means
#' 
#' Harmonic, geometric, quadratic, cubic, power and Lehmer means.
#' 
#' @param x numeric vector of values whose *mean is to be computed
#' @param na.rm logical; should \code{NA} values be removed? (default TRUE)
#' @param zero.rule for the geometric mean, how should zeros be dealt with? Add 
#' one before, and subtract one after the calculation (see \code{lop1p}), remove 
#' all zeros, or replace
#' all zeros with \eqn{1}.
#' @param p exponential power. For the power mean \code{p=-1}, \code{p=2} and
#' \code{p=3} gives the harmonic, quadratic and cubic means, respectively.
#' For the Lehmer mean \code{p=0}, \code{p=1} and \code{p=2} gives the harmonic,
#' arithmetic and contraharmonic means, respectively.
#' 
#' @section Notice:
#' For some of these means zeros and/or negative values are undefined, or make
#' otherwise little sense in context. Workarounds are given for the geometric mean,
#' but if you end up using it on data \eqn{\le 0}, the wise call would be to
#' reconsider whether using a geometric mean really makes sense in that case.
#' 
#' @examples
#' funl <- substitute(c(harm, geom, mean, quad, cubi))
#' 
#' xl <- list(c( 1, 2, 3, 5),
#'            c(-1, 1, 2, 3, 5),
#'            c( 0, 1, 2, 3, 5),
#'            c(-1, 0, 1, 2, 3, 5))
#' 
#' m <- sapply(xl, function(x) sapply(eval(funl), function(f) f(x)))
#' rownames(m) <- as.character(funl)[-1]
#' colnames(m) <- c("posi", "1neg", "zero", "1ngz")
#' round(m, 3)
#' 
#' harm(xl[[1]]); powr(xl[[1]], -1); lehm(xl[[1]], 0)
#' 
#' y <- c(0, 1, 5, 0, 6, 5, 9)
#' 
#' geom(y, zero.rule="1p")
#' geom(y, zero.rule="rm")
#' geom(y, zero.rule="1")
#' 
#' @name means

NULL

#' @rdname means
#' @export harm

harm <- function(x, na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
	1/mean(1/x)
}

#' @rdname means
#' @export geam

geom <- function(x, zero.rule=c("1p", "rm", "1"), na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
    if (any(x == 0)) {
	    zero <- match.arg(zero.rule)
	    switch(zero, 
	      "1p" = {
		      if (any(l <- x < 0)) {
				  (-1)^sum(l) * expm1(mean(log1p(abs(x))))
			  } else {
			      expm1(mean(log1p(x)))
		      }},
		  "rm" = {
		  	  x <- x[x != 0]
		      if (any(l <- x < 0)) {
				  (-1)^sum(l) * exp(mean(log(abs(x))))
			  } else {
			      exp(mean(log(x)))
		      }},
		   "1" = {
		   	  x[x == 0] <- 1
		      if (any(l <- x < 0)) {
				  (-1)^sum(l) * exp(mean(log(abs(x))))
			  } else {
			      exp(mean(log(x)))
		      }}
	    )
    } else {
	    if (any(l <- x < 0)) {
	        (-1)^sum(l) * exp(mean(log(abs(x))))
		} else {
		    exp(mean(log(x)))
	    }
    }
}

#' @rdname means
#' @export quad

quad <- function(x, na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
    sqrt(mean(x^2))
}

#' @rdname means
#' @export cubi

cubi <- function(x, na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
    mean(x^3)^(1/3)
}

#' @rdname means
#' @export powr

powr <- function(x, p=1.5, na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
    mean(x^p)^(1/p)
}

#' @rdname means
#' @export lehm

lehm <- function(x, p=2, na.rm=TRUE) {
    if (na.rm) {  
        x <- x[!is.na(x)]
    }
    sum(x^p)/sum(x^(p-1))
}