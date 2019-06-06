clamp <- function(x, lower, upper) {
	pmax(lower, pmin(upper, x))
}

#' Central tendency measures
#' 
#' @param x numeric vector
#' @param single return a single value (for \code{cmode} and \code{dmode})
#' @param na.rm remove \code{NA}s before starting calculations
#' @param ... send further arguments to underlying function, e.g. \code{density}
#' for \code{cmode}
#' 
#' # @seealso \code{\link{means}}
#' 
#' @examples
#' xx <- c(1, 3, 4, 5, 7, 8, 9, 9, 7, 5, 4, 5, 3, 8)
#' median(xx)
#' pseudomedian(xx)
#' 
#' # Discrete mode
#' dmode(c(2, 3, 3, 4, 5))
#' dmode(c(2, 3, 3, 2, 5))
#' dmode(c(2, 3, 3, 2, 5), single=FALSE)
#' dmode(c(2, 1, 3, NA, 1))
#' dmode(c(2, 1, 3, NA, NA))
#' 
#' # Continuous mode
#' cmode(c(2, 3, 3, 4, 5))
#' cmode(c(2, 3, 3, 4, 5))
#' cmode(c(2, 3, 3, 4, 4, 5), n=512)
#' cmode(c(2, 2, 3, 3, 6, 6, 6, 7), single=FALSE, adjust=0.5)
#' 
#' # Slightly robust mean
#' set.seed(1)
#' r <- round(rexp(12)*c(-100, 100))
#' mean(r)
#' srmean(r)
#' weighted.mean(sort(r), c(0.5, rep(1, length(r)-2), 0.5))
#' 
#' 
#' @name central.tendency

NULL

#' @rdname central.tendency
#' @export pseudomedian

# aka. Hodges–Lehmann estimator
pseudomedian <- function(x, na.rm=TRUE) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
    median(c(x, colMeans(combn(x, 2))))
}

#' @rdname central.tendency
#' @export cmode

# continuous mode
cmode <- function(x, single=TRUE, ...) {
	dl <- list(...)
	defarg <- alist(x=x, adjust=1.2, n=1024, from=min(x), to=max(x))
	pm <- pmatch(names(dl), names(defarg))
	nna <- !is.na(pm)
	defarg[pm[nna]] <- dl[nna]
	dl <- c(defarg, dl[!nna])
    den <- do.call(density, dl)
    if (single) {
    	den$x[which.max(den$y)]
    } else {
    	cm <- do.call(cbind, den[1:2])
        sm <- diff(cm[,"y"])
        cmx <- cm[which(diff(sign(sm)) < 0) + 1, ]
        cmx
    }
}


#' @rdname central.tendency
#' @export dmode

# discrete mode
dmode <- function(x, single=TRUE, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    ux <- unique(x)
    freq <- tabulate(match(x, ux))
    if (single) {
        ux[which.max(freq)]
    } else {
        ux[freq == max(freq)]
    }
}


#' @rdname central.tendency
#' @export midrange

midrange <- function(x, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
	(min(x)+max(x))/2
}

#' @rdname central.tendency
#' @export srmean

# slightly robust mean
srmean <- function(x, na.rm=FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
	mr <- (min(x)+max(x))/2
	(sum(x)-mr)/(length(x)-1)
}

# midhinge

# trimean

# winsorized