#' Add rows to a data.frame
#' 
#' An "\code{rbind} for data.frames", sort of.
#' 
#' @param dtf data.frame; the object that shall be added to
#' @param nrw data.frame; the new row(s) to be added
#' @param top logical; should the new rows be added to the top or the bottom (default)?
#' 
#' @details Can only bind two objects at a time, but will bind data.frames with
#' non-matching column names and -classes. In such a case the first data.frame will serve as
#' template.
#' 
#' @export
#' 
#' @examples
#' dtf <- data.frame(A=letters[1:5], 
#'                   B=1:5, 
#'                   C=as.factor(5:1), 
#'                   D=as.Date(0:4, origin="2000-01-01"),
#'                   stringsAsFactors=FALSE)
#' 
#' nrw <- data.frame(A=letters[1:5], 
#'                   B=4:8, 
#'                   C=5:1, 
#'                   D=as.Date(5:1, origin="1990-01-01"),
#'                   stringsAsFactors=FALSE)
#' str(dtf)
#' 
#' dtf.a <- addrows(dtf, nrw, top=FALSE)
#' str(dtf.a)
#' 
#' # adding a single row with little concern for data types and column names
#' b <- type.convert(beaver1[80:90,])
#' b$activ <- as.logical(b$activ)
#' 
#' addrows(b, data.frame(350, 1200, 37.02, 1))

addrows <- function(dtf, nrw, top=FALSE) {
	if (!is.data.frame(dtf) | !is.data.frame(nrw)) {
		stop("dtf and nrw must be data.frame")
	}
	if ((nc <- ncol(dtf)) != ncol(nrw)) {
		stop("dtf and nrw are of unequal length")
	}
	if (any((cl <- sapply(dtf, class)) != sapply(nrw, class))) {
		warning("the classes of dtf and nrw don't match up", call.=FALSE)
	}
	if (top) {
		ll <- mapply(c, nrw, dtf, SIMPLIFY=FALSE)
	} else {
		ll <- mapply(c, dtf, nrw, SIMPLIFY=FALSE)
	}
    ll <- lapply(1:nc, 
      function(x) {
          do.call(paste0("as.", cl)[x], list(ll[[x]]))
      })
    dtf2 <- as.data.frame(ll, stringsAsFactors=FALSE)
    colnames(dtf2) <- colnames(dtf)
    dtf2
}

