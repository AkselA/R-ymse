is_dup <- function(x, incomparables=FALSE) {
	if (is.vector(x)) {
		x %in% x[duplicated(x, incomparables=incomparables)]
	} else {
	    o <- array(TRUE, dim(x))
        o[] <- x %in% x[duplicated(as.vector(x), incomparables=incomparables)]
        o
	}
}

list_select <- function(x, include=NULL, exclude=NULL) {
	cnl <- lapply(x, colnames)
	if (!is.null(include)) {
		if (is.numeric(include) | is.logical(include)) {
			for (d in seq_along(x)) {
				cnl[[d]] <- narm(cnl[[d]][include])
				x[[d]] <- x[[d]][, cnl[[d]], drop=FALSE]
			}
		} 
		if (is.character(include)) {
			for (d in seq_along(x)) {
				cnl[[d]] <- intersect(cnl[[d]], include)
				x[[d]] <- x[[d]][, cnl[[d]], drop=FALSE]
			}
		}
	}
	if (!is.null(exclude)) {
		if (is.numeric(exclude)) {
			for (d in seq_along(x)) {
				cnl[[d]] <- narm(cnl[[d]][-exclude])
				x[[d]] <- x[[d]][, cnl[[d]], drop=FALSE]
			}
		} 
		if (is.logical(exclude)) {
			for (d in seq_along(x)) {
				cnl[[d]] <- narm(cnl[[d]][!exclude])
				x[[d]] <- x[[d]][, cnl[[d]], drop=FALSE]
			}
		} 
		if (is.character(exclude)) {
			for (d in seq_along(x)) {
				cnl[[d]] <- setdiff(cnl[[d]], exclude)
				x[[d]] <- x[[d]][, cnl[[d]], drop=FALSE]
			}
		}
	}
	x
}


#' Merge multiple \code{data.frame}s
#' 
#' @param x a list of \code{data.frame}s with at least one column in common
#' @param by name of the column to be merged by, by default the full intersect of 
#'        column names between \code{data.frame}s
#' @param all include all rows, including those with no match
#' @param sort sort the output on the \code{by} column(s)
#' @param incomparables values which cannot be matched.
#' @param include,exclude numric, logical or character vector specifying which 
#'        columns to include in or exclude from the merge
#' 
#' @details
#' If there are duplicate columns that aren't being used to merge by, one of two
#' things will happen. If the parent \code{data.frame}s of the duplicate columns
#' are named, then that name will be appended to the relevant column names. If
#' the \code{data.frame}s aren't named, then the \code{data.frame}s idices in
#' the parent list are appended to the relevant column names.\cr
#' Inclusion and exclusion are performed in sequence, so that if both 
#' \code{include} and \code{exclude} are specified, \code{exclude} acts on the
#' result from \code{include}.
#' 
#' @export
#' 
#' @examples
#' dtf1 <- data.frame(ast=1:4, bar=1:4, kat=c("A", "B", "C", "D"))
#' dtf2 <- data.frame(ast=1:6, bar=1:6, jun=9:4)
#' dtf3 <- data.frame(ast=2:6, bar=2:6, kat=c("A", "B", "C", "D", "E"))
#' dtf4 <- data.frame(ast=3:4, bar=3:4)
#' dtf5 <- data.frame(ast=1:-3, bar=0:4, git=0:4)
#' 
#' ll <- list(d1=dtf1, d2=dtf2, dtf3, A=dtf4, dtf5)
#' 
#' merge_multiple(ll, by="bar")
#' merge_multiple(ll, by="bar", all=TRUE, include=1:2)
#' merge_multiple(ll, by="bar", all=TRUE, exclude="kat")
#' merge_multiple(x=ll, by=c("bar", "ast"), all=TRUE)

merge_multiple <- function(x, by, all=FALSE, sort=TRUE, 
  incomparables=NULL, include=NULL, exclude=NULL) {
	if (!(is.null(include) & is.null(exclude))) {
		x <- list_select(x, include, exclude)
	}
	if (missing(by)) {
       by <- intsect(cnl)
    }
	cnl <- lapply(x, colnames)
	cn <- unlist(cnl)
	cnd <- is_dup(cn) & !(cn %in% by)
    if (any(cnd)) {
    	ext <- names(x)
    	s <- rep(seq_along(x), lengths(x))
    	if (is.null(ext)) {
    		ix <- s
    	} else {
    		ext[ext == ""] <- seq_along(x)[ext == ""]
    		ix <- rep(ext, lengths(x))
    	}
		cn[cnd] <- paste(cn[cnd], ix[cnd], sep=".")
		cnl <- split(cn, s)
		x <- mapply(`colnames<-`, x, cnl, SIMPLIFY=FALSE)
	}
	m <- Reduce(
	  function(a, b) 
	      merge(a, b, by, all=all, sort=FALSE, incomparables=incomparables),
	x)
	if (sort) {
		m <- m[do.call(order, m[, by, drop=FALSE]),]
	    rownames(m) <- NULL
	}
	m
}
