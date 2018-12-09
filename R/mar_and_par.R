#' Set plot margins
#' 
#' Moves axis titles and labels closer to the plotting window and shrinks the margins
#' 
#' @param x margin width for the x axis, default \code{2}
#' @param y margin width for the x axis, default \code{2}
#' @param main margin width for the main title, default \code{1}, no title
#' @param right margin width for the right edge, default \code{1}
#' 
#' @details
#' Old \code{par} settings are stored in \code{.old.par} before a call to
#' \code{par} of the form \code{par(mar=c(x, y, main, right), mgp=c(1.9, 0.6, 0))}
#' is made.
#' 
#' @family par_and_plot_margins_functions
#' 
#' @export

set_mar <- function(x=2, y=2, main=1, right=1) {
	if (!is.list("old.par")) {
		assign("old.par", par(no.readonly=TRUE), envir=.GlobalEnv)
	}
	mar <- c(x, y, main, right)
	mgp <- c(1.9, 0.6, 0)
	par(mar=mar, mgp=mgp)
}

#' Reset par
#' 
#' Reverts \code{par} settings back to \code{old.par}
#' 
#' @family par_and_plot_margins_functions
#' 
#' @export

reset_par <- function() {
	if (!exists("old.par")) {
		stop("set_mar or default_par has not been called yet")
	}
	par(old.par)
	rm(old.par, pos=1)
}

#' Default par
#' 
#' Sets \code{par} settings to their default values
#' 
#' @details
#' Default par settings can be retreived by \code{data(.def.par)}. A new default
#' can be specified by editing \code{def.par} or making a 
#' \code{def.par <- par(no.readonly=TRUE)} type call.
#' 
#' @family par_and_plot_margins_functions
#' 
#' @export

default_par <- function() {
	if (!is.list("old.par")) {
		assign("old.par", par(no.readonly=TRUE), envir=.GlobalEnv)
	}
	par(def.par)
}


