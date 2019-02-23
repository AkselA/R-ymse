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

set_mar <- function(x=1.8, y=1.8, main=1, right=1) {
	old.par <- get("old.par", envir=ymseEnv)
	assign("old.par", par(no.readonly=TRUE), envir=ymseEnv)
	mar <- c(x, y, main, right)
	mgp <- c(1.9, 0.6, 0)
	par(mar=mar, mgp=mgp)
}

#' Revert par
#' 
#' Reverts \code{par} settings back to \code{old.par}
#' 
#' @family par_and_plot_margins_functions
#' 
#' @export

revert_par <- function() {
	old.par <- get("old.par", envir=ymseEnv)
	assign("old.par", par(no.readonly=TRUE), envir=ymseEnv)
	if (!is.list(old.par)) {
		stop("set_mar or default_par needs to be called first")
	}
	par(old.par)
	invisible(old.par)
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
	assign("old.par", par(no.readonly=TRUE), envir=ymseEnv)
	def.par <- get("def.par", envir=ymseEnv)
	par(def.par)
	invisible(def.par)
}


