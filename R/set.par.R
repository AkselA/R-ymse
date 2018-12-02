#' @export

set.par <- function(x=2, y=2, main=1, right=1) {
	if (!exists("old.par")) {
		old.par <<- par(no.readonly=TRUE)
	}
	mar <- c(x, y, main, right)
	mgp <- c(1.9, 0.6, 0)
	par(mar=mar, mgp=mgp)
}

reset.par <- function() {
	par(old.par)
	rm(old.par, pos=1)
}

default.par <- function() {
	if (!exists("old.par")) {
		old.par <<- par(no.readonly=TRUE)
	}
	def.par <- ymse::def.par
	par(def.par)
}
