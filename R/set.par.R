set.par <- function(xlab=TRUE, ylab=TRUE, main=TRUE) {
	if (!exists("old.par")) {
		old.par <<- par(no.readonly=TRUE)
	}
	mar <- c(2, 2, 1, 1)
	if (xlab) mar[1] <- 3
	if (ylab) mar[2] <- 3
	if (main) mar[3] <- 2
	par(mar=mar, mgp=c(2, 0.6, 0))
}

reset.par <- function() {
	par(old.par)
	rm(old.par, pos=1)
}

default.par <- function() {
	if (!exists("old.par")) {
		old.par <<- par(no.readonly=TRUE)
	}
	data(def.par)
	par(def.par)
}
