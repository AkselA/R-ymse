

qlambda <- function(p=0.2, lambda) {
	l <- lambda
	r <- (p^l - (1-p)^l) / l
	if (any(z <- l == 0)) {
	    r[z] <- log(p[z]/(1-p[z]))
	}
    r
}

ppcc <- function(x, p=ppoints(x), qfun=qlambda, shape=seq(-2, 3, 0.01),
  ffun=cor, plot=TRUE, add=FALSE, type="l", main, ...) {
	qfun <- match.fun(qfun)
	ffun <- match.fun(ffun)
	
	fit <- sapply(shape, function(s) ffun(x, qfun(p, s)))
	if (plot) {
		wm <- which.max(fit)
		if (missing(main)) {
			main <- paste0("Optimum: fit=", round(fit[wm], 3),
			               ", shape=", round(shape[wm], 3))
		}
		if (add) {
			lines(shape, fit, type=type, main=main, ...)
		} else {
			plot(shape, fit, type=type, main=main, ...)
		}
		points(shape[wm], fit[wm])
	    invisible(cbind(shape, fit))
	} else {
		cbind(shape, fit)
	}
}

# set.seed(1)
# ppcc(sort(runif(1000)))
# ppcc(sort(rnorm(1000)))
# ppcc(sort(rlogis(1000)))
# ppcc(sort(rcauchy(1000)))

# ppcc(sort(rgamma(1000, 5, 0.5)), qfun=qweibull, shape=seq(0.01, 5, 0.01))

# irmse <- function(x, y) 1/sqrt(mean((x - scale(y))^2))
# ppcc(x=scale(c(1, 2, 4, 5, 6, 6)), efun=irmse)




# # Replace muliple patterns with the same pattern wrapped in brackets

# ch <- "Just to add, the aim is to compare a predicted set of sequence from the observed ones. So we'd appreciate comments on as to how best to compare these."

# p <- c("of", "to", "is", "as", "on")
# pb <- paste0("\\b", p, "\\b")

# ch1 <- ch
# for (i in seq_along(p)) {
    # ch1 <- gsub(pb[i], paste0("{", p[i], "}"), ch1)
# }
# ch1

# ch2 <- ch
# pbc <- paste(pb, collapse="|")
# m <- gregexpr(pbc, ch2)
# regmatches(ch2, m) <- list(paste0("{", regmatches(ch2, m)[[1]], "}"))
# ch2