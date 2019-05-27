# set.seed(1)
# f <- data.frame(x=sort(sample(1:30, 15)), y=jitter(sin((1:15)/0.9), 2))

damped_spline <- function(x, y, ..., tie.ends=TRUE, w=0.5) {
	if (!missing(y)) x <- cbind(x, y)
	g <- (rbind(c(NA, NA), x) + rbind(x, c(NA, NA)))/2
	if (tie.ends) {
		g[c(1, nrow(g)),] <- x[c(1, nrow(x)),]
	}
	sp1 <- spline(x[,1], x[,2], ...)
	sp2 <- spline(g[,1], g[,2], ...)
	cbind(x=sp1$x, y=sp1$y*(1-w)+sp2$y*w)
}


# s <- seq(min(f[,1]), max(f[,1]), l=500)
# sp <- damped_spline(x=f, xout=s, w=0)
# spd <- damped_spline(x=f, xout=s, w=0.5)
# spdo <- damped_spline(x=f, xout=s, w=1)

# plot(f, ylim=range(sp[,2]), pch=16, cex=1.2, xlab="Years BP")
# lines(sp, lwd=2, col="red")
# lines(spd, lwd=2, col="purple")
# lines(spdo, lwd=2, col="blue")
# legend("topleft", c("regular spline", "damped spline", "overdamped spline"),
  # bty="n", text.col=c("red", "purple", "blue"), cex=0.9, inset=c(0.05, 0))

linear_upsample <- function(x, y) {
	if (!missing(y)) x <- cbind(x, y)
	mn <- rbind(c(NA, NA), x) + rbind(x, c(NA, NA))
	mn <- mn/2
    x2 <- rbind(x, mn)
    x2[order(x2[,1]), ]
}

# plot(linear_upsample(f))
