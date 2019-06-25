cyl2rect <- function(rho, phi, z, deg=TRUE) {
	if (NCOL(rho) != 3) {
		li <- list(rho, phi, z)
		le <- lengths(li)
		if (sum(abs(diff(le))) != 0) {
			ml <- max(le)
			li <- lapply(li, rep, length.out=ml)
		}
		li <- do.call(cbind, li)
	} else {
		li <- rho
	}
	if (deg) {
		li[,2] <- li[,2]*pi/180
	}
	ou <- li 
	ou[,1] <- li[,1] * cos(li[,2])
	ou[,2] <- li[,1] * sin(li[,2])
	ou[,3] <- li[,3]
	colnames(ou) <- c("x", "y", "z")
	ou
}


rect2cyl <- function(x, y, z, deg=TRUE) {
	if (NCOL(x) != 3) {
		li <- list(x, y, z)
		le <- lengths(li)
		if (sum(abs(diff(le))) != 0) {
			ml <- max(le)
			li <- lapply(li, rep, length.out=ml)
		}
		li <- do.call(cbind, li)
	} else {
		li <- x
	}
	ou <- li 
	ou[,1] <- sqrt(li[,1]^2 + li[,2]^2)
	ou[,2] <- atan2(li[,2], li[,1])
	ou[,3] <- li[,3]
	if (deg) {
		ou[,2] <- ou[,2]*180/pi
	}
	colnames(ou) <- c("rho", "phi", "z")
	ou
}

# cyl2rect(rect2cyl(c(0.1, 0.5, 1), c(0.1, 0.9), c(1, 0.4)))

# s <- seq(0, 360*9, 5)
# r <- seq(1, 0, length.out=length(s))
# plot(cyl2rect(rho=r, phi=s, z=1)[,-3], type="l")
