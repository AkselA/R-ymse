r <- seq(-1, 1, 0.01)


signed_log <- function(x, p1=TRUE, scale=p1) {
	if (p1) {
    	s <- log1p(abs(x))*sign(x)
	} else {
	    s <- log(abs(x))*sign(x)
	}
	if (scale) {
		s <- s/0.6931472
	}
	s
}

signed_exp <- function(x, m1=TRUE, scale=TRUE) {
	if (m1) {
    	r <- expm1(abs(x))*sign(x)
    	if (scale) r/1.718282
	} else {
	    r <- exp(abs(x))*sign(x)
	    if (scale) r/2.718282
	}
}

signed_poly <- function(x, p=0.5, pre=0) {
	if (pre) {
        ((abs(x)+pre)^p - pre^p) * sign(x) / ((1+pre)^p - pre^p)
    } else {
        (abs(x))^p * sign(x)
    }
}


# plot(r, type="l", lty=2)
# lines(signed_poly(r, 2, pre=0.01), col="red")
# lines(signed_poly(r, 0.5, pre=0.01), col="orange")
# lines(signed_exp(r), col="blue")
# lines(signed_log(r), col="purple")


