#' Create an AR model object
#' 
#' Specify the characteristics of an AR model
#' 
#' @param coefs a vector of model coefficients
#' @param mean the mean of the process
#' @param intercept the intercept in the model
#' @param var.pred the portion of the variance not explained by this model
#' @param frequency the sampling frequency of the process
#' @param x.name name of the series
#' 
#' @seealso \code{\link{arimpulse}}
#' 
#' @export
#' 
#' @examples
#' # short decay
#' ar.mod <- armodel(c(0.5))
#' arimpulse(ar.mod, pulse=1)
#' 
#' # long decay
#' ar.mod <- armodel(c(0.8))
#' arimpulse(ar.mod, pulse=1)
#' 
#' # negative second coefficient reduce damping, signal returns to normal more quickly
#' ar.mod <- armodel(c(0.8, -0.1))
#' arimpulse(ar.mod, pulse=1)
#' 
#' # second coefficient reduce damping too much, overdamping, oscillations
#' ar.mod <- armodel(c(0.8, -0.5))
#' arimp <- arimpulse(ar.mod, pulse=1, n.ahead=40)$pred
#' polyroot(c(1, -ar.mod$ar)) # complex conjugate roots
#' acf(arimp) # period ≈ 6?
#' φ1 <- ar.mod$ar[1]
#' φ2 <- ar.mod$ar[2]
#' f <- (1/(2*pi)) * acos((φ1*(φ2-1))/(4*φ2))
#' 1/f # period = 6.78
#' sp <- spec.ar(ar.mod, plot=FALSE)
#' 1/sp$freq[which.max(sp$spec)] # period = 6.79
#' 
#' # decaying oscillations
#' ar.mod1 <- armodel(c(0.8, -0.6, -0.5, 0.2, -0.2))
#' arimpulse(ar.mod1, n.ahead=100)
#' Mod(1/polyroot(c(1, -ar.mod1$ar))) # barely inside the unit circle
#' 
#' # growing oscillations
#' ar.mod2 <- armodel(c(0.8, -0.7, -0.5, 0.2, -0.2))
#' arimpulse(ar.mod2, n.ahead=100)
#' Mod(1/polyroot(c(1, -ar.mod2$ar))) # barely outside the unit circle
#' 
#' ar.mod3 <- armodel(c(1.8, -1.1, 0.2, -0.2, 0.2))
#' arimpulse(ar.mod3, n.ahead=100)
#' spec.ar(ar.mod3)
#' 
#' resid(arfit(rnorm(10), armodel(c(0.5, -0.1), frequency=2)))

armodel <- function(coefs, mean=0, intercept=0, var.pred=1, 
  frequency=1, x.name="Synthetic AR model") {

	arm2 <- list()
		
	arm2$order <- length(coefs)
	arm2$ar <- coefs
	arm2$var.pred <- var.pred
	arm2$x.mean <- mean
	arm2$aic <- NULL
	arm2$n.used <- NULL
	arm2$n.obs <- NULL
	arm2$order.max <- NULL
	arm2$partialacf <- NULL
	arm2$resid <- NULL
	arm2$x.intercept <- intercept
	arm2$method <- "Manual selection"
	arm2$series <- x.name
	arm2$frequency <- frequency
	arm2$call <- match.call()

	class(arm2) <- "ar"
	arm2
}

#' Impulse response of an AR model
#' 
#' Get and plot the impulse response of an AR model
#' 
#' @param mod an AR model
#' @param pulse numeric vector; the initial pulse. Magnitude is added to the model mean
#' @param n.ahead the length of the computed response
#' @param plot logical; sgould the result be plotted?
#' @param ... further arguments to \code{plot}
#' 
#' @seealso \code{\link{armodel}} for examples
#' 
#' @export

arimpulse <- function(mod, pulse=1, n.ahead=20, plot=TRUE, ...) {
	# https://stats.stackexchange.com/questions/40905
	arm2 <- mod
	nulls <- rep(arm2$x.mean, arm2$order)
	initi <- c(nulls, pulse + arm2$x.mean)
	imp0 <- predict(arm2, newdata=initi, n.ahead=n.ahead, se.fit=TRUE)
	imp <- imp0$pred
	imp <- ts(c(tail(initi, length(pulse) + 1), imp), f=arm2$frequency)
	if (plot) {
	    plot(imp, ...)
	    invisible(imp0)
	} else {
		imp0
	}
}

#' AR model fit
#' 
#' Fit a specified AR model to a univariate time series
#' 
#' @param x a time series
#' @param mod an AR model
#' @param x.mean the mean used. By default the mean of the original model. Set o zero for
#'   no demeaning
#' 
#' @seealso \code{\link{armodel}} for examples
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' x <- runif(50) + sin(1:50/10)
#' plot(x); lines(arfilter(x, armodel(c(1.5, -0.5, 0.5)), x.mean=mean(x)))

arfit <- function(x, mod, x.mean=mod$x.mean) {
	res <- c(rep.int(NA, mod$order), 
	         embed(x - x.mean, mod$order + 1L) %*% c(1, -mod$ar))
	
	freq <- frequency(x)
	
	if (!is.null(mod$frequency)) {
		if (mod$frequency > 1) {
			freq <- mod$frequency
		}
	}
	arm2 <- mod
		
	arm2$x.mean <- x.mean
    arm2$n.used <- length(x)
    arm2$n.obs <- length(x)
    arm2$resid <- ts(res, f=freq)
	arm2$series <- deparse(substitute(x))
	arm2$frequency <- freq
	arm2
}

#' AR filter
#' 
#' Filter a time series using AR coefficients
#' 
#' @param x a time series
#' @param mod an AR model
#' @param x.mean the mean used. By default the mean of the original model. Set to zero for
#'   no demeaning
#' 
#' @seealso \code{\link{armodel}}
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' arap <- ar(AirPassengers)
#' spec.ar(arap)
#' spec.pgram(arfilter(rnorm(10000), arap), span=21, na.action=na.omit)
#' 
#' arm <- armodel(c(1.3, -0.4))
#' spec.ar(arm)
#' plot(x <- rnorm(200), type="l")
#' lines(scale(arfilter(x, arm), center=FALSE), col="red", lwd=2)

arfilter <- function(x, mod, x.mean=mod$x.mean, init="focb") {
	if (is.character(init)) {
		init <- match.arg(init, c("focb", "mean", "reverse"))
		init <- switch(init,
		               focb=rep(x[1], mod$order),
		               mean=rep(mean(x[1:mod$order]), mod$order),
		               reverse=x[mod$order:1])
	} else {
		init <- rep(init, length=mod$order)
	}
	filter(x - x.mean, mod$ar, method="rec", init=init)
}

