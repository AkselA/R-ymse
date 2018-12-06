#' Compare forecast accuracies
#' 
#' Test the efficacy of time series models by comparing forecasts with actual data
#' 
#' @param m a list of models to compare
#' @param y a monovariate time series; the data to train and test the models on
#' @param holdout single integer; the last n points will be forecasted
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' extr <- aggregate(sunspot.month, nfrequency=2, mean)[100:349]
#' extr <- ts(extr, f=21)
#' 
#' mod1 <- StructTS(extr)
#' mod2 <- ar(extr)
#' mod3 <- nnetar(extr)
#' mod4 <- arfima(extr)
#' mod5 <- Arima(extr, order=c(3, 0, 1))
#' mod6 <- Arima(extr, order=c(2, 0, 2), seasonal=c(2, 1, 0))
#' 
#' mod.l <- list(mod1, mod2, mod3, mod4, mod5, mod6)
#' 
#' l <- compare_forecasts(mod.l, extr, 21)
#' 
#' diffs <- sapply(l, function(y) y[["fcast"]] - y[["test"]])
#' matplot(diffs, type="l", 
#'   col=c("red", "lightgreen", "blue", "orange", "pink", "cyan"), lty=1)
#' 
#' par(mfrow=c(3, 2), mar=c(3, 3, 2, 1), mgp=c(2, 0.6, 0), oma=c(0, 0, 0, 0))
#' invisible(lapply(l, function(x) {
#'   plot(x$fcast.obj, shaded=FALSE, PI=FALSE, include=66, type="l", 
#'     cex.main=0.9, xpd=NA)
#'   lines(x$test, col="#00FF4488")
#'   }
#' ))
#' summary(l)
#' head(forecasts(l))
#' l

compare_forecasts <- function(m, y=NULL, holdout=NULL) {
	# not working: stlm, stlf
    
    if (is.null(y)) {
		m1 <- m[[1]]
		
		modcall <- NULL
		modcall <- m1[["call"]]
		
		if (is.null(modcall)) {
			modcall <- m1[["model"]][["call"]]
		}
			
		y <- eval(modcall[[2]])
	}
	
	if (is.null(holdout)) {
	    holdout <- ifelse(frequency(y) > 1, frequency(y), 9)
    }
    
	hl <- holdout
	train <- head(y, -hl)
	test <- tail(y, hl)
	
	acclist <- list()
	for (i in 1:length(m)) {
	
		mod <- m[[i]]
		
		modcall <- NULL
		modcall <- mod[["call"]]
		
		if (is.null(modcall)) {
			modcall <- mod[["model"]][["call"]]
		}
		
		origcall <- modcall
		
		modcall[[2]] <- quote(train)
		modh <- eval(modcall)
		modh$x <- train
		
		forc <- forecast::forecast(modh, h=hl)
		
		err.rmse <- sqrt(mean((test - forc$mean)^2))
		err.mape <- mean(abs((test - forc$mean)/test))
		err.mae <- mean(abs(test - forc$mean))
		err.corP <- (cor(test, forc$mean, method="pearson") - 1) / -2
		err.corS <- (cor(test, forc$mean, method="spearman") - 1) / -2
		err.corK <- (cor(test, forc$mean, method="kendall") - 1) / -2
		
		err <- c(rmse=err.rmse, mape=err.mape, mae=err.mae,
		         "ceP"=err.corP, "ceS"=err.corS, "ceK"=err.corK)
		
		acclist[[i]] <- list(errors=err, train=train, 
		                     test=test, fcast=forc$mean,
		                     fcast.obj=forc, call=origcall)
		
	}
	class(acclist) <- "compare_forecasts"
	acclist
}

#' Return forecasts
#' 
#' Return forecasts and actual data from \code{compare_forecasts} object
#' 
#' @param x a \code{compare_forecasts} object
#' 
#' @return
#' A multivarite time series (\code{mts}) with the actual data, the holdout, 
#' on the first column, and the forecasts on the rest.
#'
#' @export

forecasts <- function(x) {
	fc <- sapply(x, function(y) y[["fcast"]])
	actual <- x[[1]][["test"]]
	cbind(actual, fc)
}

#' @export

print.compare_forecasts <- function(x, nsig=3, quote=FALSE, ...) {
	mat <- t(sapply(x, function(y) y[["errors"]]))
	wm <- apply(mat, 2, which.min) + nrow(mat) * 0:(ncol(mat) - 1)
	cal <- sapply(x, function(y) y$fcast.obj$method)
	rownames(mat) <- cal
	mat0 <- mat
	colnames(mat) <- paste0(" ", colnames(mat))
	mat <- signif(mat, nsig)
	mat[wm] <- paste0("*", mat[wm])
	mat[-wm] <- paste0(" ", mat[-wm])
    print(mat, quote=quote, ...)
	invisible(mat0)
}

#' @export

summary.compare_forecasts <- function(object, ...) {
	ss <- lapply(object, function(y) y[["fcast"]] - y[["test"]])
	ss <- do.call(rbind, lapply(ss, summary, ...))
	rownames(ss) <- sapply(object, function(y) as.character(y["call"]))
	cat("Five number statistic on predicted-true difference", "\n")
    ss
}
