#' Summarizing seasonal decomposition
#' 
#' Summary method for class "\code{stl}".
#' 
#' @param object an object of class "\code{stl}"
#' @param digits the number of significant digits to use when printing
#' @param ... further arguments passed to or from other methods
#' 
#' @details
#' This function is a slight modification to \code{stats:::summary.stl}, the main
#' change being the addition of the variance statistic, which can be considered
#' a parametric (normal) compliment to the existing IQR statistic.
#' 
#' @export
#' 
#' @examples
#' set.seed(1)
#' x <- ts(rnorm(1e4, sd=1), frequency=12)
#' a <- stl(x, s.window="periodic")
#' stats:::summary.stl(a)
#' summary(a)
#' 
#' b <- stl(co2, s.window="periodic")
#' summary(b)

summary.stl <- function (object, digits = getOption("digits"), ...) {
    cat(" Call:\n ")
    dput(object$call, control = NULL)
    cat("\n Time.series components:\n")
    print(summary(object$time.series, digits = digits, ...))
    cat("\n IQR:\n")
    iqr <- apply(cbind(STL = object$time.series, data = object$time.series %*% 
        rep(1, 3)), 2L, IQR)
    print(rbind(format(iqr, digits = max(2L, digits - 3L)),
        `   %` = format(round(100 * iqr/iqr["data"], 2))), quote = FALSE)
    cat("\n Variance:\n")
    exv <- apply(cbind(STL = object$time.series, data = object$time.series %*% 
        rep(1, 3)), 2L, var)
    print(rbind(format(exv, digits = max(2L, digits - 3L)),
        `   %` = format(round(100 * exv/exv["data"], 2))), quote = FALSE)
    cat("\n Weights:")
    if (all(object$weights == 1)) 
        cat(" all == 1\n")
    else {
        cat("\n")
        print(summary(object$weights, digits = digits, ...))
    }
    cat("\n Other components: ")
    str(object[-(1L:3)], give.attr = FALSE)
    invisible(object)
}