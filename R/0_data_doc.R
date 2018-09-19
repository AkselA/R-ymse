#' Maurice Stevenson Bartlett's car data
#' 
#' @description This is an example data set Bartlett used for a lecture course on stochastic
#' processes, Statistics Department, University College, London. The data represents the
#' times, in seconds, when cars passed an observation point by a road. \cr
#' 
#' Bartlett attributes the data to a Dr A. J. Miller who supplied them as a class example.
#' According to Adery C. A. Hope the data was recorded on a rural Swedish road.
#' 
#' @section M. S. Bartlett's notes:
#' 
#' Analyse the above data with a view to examining:
#' \describe{
#'     \item{i}{whether the times of passing constitute a Poisson process;}
#'     \item{ii}{if not, whether some form of "bunching" or "clustering" seems to be
#'     present.}
#' }
#' Possible analyses include:
#' \describe{
#'     \item{a}{testing the homogeneity of the consecutive random time-intervals, by means
#'     of a partitioning of the degrees of freedom for the total (approximate) \eqn{\chi^2};}
#'     \item{b}{testing the homogeneity of counts in consecutive fixed time-intervals, choosing
#'     an appropriate interval, and partitioning the degrees of freedom corresponding
#'     to the total dispersion by means of an analysis of variance;}
#'     \item{c}{testing the correlation between the consecutive random time-intervals;}
#'     \item{d}{examining the overall distribution of counts in fixed time-intervals;}
#'     \item{e}{examining the overall distribution of the consecutive random time-intervals}
#' }
#' You should undertake at least sufficient of these to answer the questions asked.
#' 
#' @format A numeric vector representing time points in seconds
#' 
#' @source The Spectral Analysis of Point Processes (p. 280), M. S. Bartlett, 1963
#' 
#' Also mentioned in: \cr
#' Statistical Estimation of Density Functions (p. 252), M. S. Bartlett, 1963 \cr
#' A Simplified Monte Carlo Significance Test Procedure (p. 583), Adery C. A. Hope, 1968
#' 
#' @examples
#' bartlett2 <- bartlett - bartlett[1]
#' 
#' x <- rep(0, tail(bartlett2, 1)*10)
#' x[bartlett2*10] <- 1
#' 
#' par(mfrow=c(2, 1), mar=c(2, 3, 1, 1))
#' plot(x, type="l", ann=FALSE)
#' lines(cumsum(x)/sum(x), col="red", lwd=2)
#' 
#' sp <- spectrum(x, main="", xlim=c(0, 0.1), ylim=c(1e-3, 0.04))
#' spec <- predict(loess(sp$spec[1:3000] ~ sp$freq[1:3000], span=0.15), se=TRUE)
#' lines(sp$freq[1:3000], spec$fit, col="red", lwd=2)
#' lines(sp$freq[1:3000], spec$fit - qt((0.99 + 1)/2, spec$df)*spec$se, lty=1, col="lightblue")
#' lines(sp$freq[1:3000], spec$fit + qt((0.99 + 1)/2, spec$df)*spec$se, lty=1, col="lightblue")

"bartlett"


#' 2018 MarbleLympics speed skating times
#' 
#' @description Intermediate and total times for all 16 runs, arranged by lane and heat
#' number.
#' 
#' @format A list containing two data.frames, one for each lane. Columns are heat and rows
#' are time checks in seconds.
#' 
#' @source https://www.youtube.com/watch?v=fA-O6f_jArk
#' 
#' @examples
#' tt <- t(do.call(cbind, speedskate))
#' pairs(tt)
#' cor(tt)
#' outer(
#'   colnames(tt), 
#'   colnames(tt), 
#'   Vectorize(function(i,j) cor.test(tt[,i],tt[,j])$p.value)
#' )

"speedskate"