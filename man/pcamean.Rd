% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_pca.R
\name{pcamean}
\alias{pcamean}
\title{PCA mean}
\usage{
pcamean(...)
}
\arguments{
\item{...}{prcomp, princomp or factanal objects, or a single list of such objects}
}
\description{
Takes the average of several PCA objects
}
\details{
I don't know if this kind of calculation has any sort of merit. It was
written more as an impromptu challenge than as a solution to any problem
}
\examples{
xx <- data.frame(bee=c(0, 0, 1, 2, 3, 2, 0, 3), 
                wasp=c(1, 3, 2, 0, 1, 1, 2, 1), 
                  fly=c(1, 2, 4, 2, 1, 0, 1, 0),
              beetle=c(1, 0, 0, 1, 2, 2, 0, 2))

set.seed(1)
r <- 1000
xxs <- replicate(r, {
  xx$random <- sample(c(0:1, 0:4), 8, r=TRUE)
  xx
  }, simplify=FALSE)

xxm <- Reduce("+", xxs) / r
xxl <- lapply(xxs, princomp)

biplot(pcamean(xxl))
biplot(princomp(xxm))
}
\seealso{
\code{\link{prcomp}}, \code{\link{princomp}}, \code{\link{factanal}}
}
