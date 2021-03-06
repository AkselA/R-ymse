% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison_with_ties_ttt.R
\name{tied_triple_test}
\alias{tied_triple_test}
\alias{\%ttt\%}
\alias{ttt}
\alias{is.ttt}
\alias{print.ttt}
\alias{table.ttt}
\title{Tied triple test}
\usage{
x \%ttt\% y

ttt(x, y)

is.ttt(x)

\method{print}{ttt}(x, symbols = TRUE, ...)

\method{table}{ttt}(...)
}
\arguments{
\item{x, y}{numeric values to be compared}

\item{symbols}{should symbols be used instead of numeric values?}

\item{...}{further arguments passed to methods}
}
\description{
Compare numeric values, returning an inbetween value for ties
}
\examples{
1:5 \%ttt\% 3

ttt(1:3, 2)
print(ttt(1:3, 2), FALSE)

c(1, 6, 3, 0) \%ttt\% c(1, 3, 3, 2)

# Equivalent
as.integer(c(1, 6, 3, 0) \%ttt\% c(1, 3, 3, 2))
sign(c(1, 6, 3, 0) - c(1, 3, 3, 2))

# Demonstrating table method 
dtf <- data.frame(x=1:5, y=3)
dtf$`?` <- ttt(dtf$x, dtf$y)
dtf

x <- c(8, 4, 6, 8, 9, 6, 5, 7, 0, 3, 2, 1, 5, 6, 4, 7, 6,
       3, 1, 9, 5, 6, 7, 7, 4, 5, 8, 6, 2, 5, 9, 5, 4, 8)
y <- c(1, 3, 2, 4, 6, 0, 5, 3, 7, 5, 7, 4, 5, 6, 0, 1, 4,
       2, 4, 3, 1, 5, 3, 9, 2, 2, 4, 7, 5, 6, 8)

ou <- outer(sort(x), sort(y), "\%ttt\%")
ta <- table(ou)

pa <- capture.output(ta)

par(mar=c(1, 2, 3, 2))
image(ou, col=topo.colors(length(ta)), axes=FALSE)
title(pa)
box()

}
\seealso{
\code{\link{Comparison}}, \code{\link{comparison_with_ties}}
}
