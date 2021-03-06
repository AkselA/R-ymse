% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mar_and_par.R
\name{set_mar}
\alias{set_mar}
\title{Set plot margins}
\usage{
set_mar(x = 1.8, y = 1.8, main = 1, right = 1, cex.main = 1, ...)
}
\arguments{
\item{x}{margin width for the x axis, default \code{2}}

\item{y}{margin width for the x axis, default \code{2}}

\item{main}{margin width for the main title, default \code{1}}

\item{right}{margin width for the right edge, default \code{1}}

\item{cex.main}{The magnification to be used for main titles relative to the 
current setting of \code{cex}, default \code{1}}

\item{...}{further arguments passed to \code{par}}
}
\description{
Moves axis titles and labels closer to the plotting window and shrinks the margins
}
\details{
Old \code{par} settings are stored in \code{.old.par} before a call to
\code{par} of the form \code{par(mar=c(x, y, main, right), mgp=c(1.9, 0.6, 0))}
is made.
}
\examples{
ymse:::.old.par
get("old.par", envir=ymse:::ymseEnv)
ls(envir=ymse:::ymseEnv)

par(col.axis=2)
plot(1:4)

set_mar()
plot(1:4)

default_par()
plot(1:4)

revert_par()
plot(1:4)

ymse:::.old.par
head(get("old.par", envir=ymse:::ymseEnv))
}
\seealso{
\code{\link{par}}

Other par_and_plot_margins_functions: \code{\link{default_par}},
  \code{\link{revert_par}}
}
\concept{par_and_plot_margins_functions}
