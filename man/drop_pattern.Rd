% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop_effects.R
\name{drop_pattern}
\alias{drop_pattern}
\title{Drop predictors}
\usage{
drop_pattern(form, pattern, ...)
}
\arguments{
\item{form}{a formula object}

\item{pattern}{predictors matching this pattern will be dropped}

\item{...}{further arguments passed on to \code{\link{grepl}}}
}
\value{
A formula object
}
\description{
Drop predictor variables according to a (regex) pattern
}
\details{
\code{form} is divided into its individual terms, any term matching 
\code{pattern} is removed, before \code{form} is updated and returned. 
In case no match is made, \code{form} is returned unmodified. 
In case all predictors match, only the intercept is retained.
In any case the response variable(s) are kept as is.
}
\examples{
f6 <- y ~ aa*bb + aa + ac + cc + acab

drop_pattern(f6, "a") # Drop all containing a
drop_pattern(f6, "a{2}") # Drop all containing exactly 2 consecutive as
drop_pattern(f6, "^[^a]*a[^a]*$")  # All containing exactly 1 a
drop_pattern(f6, ":") # Drop interaction
drop_pattern(f6, "^[^:]*a[^:]*$") # Drop all containg a, but not interaction
drop_pattern(f6, "^((?!a).)*$", perl=TRUE) # Drop all not containing a

# Degenerate cases
drop_pattern(f6, "[abc]") # Drop all
drop_pattern(f6, "q") # Drop none

}
\seealso{
\code{\link{drop_randfx}}
}
