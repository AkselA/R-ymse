% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weekday.R
\name{weekday}
\alias{weekday}
\alias{weekday.default}
\alias{weekday.Date}
\alias{weekday.POSIXt}
\title{Week-day names}
\usage{
weekday(x, ...)

\method{weekday}{default}(x, short = TRUE, language = c("english",
  "nn norwegian", "bm norwegian"), ...)

\method{weekday}{Date}(x, ...)

\method{weekday}{POSIXt}(x, ...)
}
\arguments{
\item{x}{a vector}

\item{...}{further arguments passed to methods}

\item{short}{if \code{TRUE} the names will be returned in shortened form}

\item{language}{what language the names should be returned in}
}
\description{
Convert numeric, character, factor and date-time vectors to week-day names
}
\details{
This function follows the ISO 8601 standard, meaning that Monday is
considered the first day of the week.
}
\examples{
weekday(c("c", "b", "a"))
weekday(c("3", "2", "1"))
weekday(3:1)

weekday(Sys.Date())
weekday(Sys.Date(), short=FALSE, lang="nn nor")
}
