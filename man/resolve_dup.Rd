% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quartz.png.R
\name{resolve_dup}
\alias{resolve_dup}
\title{Resolve duplicate}
\usage{
resolve_dup(x, candidates, ignore.extension = FALSE)
}
\arguments{
\item{x}{character string; name to be resolved}

\item{candidates}{character vector; possible duplicate names}

\item{ignore.extension}{logical; append to the end of \code{x}, even if
it has something that can be interpreted as an extension}
}
\description{
Resolve duplicate names by appending successive integers
}
\examples{
x <- c("my.var", "aaa.png", "aaa.jpg", "aaa_1.png", "doc-folder")
resolve_dup("aaa.jpg", x)
resolve_dup("aaa.png", x)
resolve_dup("aaa_1.png", x)
resolve_dup("doc-folder", x)
resolve_dup("New Document", x)
resolve_dup("my.var", x, ignore.ext=TRUE)
x <- c(x, resolve_dup("aaa.png", x))
resolve_dup("aaa.png", x)
}
