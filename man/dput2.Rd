% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dput2.R
\name{dput2}
\alias{dput2}
\title{Write an Object to console}
\usage{
dput2(x, width = 65, assign = c("front", "end", "none"),
  breakAtParen = FALSE, compact = TRUE, exdent = NULL)
}
\arguments{
\item{x}{an object}

\item{width}{integer; column width}

\item{assign}{character; should assignment be included?}

\item{breakAtParen}{logical; should lines break at parenthesis begins}

\item{compact}{remove spaces around ' = ' assignments}

\item{exdent}{a non-negative integer specifying the exdentation of lines
after the first. default 2 if assign="front", else 0.}
}
\description{
Writes an ASCII text representation of an R object to the console for easy
copy/paste
sharing
}
\details{
This is similar to the way \code{dput} is used to print ASCII
representations
of objects to the console. The differences are that \code{dput2} lets you specify
the width of the resulting column, and assignment of the object to the name used
in the call will by default be included. Line breaks are by default only done on
whitespace,
but can be set to happen at parenthesis begins as well. This should not break code
and can make for a more compact representation, but it can also make the code
harder to read.
}
\examples{
xmpl <- faithful[sort(sample(1:nrow(faithful), 50)), ]
dput(xmpl)
cat(deparse(xmpl, width.cutoff=65), sep='\\n')
dput2(xmpl, compact=FALSE)
dput2(xmpl)
dput2(xmpl, assign="end")
dput2(xmpl, assign="none")
dput2(xmpl, 80)

# no line breaks on whitespaces or parens within character strings
xmpl <- mtcars[1:5, ]
rownames(xmpl) <- c("bbbb (hhhhhhh\\u00A0hhhhhhhh)", 
                    " rrrrrrrr ( bbbbbbb )",
                    "v v v v v v v v v v",
                    "(  g-god, d-god, _-___)",
                    "100*(part)/(total)")
dput2(xmpl, 15)
dput2(xmpl, 15, breakAtParen=TRUE)
}
\seealso{
\code{\link{dput}}, \code{\link{deparse}}, \code{\link{explode}}
}
