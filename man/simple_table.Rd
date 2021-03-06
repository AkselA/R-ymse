% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_table.R
\name{simple_table}
\alias{simple_table}
\title{Read a simple table}
\usage{
simple_table(x, header = TRUE, rem.dup.header = header,
  na.strings = c("NA", "N/A"), stringsAsFactors = FALSE, ...)
}
\arguments{
\item{x}{a teble represented as a character string}

\item{header}{are the table columns named? By default \code{TRUE}}

\item{rem.dup.header}{remove duplicated headers.}

\item{na.strings}{a character vector of strings which are to be interpreted 
as \code{NA} values}

\item{stringsAsFactors}{should character vectors be converted to factors? By
default \code{FALSE}}

\item{...}{further arguments passed to \code{read.table}}
}
\value{
A \code{data.frame} containing a representation of the data.
}
\description{
Read tables given in more or less elaborate human-readable formats
}
\examples{
x1 <- "
+------------+------+------+----------+--------------------------+
|    Date    | Emp1 | Case | Priority | PriorityCountinLast7days |
+------------+------+------+----------+--------------------------+
| 2018-06-01 | A    | A1   |        0 |                        0 |
| 2018-06-03 | A    | A2   |        0 |                        1 |
| 2018-06-02 | B    | B2   |        0 |                        2 |
| 2018-06-03 | B    | B3   |        0 |                        3 |
+------------+------+------+----------+--------------------------+
"

x2 <- "
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
    Date    | Emp1 | Case | Priority | PriorityCountinLast7days 
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
 2018-06-01 | A    | A|1  |        0 |                        0 
 2018-06-03 | A    | A|2  |        0 |                        1 
 2018-06-02 | B    | B|2  |        0 |                        2 
 2018-06-03 | B    | B|3  |        0 |                        3 
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
"

x3 <- "
 Maths | English | Science | History | Class

  0.1  |  0.2    |  0.3    |  0.2    |  Y2

  0.9  |  0.5    |  0.7    |  0.4    |  Y1

  0.2  |  0.4    |  0.6    |  0.2    |  Y2

  0.9  |  0.5    |  0.2    |  0.7    |  Y1
"

x4 <- "
         Season |   Team  | W | AHWO
-------------------------------------
1  |  2017/2018 |  TeamA  | 2 | 1.75
2  |  2017/2018 |  TeamB  | 1 | 1.85
3  |  2017/2018 |  TeamC  | 1 | 1.70
4  |  2017/2018 |  TeamD  | 0 | 3.10
5  |  2016/2017 |  TeamA  | 1 | 1.49
6  |  2016/2017 |  TeamB  | 3 | 1.51
7  |  2016/2017 |  TeamC  | 2 | 1.90
8  |  2016/2017 |  TeamD  | 0 | N/A 
"

x5 <- "
    A   T   G   C
   ---------------
A | 6 | 0 | 4 | 0 |
  |---:---:---:---
T | 0 | 6 | 0 | 4 |
  |---:---:---:---
G | 4 | 0 | 6 | 0 |
  |---:---:---:---
C | 0 | 4 | 0 | 6 |
   ---------------
"

x6 <- "
------------------------------------------------------------
|date              |Material          |Description         |
|----------------------------------------------------------|
|10/04/2013        |WM.5597394        |PNEUMATIC           |
|11/07/2013        |GB.D040790        |RING                |
------------------------------------------------------------

------------------------------------------------------------
|date              |Material          |Description         |
|----------------------------------------------------------|
|08/06/2013        |WM.4M01004A05     |TOUCHEUR            |
|08/06/2013        |WM.4M010108-1     |LEVER               |
------------------------------------------------------------
"

lapply(c(x1, x2, x3, x4, x5, x6), simple_table)
}
