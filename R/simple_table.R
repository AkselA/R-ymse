#' Read a simple table
#' 
#' Read tables given in more or less elaborate human-readable formats
#' 
#' @param x a teble represented as a character string 
#' @param header are the table columns named? By default \code{TRUE}
#' @param rem.dup.header remove duplicated headers.
#' @param na.strings a character vector of strings which are to be interpreted 
#'  as \code{NA} values
#' @param stringsAsFactors should character vectors be converted to factors? By
#'  default \code{FALSE}
#' @param ... further arguments passed to \code{read.table}
#' 
#' @return A \code{data.frame} containing a representation of the data.
#' 
#' @export
#' 
#' @examples
#' x1 <- "
#' +------------+------+------+----------+--------------------------+
#' |    Date    | Emp1 | Case | Priority | PriorityCountinLast7days |
#' +------------+------+------+----------+--------------------------+
#' | 2018-06-01 | A    | A1   |        0 |                        0 |
#' | 2018-06-03 | A    | A2   |        0 |                        1 |
#' | 2018-06-02 | B    | B2   |        0 |                        2 |
#' | 2018-06-03 | B    | B3   |        0 |                        3 |
#' +------------+------+------+----------+--------------------------+
#' "
#' 
#' x2 <- "
#' ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#'     Date    | Emp1 | Case | Priority | PriorityCountinLast7days 
#' ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#'  2018-06-01 | A    | A|1  |        0 |                        0 
#'  2018-06-03 | A    | A|2  |        0 |                        1 
#'  2018-06-02 | B    | B|2  |        0 |                        2 
#'  2018-06-03 | B    | B|3  |        0 |                        3 
#' ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#' "
#' 
#' x3 <- "
#'  Maths | English | Science | History | Class
#' 
#'   0.1  |  0.2    |  0.3    |  0.2    |  Y2
#' 
#'   0.9  |  0.5    |  0.7    |  0.4    |  Y1
#' 
#'   0.2  |  0.4    |  0.6    |  0.2    |  Y2
#' 
#'   0.9  |  0.5    |  0.2    |  0.7    |  Y1
#' "
#' 
#' x4 <- "
#'          Season |   Team  | W | AHWO
#' -------------------------------------
#' 1  |  2017/2018 |  TeamA  | 2 | 1.75
#' 2  |  2017/2018 |  TeamB  | 1 | 1.85
#' 3  |  2017/2018 |  TeamC  | 1 | 1.70
#' 4  |  2017/2018 |  TeamD  | 0 | 3.10
#' 5  |  2016/2017 |  TeamA  | 1 | 1.49
#' 6  |  2016/2017 |  TeamB  | 3 | 1.51
#' 7  |  2016/2017 |  TeamC  | 2 | 1.90
#' 8  |  2016/2017 |  TeamD  | 0 | N/A 
#' "
#' 
#' x5 <- "
#'     A   T   G   C
#'    ---------------
#' A | 6 | 0 | 4 | 0 |
#'   |---:---:---:---
#' T | 0 | 6 | 0 | 4 |
#'   |---:---:---:---
#' G | 4 | 0 | 6 | 0 |
#'   |---:---:---:---
#' C | 0 | 4 | 0 | 6 |
#'    ---------------
#' "
#' 
#' x6 <- "
#' ------------------------------------------------------------
#' |date              |Material          |Description         |
#' |----------------------------------------------------------|
#' |10/04/2013        |WM.5597394        |PNEUMATIC           |
#' |11/07/2013        |GB.D040790        |RING                |
#' ------------------------------------------------------------
#' 
#' ------------------------------------------------------------
#' |date              |Material          |Description         |
#' |----------------------------------------------------------|
#' |08/06/2013        |WM.4M01004A05     |TOUCHEUR            |
#' |08/06/2013        |WM.4M010108-1     |LEVER               |
#' ------------------------------------------------------------
#' "
#' 
#' lapply(c(x1, x2, x3, x4, x5, x6), simple_table)

simple_table <- function(x, header=TRUE, rem.dup.header=header, 
  na.strings=c("NA", "N/A"), stringsAsFactors=FALSE, ...) {

    # read each row as a character string
    x <- scan(text=x, what="character", sep="\n", quiet=TRUE)

    # keep only lines containing alphanumerics
    x <- x[grep("[[:alnum:]]", x)]
    
    # remove vertical bars with trailing or leading space
    x <- gsub("\\|? | \\|?", " ", x)

    # remove vertical bars at beginning and end of string
    x <- gsub("\\|?$|^\\|?", "", x)

    # remove vertical box-drawing characters
    x <- gsub("\U2502|\U2503|\U2505|\U2507|\U250A|\U250B", " ", x)
    
    if (rem.dup.header) {
    	dup.header <- x == x[1]
    	dup.header[1] <- FALSE
    	x <- x[!dup.header]
    }

    # read the result as a table
    read.table(text=paste(x, collapse="\n"), header=header, 
      na.strings=na.strings, stringsAsFactors=stringsAsFactors)    
}

# # Also works with box-drawing characters
# xx <- "
    # A   B   C
  # ┌───┬───┬───┐
# A │ 5 │ 1 │ 4 │
  # ├───┼───┼───┤
# B │ 2 │ 5 │ 3 │
  # ├───┼───┼───┤
# C │ 3 | 4 | 4 │
  # └───┴───┴───┘
# "

# simple_table(xx)


# ─ │ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼
# hb <- "\U2500"
# vb <- "\U2502"

# tl <- "\U250C"
# tr <- "\U2510"
# bl <- "\U2514"
# br <- "\U2518"

# lt <- "\U251C"
# rt <- "\U2524"
# tt <- "\U252C"
# bt <- "\U2534"
