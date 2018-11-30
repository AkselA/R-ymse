#' Data cleanup
#' 
#' Create a data.frame from a messy table
#' 
#' @param x A messy table the form of a character string
#' @param header Does the table include headers? (default TRUE)
#' @param ... further arguments passed to \code{read.table}
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
#' x2 <- '
#' ------------------------------------------------------------------
#' |    Date    | Emp1 | Case  | Priority | PriorityCountinLast7days |
#' ------------------------------------------------------------------
#' | 2018-06-01 | A    | "A 1" |        0 |                        0 |
#' | 2018-06-03 | A    | "A 2" |        0 |                        1 |
#' | 2018-06-02 | B    | "B 2" |        0 |                        2 |
#' | 2018-06-03 | B    | "B 3" |        0 |                        3 |
#' ------------------------------------------------------------------
#' '
#' 
#' x3 <- "
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
#' x4 <- "
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
#' lapply(c(x1, x2, x3, x4), dtf.clean)

dtf.clean <- function(x, header=TRUE, ...) {
    # https://stackoverflow.com/questions/52023709
    # read each row as a character string
    x <- scan(text=x, what="character", sep="\n")

    # keep only lines containing alphanumerics
    x <- x[grep("[[:alnum:]]", x)]

    # remove vertical bars with trailing or leading space
    x <- gsub("\\| | \\|", " ", x)

    # read the result as a table
    read.table(text=paste(x, collapse="\n"), header=header, ...)    
}

