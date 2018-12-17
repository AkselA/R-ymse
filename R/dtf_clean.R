#' Data cleanup
#' 
#' Create a data.frame from a messy table
#' 
#' @param x a messy table the form of a character string
#' @param header does the table include headers? (default TRUE)
#' @param stringsAsFactors should strings be read as factors? (default FALSE)
#' @param na.strings a vector of character strings which will be interpreted
#' as missing values
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
#' x5 <- "
#'        Season   |   Team  | W | AHWO
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
#' lapply(c(x1, x2, x3, x4), dtf_clean)

dtf_clean <- function(x, header=TRUE, na.strings=c("NA", "N/A"), 
  stringsAsFactors=FALSE, ...) {
    # https://stackoverflow.com/questions/52023709
    # read each row as a character string
    x <- scan(text=x, what="character", sep="\n", quiet=TRUE)

    # keep only lines containing alphanumerics
    x <- x[grep("[[:alnum:]]", x)]

    # remove vertical bars with trailing or leading space
    x <- gsub("\\| | \\|", " ", x)

    # read the result as a table
    read.table(text=paste(x, collapse="\n"), header=header, 
      na.strings=na.strings, stringsAsFactors=stringsAsFactors, ...)    
}

