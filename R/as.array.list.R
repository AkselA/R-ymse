#' Coerce a list to an array
#' 
#' Coerce a list consisting of data.frames or matrices of equal size to a 3d array
#' 
#' @param x a list of equal sized data.frames or matrices
#' @param ... (not used)
#' 
#' @return A list of length \eqn{l} with elements of \eqn{m} rows and \eqn{n} 
#' columns wix result in an \eqn{m \times n \times l}{m × n × l} array.
#' 
#' @export
#' 
#' @examples
#' df1 <- data.frame(x=c(1, 2, 3), y=c(2, 3, 4), z=c(3, 4, 5))
#' df2 <- data.frame(x=c(4, 2, 3), y=c(2, 5, 4), z=c(3, 4, 6))
#' df3 <- data.frame(x=c(1, 4, 2), y=c(3, 3, 8), z=c(4, 3, 5))
#' 
#' l <- list(df1, df2, df3)
#' 
#' as.array(l)
#'
#' llm <- list(matrix(LETTERS[1:6], 2), 
#'             matrix(LETTERS[7:12], 2))
#' 
#' as.array(llm)
#' 
#' as.array(speedskate)

as.array.list <- function(x, ...) {
    arr <- array(data=unlist(x), 
      dim=c(nrow(x[[1]]), ncol(x[[1]]), length(x)), 
      dimnames=list(
        rownames(x[[1]]), colnames(x[[1]]), names(x)))
    comment(arr) <- comment(x)
    arr
}