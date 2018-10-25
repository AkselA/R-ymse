#' Coerce a list to an array
#' 
#' Coerce a list consisting of data.frames or matrices of equal size to a 3d array
#' 
#' @param ll a list of equal sized data.frames or matrices
#' 
#' @return A list of length \eqn{l} with elements of \eqn{m} rows and \eqn{n} columns will
#' result in an \eqn{m \times n \times l}{m × n × l} array.
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
#' as.array(speedskate)

as.array.list <- function(ll) {
    arr <- array(data=unlist(ll), 
      dim=c(nrow(ll[[1]]), ncol(ll[[1]]), length(ll)), 
      dimnames=list(
        rownames(ll[[1]]), colnames(ll[[1]]), names(ll)))
    comment(arr) <- comment(ll)
    arr
}

