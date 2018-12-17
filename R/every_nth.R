#' Select every n'th element
#' 
#' Select every second, third, fourth etc. element (or slice/hyperplane)
#' 
#' @param x an object to be selected from
#' @param n selection "step size"
#' @param start integer in [1:n] specifying the start of selection
#' @param margin for matrices, what margin to select along
#' @param ... further arguments passed to methods
#' 
#' @export

every_nth <- function(x, ...) {
	UseMethod("every_nth")
}

#' @rdname every_nth
#' @export

every_nth.default <- function(x, n=2, start=1, ...) {
	x[seq_along(x) %% n == start]
}

#' @rdname every_nth
#' @export

every_nth.matrix <- function(x, n=2, start=1, margin=1, ...) {
	d <- dim(x)[margin]
	if (margin == 1) x[1:d %% n == start, ]
	else
	if (margin == 2) x[, 1:d %% n == start]
    else stop("invalid margin")
}

#' @rdname every_nth
#' @export

every_nth.data.frame <- function(x, n=2, start=1, ...) {
	x[1:nrom(x) %% n == start,]
}

#' @rdname every_nth
#' @export

every_nth.list <- function(x, n=2, start=1, ...) {
	x[1:length(x) %% n == start]
}

# ## every_nth.array
# m <- matrix(1:20, 5)
# every_nth(m, margin=1)
# every_nth(m, margin=2)
# every_nth(m, margin=3)


# l <- list(a=1:3, b=2:6, c=8:5, d=9:7)
# every_nth(l)

# arr3 <- array(1:4^3, rep(4, 3))
# n <- c(1, 3)

# arr3[n,,]
# arr3[,n,]
# arr3[,,n]

# arr4 <- array(1:4^4, rep(4, 4))

# arr4[n,,,]
# arr4[,n,,]
# arr4[,,n,]
# arr4[,,,n]

# ndim <- length(dim(arr4))
# margin <- 2
# mar <- rep("", ndim)
# mar[margin] <- "n"
# mar <- paste(mar, collapse=",")
# mar

# eval(parse(text=paste0("arr4[", mar, "]")))

# s
