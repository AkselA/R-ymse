#' File extension
#' 
#' Separate file name and extension from a file path
#' 
#' @param x a character vector
#' 
#' @details
#' If the supplied file name has several extensions, f.ex. like 
#' \code{foobar.tar.bz}, only the last extension will be considered.
#' 
#' @return
#' \code{file_ext} returns the file extension of each file path. \code{file_name}
#' returns the file name, no extension, of each file path. \code{file_name_ext}
#' returns both name and extension, but arranged in separate columns of a matrix.
#' 
#' @seealso \code{\link{basename}}
#' 
#' @export
#' 
#' @examples
#' x <- c("/hg/.gi.tar.gz", "ff/hg/hh.pdf", "git", ".History", ".History.log")
#' 
#' file_ext(x)
#' file_name(x)
#' file_name_ext(x)

file_ext <- function(x) {
	sp <- strsplit(x, ".+\\.")
	le <- pmax(lengths(sp), 2)
	sapply(seq_along(sp), function(i) sp[[i]][le[i]])
}

#' @rdname file_ext
#' @export

file_name <- function(x) {
	sp <- strsplit(x, "(?<=.)\\.", perl=TRUE)
	le <- -pmax(lengths(sp), 2)
	sapply(seq_along(sp), function(i) paste(sp[[i]][le[i]], collapse="."))
}

#' @rdname file_ext
#' @export

file_name_ext <- function(x) {
	sp <- strsplit(x, "(?<=.)\\.", perl=TRUE)
	le <- pmax(lengths(sp), 2)
	ne <- sapply(seq_along(sp), 
	  function(i) {
	  	  spi <- sp[[i]]
	  	  lei <- le[i]
	  	  c(paste(spi[-lei], collapse="."), spi[lei])
	  }
	)
	rownames(ne) <- c("name", "extension")
	t(ne)
}
