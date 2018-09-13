#' PCA mean
#' 
#' Takes the average of several PCA objects
#' 
#' @param ... PCA objects, or a single list of PCA objects
#' 
#' @details I don't know if this sort of calculation has any kind of merit. It was
#' written more as an impromptu challenge than as a solution to any problem
#' 
#' @export
#' 
#' @examples
#' xx <- data.frame(bee=c(0, 0, 1, 2, 3, 2, 0, 3), 
#'                 wasp=c(1, 3, 2, 0, 1, 1, 2, 1), 
#'                   fly=c(1, 2, 4, 2, 1, 0, 1, 0),
#'               beetle=c(1, 0, 0, 1, 2, 2, 0, 2))
#' 
#' set.seed(1)
#' r <- 1000
#' xxs <- replicate(r, {
#'   xx$random <- sample(c(0:1, 0:4), 8, r=TRUE)
#'   xx
#'   }, simplify=FALSE)
#' 
#' xxm <- Reduce("+", xxs) / r
#' xxl <- lapply(xxs, princomp)
#' 
#' biplot(mean.pca(xxl))
#' biplot(princomp(xxm))

mean.pca <- function(...) {
	prc <- ..1
	len <- ...length()
	xxl <- list(...)
	if (len == 1) {
		xxl <- ..1
		prc <- xxl[[1]]
		len <- length(xxl)
	}
	if (class(prc) == "prcomp") {
		el <- c("sdev", "rotation", "center", "scale", "x")
		for (i in el) {
	        prc[[i]] <- Reduce("+", lapply(xxl, "[[", i)) / len
		}

	} else if (class(prc) == "princomp") {
		el <- c("sdev", "loadings", "center", "scale", "scores")
		for (i in el) {
	        prc[[i]] <- Reduce("+", lapply(xxl, "[[", i)) / len
		}
		class(prc[["loadings"]]) <- "loadings"

	} else if (class(prc) == "factanal") {
		el <- c("loadings", "uniquenesses", "correlation", "criteria",
		        "rotmat", "STATISTIC", "PVAL")
		for (i in el) {
	        prc[[i]] <- Reduce("+", lapply(xxl, "[[", i)) / len
		}
		class(prc[["loadings"]]) <- "loadings"

	} else stop("not a reckognized PCA object")
	prc
}