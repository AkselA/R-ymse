#' @export

table_pairwise <- function(x, ...) {
	tab <- table(x, ...)
    cbn <- as.data.frame(combn(1:ncol(x), 2))
    lapply(cbn, function(x) apply(tab, x, sum))
}

# set.seed(1)
# f <- c("cat", "dog", "hen")[sample(1:3, 10*4, rep=TRUE)]
# d1 <- data.frame(matrix(f, ncol=4), stringsAsFactors=FALSE)

# (tp <- table_pairwise(d1))

# round(apply(as.array(tp), 1:2, mean), 2)