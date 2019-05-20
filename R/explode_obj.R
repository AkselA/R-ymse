#' Explode object
#' 
#' Presents an R object in an exploded form
#' 
#' @param x an R oject, or a character string describing an R object
#' @param indent how many spaces for indention (and exdention) at each level
#' 
#' @details
#' If \code{x} is an R oject it is first deparsed and converted into a character
#' string describing the object. This string is then unwrapped, or exploded, 
#' according to these rules: newline and exdention after each open parenthesis,
#' newline and indention after each close parenthesis, and newline after each comma.
#' Parentheses and commas forming part of character strings are ignored.
#' 
#' @return
#' An exploded representation of the object is printed to console, and returned
#' invisibly. The output is in most cases a complete and reproducible
#' representation of the object, similarly to \code{dput}, but less compact and
#' more reaviling of its inner structure.
#' 
#' @seealso \code{\link{dput}}, \code{\link{dput2}}
#' 
#' @export
#' 
#' @examples
#' xc <- 'list(v=1, A=c("abv", "bom"), B=c(1:3, 31, 28), list("foo", "bar", 1))'
#' explode_obj(xc)
#' 
#' xl <- list(O=NA, R=list(j=1:3, h="(a)", q=structure(list(a=1:2, b=c("A, K",
#'   "B, L")), class="data.frame", row.names=c(NA, -2L))), N=1, L=FALSE)
#' explode_obj(xl)
#' 
#' mt <- 'coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
#'        panel = panel.smooth, rows = 1)'
#' explode_obj(mt)

explode_obj <- function(x, indent=2) {

	if (is.character(x) & length(x) == 1) {
	    dep <- x
	} else {
	    dep <- paste(deparse(x), collapse="")
	}
	
	# To avoid splitting character strings
	ddl <- strsplit(dep, "\"")[[1]]
	chstr <- 1:length(ddl) %% 2 == 0
	
	# Replace chr comma+space with comma+nbsp
	ddl[chstr] <- gsub(", ", ",\u00A0", ddl[chstr])

    # Replace newlines with space
	ddl[!chstr] <- gsub("\n+", " ", ddl[!chstr])
	
	# Replace non-chr leftparen+space{0 or more} with leftparen+zwsp
	ddl[!chstr] <- gsub("\\( *", "\\(\u200B", ddl[!chstr])

	# Replace non-chr space{0 or more}+rightparen with zwsp+rightparen
	ddl[!chstr] <- gsub(" *\\)", "\u200B\\)", ddl[!chstr])
	dep <- paste(ddl, collapse="\"")
	
	dep <- gsub("  +", " ", dep)
	dep <- gsub(" = ", "=", dep)

	# Newline at comma+space, leftparen+zwsp and zwsp+rightparen
	dep <- gsub(", ", ",\n", dep)
	dep <- gsub("\\(\u200B", "\\(\u200B\n", dep)
	dep <- gsub("\u200B\\)", "\n\u200B\\)", dep)
	
	# Exdent after leftparen+zwsp and indent before zwsp+rightparen
	v <- readLines(textConnection(dep))

	lp <- grepl("\\(\u200B", v)
	rp <- grepl("\u200B\\)", v)
	exd <- cumsum(c(0, lp) - c(rp, 0))[1:length(v)]
	
	exd <- sapply(exd*indent, function(x) paste(rep(" ", x), collapse=""))
	v <- paste0(exd, v)
	
	# Replace nbsp and zwsp
	v <- gsub("\u00A0", " ", v)
	v <- gsub("\u200B", "", v)
	
	cat(v, sep="\n")
	invisible(v)
}