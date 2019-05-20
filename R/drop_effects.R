#' Drop random effects
#' 
#' Drop random effects from a mixed effects model formula
#' 
#' @param form a formula object
#' 
#' @details
#' \code{form} is divided into its individual terms, any term containg a vertical
#' bar (\code{|}) is removed, before \code{form} is updated and returned. 
#' In case \code{form} has no random effect terms, \code{form} is returned 
#' unmodified. In case all effects are random, only the intercept is retained.
#' In any case the response variable(s) are kept as is.
#' 
#' @return
#' A formula object
#' 
#' @seealso \code{\link{drop_pattern}}
#' 
#' @examples
#' f1 <- Reaction ~ (1 + Days | Subject)
#' f2 <- Reaction ~ (1 | mygrp/mysubgrp) + (1 | Subject)
#' f3 <- Reaction ~ x1 + x2 + (1 + Days | Subject)
#' f4 <- Reaction ~ x1 * x2 + (1 | mygrp/mysubgrp) + (1 | Subject)
#' f5 <- Reaction ~ x1 + x2
#' 
#' sapply(list(f1, f2, f3, f4, f5), drop_randfx)
#' 
#' @export

drop_randfx <- function(form) {
	form.t <- delete.response(terms(form))
	dr <- grepl("|", labels(form.t), fixed=TRUE)
	x <- mean(dr)
	case <- as.character(floor(x) + ceiling(x))
    switch(case, 
      "0"=form,
      "1"=update(form, drop.terms(form.t, which(dr))),
      "2"=update(form, . ~ 1)
     )
}

#' Drop predictors
#' 
#' Drop predictor variables according to a (regex) pattern 
#' 
#' @param form a formula object
#' @param pattern predictors matching this pattern will be dropped
#' @param ... further arguments passed on to \code{\link{grepl}}
#' 
#' @details
#' \code{form} is divided into its individual terms, any term matching 
#' \code{pattern} is removed, before \code{form} is updated and returned. 
#' In case no match is made, \code{form} is returned unmodified. 
#' In case all predictors match, only the intercept is retained.
#' In any case the response variable(s) are kept as is.
#' 
#' @return
#' A formula object
#' 
#' @seealso \code{\link{drop_randfx}}
#' 
#' @examples
#' f6 <- y ~ aa*bb + aa + ac + cc + acab
#' 
#' drop_pattern(f6, "a") # Drop all containing a
#' drop_pattern(f6, "a{2}") # Drop all containing exactly 2 consecutive as
#' drop_pattern(f6, "^[^a]*a[^a]*$")  # All containing exactly 1 a
#' drop_pattern(f6, ":") # Drop interaction
#' drop_pattern(f6, "^[^:]*a[^:]*$") # Drop all containg a, but not interaction
#' drop_pattern(f6, "^((?!a).)*$", perl=TRUE) # Drop all not containing a
#' 
#' # Degenerate cases
#' drop_pattern(f6, "[abc]") # Drop all
#' drop_pattern(f6, "q") # Drop none
#' 
#' @export

drop_pattern <- function(form, pattern, ...) {
	form.t <- delete.response(terms(form))
	dr <- grepl(pattern, labels(form.t), ...)
	x <- mean(dr)
	case <- as.character(floor(x) + ceiling(x))
    switch(case, 
      "0"=form,
      "1"=update(form, drop.terms(form.t, which(dr))),
      "2"=update(form, . ~ 1)
     )
}