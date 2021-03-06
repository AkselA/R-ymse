weekday_english <- function(x, short=TRUE) {
	if (short) {
	    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[x]
	} else {
		c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
		  "Saturday", "Sunday")[x]
	}
}

# The days of the week aren't considered proper nouns in Norwegian, and
# hence shouldn't be capitalized

weekday_nn_norwegian <- function(x, short=TRUE) {
	if (short) {
	    c("m\U00E5n", "tys", "ons", "tor", "fre", "laur", "sun")[x]
	} else {
		c("m\U00E5ndag", "tysdag", "onsdag", "torsdag", "fredag",
		  "laurdag", "sundag")[x]
	}
}

weekday_bm_norwegian <- function(x, short=TRUE) {
	if (short) {
	    c("man", "tir", "ons", "tor", "fre", "l\U00F8r", "s\U00F8n")[x]
	} else {
		c("mandag", "tirsdag", "onsdag", "torsdag", "fredag",
		  "l\U00F8rdag", "s\U00F8ndag")[x]
	}
}

#' Week-day names
#' 
#' Convert numeric, character, factor and date-time vectors to week-day names
#' 
#' @param x a vector
#' @param language what language the names should be returned in
#' @param short if \code{TRUE} the names will be returned in shortened form
#' @param ... further arguments passed to methods
#' 
#' @details
#' This function follows the ISO 8601 standard, meaning that Monday is
#' considered the first day of the week.
#' 
#' @export
#' 
#' @examples
#' weekday(c("c", "b", "a"))
#' weekday(c("3", "2", "1"))
#' weekday(3:1)
#' 
#' weekday(Sys.Date())
#' weekday(Sys.Date(), short=FALSE, lang="nn nor")

weekday <- function(x, ...) {
	UseMethod("weekday")
}

#' @rdname weekday
#' @export

weekday.default <- function(x, short=TRUE, 
  language=c("english", "nn norwegian", "bm norwegian"),
  ...) {
  	if (!is.integer(x)) {
	    x <- type.convert(x)
	}
	language <- match.arg(language)
    switch(language,
      "english"=weekday_english(x, short),
      "nn norwegian"=weekday_nn_norwegian(x, short),
      "bm norwegian"=weekday_bm_norwegian(x, short)
    )
}

#' @rdname weekday
#' @export

weekday.Date <- function(x, ...) {
	x <- as.integer(format(x, format="%u"))
    weekday.default(x, ...)
}

#' @rdname weekday
#' @export

weekday.POSIXt <- function(x, ...) {
	x <- as.integer(format(x, format="%u"))
    weekday.default(x, ...)
}
