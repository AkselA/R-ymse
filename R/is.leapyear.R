
is.leapyear <- function(x) {
	UseMethod("is.leapyear")
}

is.leapyear.default <- function(x) {
	(x %% 4 == 0) & ((x %% 400 == 0) | (x %% 100 != 0))
}

is.leapyear.Date <- function(x) {
	is.leapyear.default(as.POSIXlt(x)$year + 1900)
}

is.leapyear.POSIXct <- function(x) {
	is.leapyear.default(as.POSIXlt(x)$year + 1900)
}

is.leapyear.POSIXlt <- function(x) {
	is.leapyear.default(x$year + 1900)
}

# y <- as.Date(c("2000-2-2", "2004-2-2", "1900-2-2", "1901-2-2", "1904-2-2"))
# is.leapyear(y)

# d <- c(2000, 2004, 1900, 1901, 1904)
# is.leapyear(d)