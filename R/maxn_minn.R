
x <- c(1, 2, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7, 8, NA)

max2 <- function(x, na.rm=FALSE) {
    max(x[-which.max(x)], na.rm=na.rm)
}

# The second largest value, not the second largest unique value
max2(x, na.rm=TRUE)

min2 <- function(x, na.rm=TRUE) {
    min(x[-which.min(x)], na.rm=na.rm)
}

min2(x, na.rm=TRUE)


which.max2 <- function(x) {
    x[which.max(x)] <- -Inf
    which.max(x)
}

# Returns the position of the second largest value.
# If it's duplicate still only one position is returned.
# If max(x) is unique, but max(2) is duplicate, the first
# position is returned. If max(x) is duplicate, the second
# position is returned.
which.max2(x)

which.min2 <- function(x) {
    x[which.min(x)] <- Inf
    which.min(x)
}

which.min2(x)


maxn <- function(x, n=1:3, na.rm=FALSE) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	p <- length(x)-(n-1)
	sort(x, partial=p)[p]
}

# The three largest values, not the three largest unique value
maxn(x, na.rm=TRUE)

# The fifth and eighth largest values
maxn(x, c(5, 8), na.rm=TRUE)

# Same, but much slower
sort(x, decreasing=TRUE)[c(5, 8)]

set.seed(1)
s <- sample(1:(5*10^7))

maxn(s, c(2, 4))
sort(s, decreasing=TRUE)[c(2, 4)]


minn <- function(x, n=1:3, na.rm=FALSE) {
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	p <- length(x)-(n-1)
	-sort(-x, partial=p)[p]
}

which.maxn <- function(x, n=1:3, na.rm=FALSE, 
  keep=c("all", "first", "last", "random", "max", "min")) {
  	x0 <- x
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	p <- length(x)-(n-1)
	w <- which(x0 %in% sort(x, partial=p)[p])
	if (keep[1] == "all") {
		w
	} else {
		xw <- x0[w]
		sn <- seq_along(n)
		keep <- match.arg(keep)
		switch(keep,
		  all=w,
		  first=minn(w, sn),
		  last=maxn(w, sn),
		  random=sample(w, length(n)),
		  min=w[order(xw)][sn],
		  max=w[order(-xw)][sn]
		)
	}
}

#       1  2  3  4  5  6  7   8  9 10 11 12 13 14 15 16  17 
x <- c(NA, 1, 2, 3, 4, 3, 5, NA, 7, 7, 8, 7, 6, 6, 8, 7, NA)

which.maxn(x, 1:3, na.rm=TRUE)
which.maxn(x, 1:3, na.rm=TRUE, keep="first")
which.maxn(x, 1:3, na.rm=TRUE, keep="last")
which.maxn(x, 1:3, na.rm=TRUE, keep="min")
which.maxn(x, 1:3, na.rm=TRUE, keep="max")

which.maxn(x, 1:3, na.rm=TRUE, keep="random")

which.minn <- function(x, n=1:3, na.rm=FALSE, 
  keep=c("all", "first", "last", "random", "max", "min")) {
  	x0 <- x
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	p <- length(x)-(n-1)
	w <- which(x0 %in% -sort(-x, partial=p)[p])
	if (keep[1] == "all") {
		w
	} else {
		xw <- x0[w]
		sn <- seq_along(n)
		keep <- match.arg(keep)
		switch(keep,
		  all=w,
		  first=minn(w, sn),
		  last=maxn(w, sn),
		  random=sample(w, length(n)),
		  min=w[order(xw)][sn],
		  max=w[order(-xw)][sn]
		)
	}
}

#       1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18  19
x <- c(NA, 1, 2, 1, 3, 4, 2, 5, NA, 2, 7, 7, 7, 8, 6, 6, 8, 7, NA)

# 2, 4, and 3, 7, 10 are ties 
which.minn(x, 1:3, na.rm=TRUE) 

# keeps the three first (val = 1, 2, 1)
which.minn(x, 1:3, na.rm=TRUE, keep="first")

# keeps the three last (val = 2, 2, 1)
which.minn(x, 1:3, na.rm=TRUE, keep="last")

# keeps the three highest (val = 2, 2, 2)
which.minn(x, 1:3, na.rm=TRUE, keep="max")

# keeps the three lowest (val = 1, 1, 2)
which.minn(x, 1:3, na.rm=TRUE, keep="min")

# keeps three at random
which.minn(x, 1:3, na.rm=TRUE, keep="random")


# 
which.high <- function(x, n=2, keep=c("first", "last", "random"),
  na.rm=FALSE) {
  	x0 <- x
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	u <- unique(maxn(x, 1:n))
    s <- lapply(u, function(y) which(x0 == y))
    if (keep[1] != "first" & length(s[[l <- length(s)]]) > 1) {
    	keep <- match.arg(keep)
    	s[[l]] <- switch(keep,
    	  first=s[[l]],
    	  last=rev(s[[l]]),
    	  random=sample(s[[l]])
    	)
    }
    unlist(s)[1:n]
}

which.low <- function(x, n=2, keep=c("first", "last", "random"),
  na.rm=FALSE) {
  	x0 <- x
	if (na.rm) {
		x <- x[!is.na(x)]
	}
	u <- unique(minn(x, 1:n))
    s <- lapply(u, function(y) which(x0 == y))
    if (keep[1] != "first" & length(s[[l <- length(s)]]) > 1) {
    	keep <- match.arg(keep)
    	s[[l]] <- switch(keep,
    	  first=s[[l]],
    	  last=rev(s[[l]]),
    	  random=sample(s[[l]])
    	)
    }
    unlist(s)[1:n]
}

#       1  2  3  4  5  6  7  8   9 10 11 12 13 14 15 16 17 18  19
x <- c(NA, 1, 2, 1, 3, 4, 2, 5, NA, 2, 7, 7, 7, 8, 6, 6, 8, 7, NA)


which.low(x, 3, keep="first", na.rm=TRUE)
which.low(x, 3, keep="last", na.rm=TRUE)

which.high(x, 3, keep="first", na.rm=TRUE)
which.high(x, 3, keep="last", na.rm=TRUE)
