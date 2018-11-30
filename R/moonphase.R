
moonphase <- function(N, phase=c("full", "new")) {
	d0 <- 29.530588861*N - 1.32974e-10*N^2
    phase <- match.arg(phase)
    d <- d0 + c(20.36126, 5.596922)[match(phase, c("full", "new"))]
    as.POSIXct(d*86400, origin="2000-01-01 00:00:00", tz="UTC")
}

nextfullmoon <- function(n=1) {
	a <- 20.36126
    b <- 29.530588861
    c <- -1.32974e-10

    td <- structure(-10957, class = "difftime", units = "days")
    d <- unclass(Sys.time() + td)/86400
    
    N <- -(-b + sqrt(b^2 + 4*c*(a - d)))/(2*c)
    N <- seq_len(n) - 1 + ceiling(N)

    d1 <- a + b*N + c*N^2
    as.POSIXct(d1*86400, origin="2000-01-01 00:00:00", tz="UTC")
}

nextfullmoon()