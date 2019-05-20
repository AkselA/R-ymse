rainbowHCL <- function (n, c=100, l=75, start=0, end=max(1, n - 1)/n, 
  alpha=1, s=NULL, v=NULL) {
    if ((n <- as.integer(n[1L])) > 0) {
        if (start == end || 
          any(c(start, end) < 0) || 
          any(c(start, end) > 1)) {
            stop("'start' and 'end' must be distinct and in [0, 1].")
        }
        h <- seq.int(start, ifelse(start > end, 1, 0) + end, length.out = n)%%1
        col <- hcl(h*360, c, l, alpha)
        if (is.null(s) + is.null(v) < 2) {
        	col <- adjustcolorHSV(col, s=s, v=v)
        }
        col
    }
    else character()
}

# n <- 25
# hcl0 <- rainbowHCL(n)
# hcl1 <- rainbowHCL(n, c=150, l=85)
# hcl2 <- adjustcolorHSV(rainbowHCL(n), s.f=1.5, v.f=1.2)
# hsv0 <- rainbow(n)

# cols <- rbind(hcl0, hcl1, hcl2, hsv0)

# mat2grid <- function(x) {
	# eg <- expand.grid(1:NCOL(x), NROW(x):1)
	# gd <- data.frame(eg, c(t(x)), stringsAsFactors=FALSE)
	# colnames(gd) <- c("x", "y", "z")
	# gd
# }

# pos <- mat2grid(cols)
# plot(pos[,1:2], pch=17, cex=3.5, col=pos[,3], ylim=c(0.5, 4.5))

