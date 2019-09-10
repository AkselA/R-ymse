
centre.shrink <- function(x, trace=FALSE, break.ties=TRUE, cfun=mean) {
    cf <- match.fun(cfun)
    or <- order(x)
    so <- x[or]
    le <- length(so)
    ol <- numeric(le)
    
    if (break.ties) {
        rn <- sample.int(2, le, replace=TRUE)
        for (i in le:1) {
            mn <- cf(so)
            ht <- c(1, i)
        
            ad <- abs(so[ht] - mn)
            if (diff(ad) == 0) {
                wm <- rn[i]
            } else {
                wm <- which.max(ad)                
            }
            ix <- ht[wm]
            
            ol[i] <- so[ix]            
            so <- so[-ix]
        }
        ol
    } else {        
        for (i in le:1) {
            mn <- cf(so)
            ht <- c(1, i)
        
            ad <- abs(so[ht] - mn)
            wm <- which.max(ad)
            ix <- ht[wm]
            
            ol[i] <- so[ix]            
            so <- so[-ix]
        }
    }
    if (trace) {
        rev(ol)
    } else {
        ol[1]
    }
}



# set.seed(1)
# x <- round(rgamma(25, 2, 1)*50)

# cs <- centre.shrink(x, TRUE, cfun=mean)
# cm <- rev(cumsum(rev(cs))/1:length(cs))
# de <- abs(cs - cm)

# xy <- cbind(rep(length(cs), 3), c(mean(x), median(x), cmode(x)))
# cl <- c("red", "blue", "orange")

# par(mar=c(2, 2, 1, 1))
# plot(cs, type="o")
# lines(cm, col="green", lty=2)
# points(xy, col=cl)
# text(xy, c("mean", "median", "mode"), col=cl, pos=2, cex=0.8)

# plot(density(x))
# abline(v=xy[, 2], col=cl)
# abline(v=centre.shrink(x, FALSE), col="green")


# set.seed(1)
# cf <- function(x) mean(x)*0.7 + median(x)*0.3
# x <- round(rbeta(21, 1.1, 1.1)*10)
# plot(table(x))
# tr <- replicate(2e3, centre.shrink(x, trace=TRUE, break.ties=TRUE, cfun=cf))
# vs <- apply(tr, 2, function(x) sd(diff(x, d=2)))
# tru <- tr[,!duplicated(vs)]
# prop <- prop.table(table(round(vs, 3)))*100

# col <- randcolours(max(2, NCOL(tru)), space="Lab")
# lty <- rep(1:4, length=NCOL(tru))
# leg <- paste0(prop, "%")

# matplot(tru, type="l", col=col, lty=lty, lwd=2)
# legend("topright", legend=leg, col=col, lty=lty, bty="n", lwd=2, cex=0.9)

