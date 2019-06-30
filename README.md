# ymse
**ymse** [y`msə] 
adjective

	Various, of various kind, not specified

Old Norse *ýmsir*, *ýmissir*, pl. *ýmiss*, *ímiss*  

### Usage
```R
library(devtools)
install_github("AkselA/R-ymse")
library(ymse)

# Effect of an uneven d20 die on summed PDF 
d20l <- d20 <- dice(20)
d20l[c(16, 11)] <- 0.6
d20l[c(3, 20, 18, 19)] <- 1.2

c0 <- combodice(list(dice(6), dice(10), d20), method="conv", name="fair")
cl <- combodice(list(dice(6), dice(10), d20l), method="conv", name="uneven")

set_mar()
plot(c0, type="o", pch=16, col="grey")
points(cl, col=2, type="o", lwd=1, pch=16, cex=0.6)
legend("topright", c("fair", "uneven"), bty="n", col=c("grey", "red"), lwd=2:1)
```

![graph](https://raw.githubusercontent.com/AkselA/R-ymse/master/dice.png)