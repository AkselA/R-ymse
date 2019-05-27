var_th <- function(p, 
  distribution=c("uniform", "exponential", "gamma", "t", "students-t", 
  "bates", "binomial", "nbinom", "negative binomial", "beta", "f",
  "geometric", "hypergeometric", "lognormal", "log-normal", "weibull",
  "signed-rank", "rank-sum", "logistic")) {
	dist <- match.arg(distribution)
	var <- switch(dist,
	  "uniform"={
	  	namp <- names(p)
        names(p)[namp == "a"] <- "min"
        names(p)[namp == "b"] <- "max"
	    v <- ((p["max"]-p["min"])^2)/12
	    v[[1]]
	  },
	  "exponential"=p^-2,
	  "gamma"=ifelse("rate" %in% names(p), 
	    p["shape"]/p["rate"]^2, 
	    p["shape"]*p["scale"]^2)[[1]],
	  "t"=,
	  "students-t"=ifelse(p > 2,
	    p/(p - 2),
	    Inf),
	  "bates"={
	  	namp <- names(p)
        names(p)[namp == "a"] <- "min"
        names(p)[namp == "b"] <- "max"
	    v <- ((p["max"]-p["min"])^2)/(12*p["n"])
	    v[[1]]
	  },
	  "binomial"={
	  	namp <- names(p)
        names(p)[namp == "n"] <- "size"
        names(p)[namp == "p"] <- "prob"
	    v <- p["size"]*p["prob"]*(1 - p["prob"])
	    v[[1]]
	  },
	  "nbinom"=,
	  "negative binomial"={
	  	namp <- names(p)
        names(p)[namp == "n"] <- "size"
        names(p)[namp == "p"] <- "prob"
	  	if (!"prob" %in% namp) {
	  		p["prob"] <- p["size"]/(p["size"] + p["mu"])
	  	}
	    v <- p["size"]*(1 - p["prob"])/p["prob"]^2
	    v[[1]]
	  },
	  "beta"={
	  	namp <- names(p)
        names(p)[namp == "shape1"] <- "a"
        names(p)[namp == "shape2"] <- "b"
        v <- p["a"]*p["b"] / ((p["a"]+p["b"])^2 * (p["a"]+p["b"]+1))
	    v[[1]]
	  },
	  "f"={
	  	namp <- names(p)
        names(p)[namp == "df1"] <- "d1"
        names(p)[namp == "df2"] <- "d2"
        v <- ifelse(p["d2"] > 2,
          (2*p["d2"]^2 * (p["d1"]+p["d2"]-2)) / 
            (p["d1"]*(p["d2"]-2)^2 * (p["d2"]-4)),
          Inf)
        v[[1]]
	  },
	  "geometric"=(1-p)/p^2,
	  "hypergeometric"={
	    pr <- p["m"]/(p["m"]+p["n"])
	    v <- p["k"]*pr*(1-pr)*(p["m"]+p["n"]-p["k"])/(p["m"]+p["n"]-1)
	    v[[1]]
	  },
	  "lognormal"=,
	  "log-normal"={
	  	namp <- names(p)
        names(p)[namp == "sdlog"] <- "sd"
        names(p)[namp == "meanlog"] <- "mean"
        v <- exp(2*p["mean"] + p["sd"]^2)*(exp(p["sd"]^2) - 1)
        v[[1]]
      },
      "weibull"={
	  	namp <- names(p)
        names(p)[namp == "shape"] <- "a"
        names(p)[namp == "scale"] <- "b"
        v <- p["b"]^2 * (gamma(1 + 2/p["a"]) - (gamma(1 + 1/p["a"]))^2)
        v[[1]]
      },
      "signrank"=,
      "signed-rank"=p*(p+1)*(2*p+1)/24,
      "wilcox"=,
      "rank-sum"=(p["m"]*p["n"]*(p["m"]+p["n"]+1)/12)[[1]],
      "logistic"=pi^2 * p^2 / 3
	)
	var
}



# var_th(p=data.frame(min=1:2, max=5:6), dist="unif")
# var(runif(1e5, 1, 5))

# var_th(p=2:3, dist="exp")
# var(rexp(1e5, 2))

# var_th(p=data.frame(shape=3:2, scale=c(0.8, 1)), dist="gamma")
# var(rgamma(1e5, shape=3, scale=0.8))

# var_th(p=c(shape=3, rate=1.25), dist="gamma")
# var(rgamma(1e5, shape=3, rate=1.25))

# var_th(p=18:20, dist="t")
# var(rt(1e5, 18))

# var_th(p=c(a=1, b=2, n=3), dist="bates")
# var(rbates(1e5, a=1, b=2, nr=3))

# var_th(p=c(size=10, prob=0.8), dist="binom")
# var(rbinom(1e5, 10, 0.8))

# var_th(p=c(size=10, prob=0.8), dist="nbinom")
# var(rnbinom(1e5, size=10, prob=0.8))

# var_th(p=c(size=10, mu=2), dist="nbinom")
# var(rnbinom(1e5, size=10, mu=2))

# var_th(p=data.frame(shape1=c(1, 2), shape2=c(1.5, 1)), dist="beta")
# var(rbeta(1e5, shape1=1, shape2=1.5))
# var(rbeta(1e5, shape1=2, shape2=1))

# var_th(p=c(df1=6, df2=11), dist="f")
# var(rf(1e5, 6, 11))

# var_th(p=c(m=3, n=3, k=2), dist="hypergeom")
# var(rhyper(1e5, m=3, n=3, k=2))

# var_th(p=c(meanlog=0, sdlog=1), dist="log-normal")
# var(rlnorm(1e5, meanlog=0, sdlog=1))

# var_th(p=c(shape=2, scale=1), dist="weibull")
# var(rweibull(1e5, shape=2, scale=1))

# var_th(p=20, dist="signed-rank")
# var(rsignrank(1e5, n=20))

# var_th(p=c(m=13, n=10), dist="rank-sum")
# var(rwilcox(1e5, m=13, n=10))
