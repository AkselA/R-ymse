#' Discrete markov sequence
#' 
#' Generate a random discrete markov sequence
#' 
#' @param n length of the sequence
#' @param tmat a transition matrix
#' @param init the initial state
#' 
#' @export
#' 
#' @examples
#' m <- matrix(c(0.5, 0.3, 0.2,
#'               0.2, 0.6, 0.2,
#'               0.2, 0.3, 0.5), 3, byrow=TRUE)
#' 
#' set.seed(1)
#' ms <- markov_seq(n=1000, tmat=m)
#' 
#' colMeans(m)
#' prop.table(table(ms))
#' round(prop.table(table(head(ms, -1), tail(ms, -1), dnn=c("n", "n+1")), 1), 2)

markov_seq <- function(n=100, tmat=rbind(1:3, 3:1, 2:0), init=1) {
	nstates <- nrow(tmat)
	if (init > nstates) {
		stop("The initial state must be within the state space")
	}
	statespace <- seq_len(nstates)
	statevec0 <- vector(length=nstates)
	outvec <- vector(length=n)
	outvec[1] <- init
	
	for (n in seq_along(outvec)) {
		statevec <- statevec0
		statevec[outvec[n]] <- 1
		state <- sample(statespace, 1, prob=statevec %*% tmat)
		outvec[n+1] <- state
	}
	outvec[-1]
}
