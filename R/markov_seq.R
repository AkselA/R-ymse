#' Discrete markov sequence
#' 
#' Generate a random discrete markov sequence
#' 
#' @param tmat a transition matrix
#' @param init the initial state
#' @param length length of the sequence
#' 
#' @export
#' 
#' @examples
#' m <- matrix(c(0.5, 0.3, 0.2,
#'               0.2, 0.6, 0.2,
#'               0.2, 0.3, 0.5), 3, byrow=TRUE)
#' 
#' set.seed(1)
#' ms <- markov_seq(m)
#' 
#' colMeans(m)
#' prop.table(table(ms))
#' prop.table(table(tail(ms, -1), head(ms, -1), dnn=c("n", "n+1")), 1)

markov_seq <- function(tmat, init=1, length=1e3) {
	nstates <- nrow(tmat)
	if (init > nstates) {
		stop("The initial state must be within the state space")
	}
	statespace <- seq_len(nstates)
	statevec0 <- vector(length=nstates)
	outvec <- vector(length=length)
	outvec[1] <- init
	
	for (n in seq_along(outvec)) {
		statevec <- statevec0
		statevec[outvec[n]] <- 1
		state <- sample(statespace, 1, prob=statevec %*% tmat)
		outvec[n+1] <- state
	}
	outvec[-1]
}