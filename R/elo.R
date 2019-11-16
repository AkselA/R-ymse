#' Elo rating
#' 
#' Calculate updated Elo ratings based on existing rating and results from matches
#' 
#' @param ra rating of player A
#' @param rb rating of player B
#' @param score score respective to player A. Numeric or character;
#' 1, 0.5, 0; win, tie, loss
#' @param k a measure of how big the correction should be
#' @param sum calculate the new rating based on sum of scores and opponents ratings
#' 
#' @examples
#' # as in example from
#' # https://en.wikipedia.org/wiki/Elo_rating_system#Mathematical_details
#' # per 2019-10-04
#' ra <- 1613
#' rb <- c(1609, 1477, 1388, 1586, 1720)
#' score <- c(0, 0.5, 1, 1, 0)
#' elo_upd(ra, rb, score, k=32, sum=FALSE)
#' elo_upd(ra, rb, score, k=32, sum=TRUE)
#' 
#' elo_upd_pw(c(1400, 1500), c(1300, 1400), c("w", "t"))
#' 
#' results <- read.table(text="
#' Player1   Player2   Result
#' Alice     Bob       Win
#' Charlie   Dennis    Loss
#' Elena     Frank     Loss
#' June      Rashida   Tie", header=TRUE, stringsAsFactors=FALSE)
#' 
#' scores <- read.table(text="
#' Player    Score
#' Alice     1150
#' Charlie   1150
#' Frank     1150
#' Bob       800
#' Dennis    800
#' Elena     800
#' June      900
#' Rashida   1100", header=TRUE, stringsAsFactors=FALSE)
#' 
#' rownames(scores) <- scores$Player
#' 
#' r2 <- results
#' r2[,1:2] <- scores[as.matrix(r2[,1:2]), 2]
#' 
#' r2u <- elo_upd_pw(r2)
#' 
#' scores.new <- data.frame(Score=c(r2u))
#' rownames(scores.new) <- as.matrix(results[,1:2])
#' 
#' scores.new <- round(scores.new[rownames(scores),, drop=FALSE])
#' scores.new$diff <- scores.new$Score - scores$Score
#' scores.new
#' 
#' @name Elo_rating

NULL

#' @rdname Elo_rating
#' @export elo_upd

# update rating
elo_upd <- function(ra, rb, score, k=16, sum=length(rb) > 1) {
	if (is.character(score)) {
        score <- toupper(substr(score, 1, 1))
        score <- (match(score, c("L", "T", "W"))-1)/2
        if (any(is.na(score))) {
        	stop("Not all strings in 'score' are interpretable")
        } else {
        	if (!in_range(score, 0, 1)) {
        		stop("'score' values must be in the range [0, 1]")
        	}
        }
	}
	if (sum) {
		ra + k*(sum(score) - sum(exps(ra, rb)))
	} else {
	    ra + k*(score - exps(ra, rb))
	}
}

#' @rdname Elo_rating
#' @export elo_upd_pw

# update ratings pairwise
elo_upd_pw <- function(ra, rb, score, k=16) {
	if (NCOL(ra) == 3) {
		rb <- ra[,2]
		score <- ra[,3]
		ra <- ra[,1]
	}
	if (is.character(score)) {
        score <- toupper(substr(score, 1, 1))
        score <- (match(score, c("L", "T", "W"))-1)/2
        if (any(is.na(score))) {
        	stop("Not all strings in 'score' are interpretable")
        } else {
        	if (!all(in_range(score, 0, 1))) {
        		stop("'score' values must be in the range [0, 1]")
        	}
        }
	}
	ea <- exps(ra, rb)
	eb <- 1 - ea
	sa <- score
	sb <- 1 - sa
	cbind(ra=ra + k*(sa - ea), rb=rb + k*(sb - eb))
}

# expected score
exps <- function(ra, rb) {
	d <- (rb - ra)/400
	1/(1 + 10^d)
}
