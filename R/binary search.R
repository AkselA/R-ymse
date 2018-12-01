#' Binary search
#' 
#' Find the position of a given value in a sorted array
#' 
#' @param val the value to search for
#' @param arr a sorted array to make the search in
#' @param L a lower bound
#' @param H an upper bound
#' 
#' @details While both \code{val} and \code{arr} can be either integer or double, the
#' algorithm is limited by integer storage in how long the array can be.
#' \code{L} and \code{H} can be used to limit the range of indices to be search within. \cr
#' \code{binsearch} will return either the index of the exact match, or the index just below
#' if no exact match is found. This means that if \code{val} is less than the lowest value
#' in \code{arr} (and \code{L=1}), a \code{0} will be returned, which can lead to issues as
#' such an index does not exist in R. An array indexed by \code{0} will return a zero 
#' length object.
#' \code{binclosest} will return the index of the closest match, and therefore a \code{1} 
#' in the situation where \code{binsearch} returns a \code{0}. If there is a tie the lower
#' index will be returned. \cr
#' In either case, if there are duplicate matches, the lower index will be returned. 
#' 
#' @return A single integer representing an index on the input array.
#' 
#' @export
#' 
#' @examples
#' binsearch(15, (1:9)*3.333)
#' binsearch(2, (1:9)*3.333)
#' binclosest(2, (1:9)*3.333)
#' 
#' binsearch(18, seq_len(2e9))
#' binsearch(18, seq_len(3e9))
#' binsearch(18, seq_len(3e9), H=2e9)
#' binsearch(2000, seq_len(3e7)*100 + 10.71)
#' 
#' set.seed(1)
#' x <- sort(sample(1:300, 30))
#' r <- sort(sample(1:300, 30))
#' 
#' plot(sapply(r, binsearch, x), type="l")
#' lines(sapply(r, binclosest, x), col="red")
#' 
#' x <- c(1, 2, 3, 5, 8, 9)
#' binclosest(6, x)
#' binclosest(7, x)
#' binclosest(5, x)

binsearch <- function(val, arr, L=1L, H=length(arr)) {
	if (H - L > 2147483646) {
		stop("Array length exeeding integer range (2^31 - 1 = 2,147,483,647)")
	}
     while (H >= L) { 
         M <- L + (H - L) %/% 2L 
         if (arr[M] > val) {
         	H <- M - 1L
         } else {
         if (arr[M] < val) {
             L <- M + 1L
         } else {
         	return(M)
         }
         }
     } 
     L - 1L
}

#' @rdname binsearch
#' 
#' @export binclosest

binclosest <- function(val, arr, L=1L, H=length(arr)) {
	i <- binsearch(val, arr, L, H)
    if (i == 0) {
    	return(L)
    }
    l <- arr[i]
    u <- arr[i+1]
	i + (abs(val - l) > abs(val - u))
}