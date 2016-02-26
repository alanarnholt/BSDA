#' Simple Random Sampling
#' 
#' Computes all possible samples from a given population using simple random
#' sampling.
#' 
#' 
#' @param POPvalues vector containing the poulation values.
#' @param n the sample size.
#' @return Returns a matrix containing the possible simple random samples of
#' size \code{n} taken from a population \code{POPvalues}.
#' @author Alan T. Arnholt
#' @seealso \code{\link{Combinations}}
#' @keywords distribution
#' @examples
#' 
#' SRS(c(5,8,3),2)
#'     # The rows in the matrix list the values for the 3 possible
#'     # simple random samples of size 2 from the population of 5,8, and 3.
#' 
#' @export SRS
SRS <-
function(POPvalues,n)
{
# SRS generates all possible SRS's of size n
# from the population in vector POPvalues 
# by calling the function Combinations.
N <- length(POPvalues)
store <- t(Combinations(N,n))
matrix(POPvalues[t(store)],nrow=nrow(store),byrow=TRUE)
}
