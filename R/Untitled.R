#' Birthday function to find probabilities
#'
#' Computes the probability that in a group of size `n`, at least two people
#' share a birthday, assuming 365 equally likely birthdays.
#'
#' @param x integer, group size(s)
#'
#' @returns numeric probability in [0,1]
#' @export
#'
#' @examples
#' birthday(20:24)
birthday <- function(x) { 1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))}

birthday(20:24)
