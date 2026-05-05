#' @title Burr Type XII Cumulative Distribution Function
#'
#' @description
#' Computes the cumulative distribution function (CDF) of the Burr Type XII distribution.
#'
#' @param x Numeric vector of values (x > 0).
#' @param c Positive shape parameter (c > 0).
#' @param k Positive shape parameter (k > 0).
#'
#' @return Numeric vector of cumulative probabilities F(x).
#'
#' @details
#' The cumulative distribution function is defined as:
#' \deqn{F(x) = 1 - (1 + x^c)^{-k}, \quad x > 0}
#'
#'
#'
#' @examples
#' pburr(1, c = 2, k = 3)
#'
#' @export
pburr <- function(x, c, k){
  1 - (1 + x^c)^(-k)
}
