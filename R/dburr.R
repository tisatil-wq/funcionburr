#' @title Burr Type XII Density Function
#'
#' @description
#' Computes the probability density function (PDF) of the Burr Type XII distribution.
#'
#' @param x Numeric vector of values (x > 0).
#' @param c Positive shape parameter (c > 0).
#' @param k Positive shape parameter (k > 0).
#'
#' @return Numeric vector of density values f(x).
#'
#' @details
#' The density function is defined as:
#' \deqn{f(x) = \frac{c k x^{c-1}}{(1 + x^c)^{k+1}}, \quad x > 0}
#'
#' @examples
#' dburr(1, c = 2, k = 3)
#'
#' @export
dburr <- function(x, c, k){
  c * k * x^(c - 1) / (1 + x^c)^(k + 1)
}

