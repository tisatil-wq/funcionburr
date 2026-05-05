#' @title Random Generation from Burr Type XII Distribution
#'
#' @description
#' Generates random values from the Burr Type XII distribution using
#' the inverse transform method.
#'
#' @param n Integer. Number of observations to generate.
#' @param c Positive shape parameter (c > 0).
#' @param k Positive shape parameter (k > 0).
#'
#' @return Numeric vector of random values distributed as Burr(c, k).
#'
#' @details
#' Random values are generated using the inverse transform method:
#' \deqn{X = \left((1 - U)^{-1/k} - 1\right)^{1/c}, \quad U \sim U(0,1)}
#'
#'
#'
#' @examples
#' x <- rburr(1000, c = 2, k = 3)
#' hist(x, probability = TRUE)
#' curve(dburr(x, 2, 3), add = TRUE, col = "red")
#'
#' @importFrom stats runif
#'
#' @export
rburr <- function(n, c, k){
  U <- runif(n)
  ((1 - U)^(-1/k) - 1)^(1/c)
}
