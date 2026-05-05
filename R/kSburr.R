#' @title Kolmogorov-Smirnov Test for Burr Distribution
#'
#' @description
#' Performs a manual Kolmogorov-Smirnov (KS) goodness-of-fit test
#' to evaluate whether a dataset follows a Burr Type XII distribution.
#'
#' @param datos Numeric vector of observations.
#' @param c Positive shape parameter (c > 0).
#' @param k Positive shape parameter (k > 0).
#' @param alpha Significance level (default = 0.05).
#' @param plot Logical. If TRUE, displays the empirical vs theoretical CDF plot.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{D}: KS statistic
#'   \item \code{D_plus}: Positive deviation
#'   \item \code{D_minus}: Negative deviation
#'   \item \code{D_critical}: Critical value for the test
#' }
#'
#' @details
#' The test compares the empirical cumulative distribution function (ECDF)
#' of the data with the theoretical CDF of the Burr Type XII distribution:
#' \deqn{F(x) = 1 - (1 + x^c)^{-k}}
#'
#' The KS statistic is computed as:
#' \deqn{D = \max(D^+, D^-)}
#'
#' where:
#' \deqn{D^+ = \max(F_{emp}(x) - F_{teo}(x))}
#' \deqn{D^- = \max(F_{teo}(x) - F_{emp}(x))}
#'
#' The critical value is approximated by:
#' \deqn{D_{crit} = \frac{1.36}{\sqrt{n}}}
#'
#' @examples
#' # Generate data
#' datos <- rburr(1000, c = 2, k = 3)
#'
#' # Apply KS test
#' result <- kSburr(datos, c = 2, k = 3)
#'
#' result$D
#'
#' @export
kSburr <- function(datos, c, k, alpha = 0.05, plot = TRUE){
  x <- sort(datos)
  n <- length(x)

  F_teo <- 1 - (1 + x^c)^(-k)
  F_emp_sup <- (1:n) / n
  F_emp_inf <- (0:(n-1)) / n

  D_plus  <- max(F_emp_sup - F_teo)
  D_minus <- max(F_teo - F_emp_inf)

  D <- max(D_plus, D_minus)

  D_critico <- 1.36 / sqrt(n)


  if(plot){
    plot(x, F_emp_sup, type="s", col="blue",
         main="KS Test: Empirical vs Theoretical",
         xlab="x", ylab="F(x)")

    lines(x, F_teo, col="red", lwd=2)

    legend("bottomright",
           legend=c("Empirical","Theoretical"),
           col=c("blue","red"),
           lty=1)
  }

  return(list(
    D = D,
    D_plus = D_plus,
    D_minus = D_minus,
    D_critical = D_critico

  ))
}
