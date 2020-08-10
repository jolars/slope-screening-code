#' Generate correlated design
#'
#' Generates a design matrix consisting of an n by p matrix of
#' standard normal variables that are pairwise correlated with correlation
#' `rho`
#'
#' @param n rows
#' @param p columns
#' @param rho correlation
#'
#' @return A matrix
generate_correlated_design <- function(n, p, rho) {
  x <- matrix(rnorm(n*p), n)

  if (rho != 0) {
    a <- (1/p)*(sqrt(1 + (p - 1)*rho) - sqrt(1 - rho))
    b <- sqrt(1 - rho) + a

    M <- matrix(a, p, p)
    diag(M) <- b

    x <- x %*% M
  }

  x
}
