#' Screen features
#'
#' @param x features
#' @param y response
#' @param method type of screening
#' @param lambda regularization sequence of current fit
#' @param lambda_prev regularization sequence of previous fit
#' @param beta_prev coefficients from previous fit
#' @param intercept_prev intercept from previous fit
#' @param gradient_prev gradient from previous fit
#' @param pseudo_gradient_prev pseudo-gradient from previous fit
#' @param x_colnorms x column norms
#'
#' @return A logical vector indicating whether to drop a feature or not
#' @export
activeSet <- function(x,
                      y,
                      lambda,
                      lambda_prev,
                      beta_prev,
                      intercept_prev,
                      gradient_prev,
                      pseudo_gradient_prev,
                      x_colnorms,
                      method = c("none", "strong"),
                      hessian_prev = NULL) {
  method <- match.arg(method)
  p <- NCOL(x)

  switch(
    method,

    none = {
      rep(TRUE, p)
    },

    strong = {
      out <- logical(p)

      abs_grad <- abs(gradient_prev)
      ord <- order(abs_grad, decreasing = TRUE)
      abs_grad_sorted <- abs_grad[ord]

      i <- 1
      k <- 0

      s <- 0

      # tmp <- abs_grad_sorted + lambda_prev - 2*lambda

      while (i + k <= p) {
        s <- s + abs_grad_sorted[k+i] + lambda_prev[k+i] - 2*lambda[k+i]

        if (s >= 0) {
          k <- k+i
          i <- 1
          s <- 0
        } else {
          i <- i + 1
        }
      }

      out[0:k] <- TRUE
      out[ord] <- out
      out
    }
  )
}
