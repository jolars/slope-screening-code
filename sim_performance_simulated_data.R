library(SLOPE)

p <- 20000
m <- 3
n <- 200
k <- 20
q <- 0.001

sigma <- 20

n_rep <- 20

out <- data.frame()

set.seed(412)

for (i in 1:n_rep) {
  for (family in c("gaussian", "binomial", "poisson", "multinomial")) {
    for (rho in c(0, 0.5, 0.99, 0.999)) {
      x <- matrix(rnorm(n*p), n)

      for (j in 2:p) {
        x[, j] <- rho*x[, j-1] + sqrt(1 - rho^2)*rnorm(n)
      }

      n_lambda <- p

      if (family == "gaussian") {
        beta <- double(p)
        beta[sample(20)] <- 20:1

        y <- x %*% beta + sigma*rnorm(n)

      } else if (family == "binomial") {
        beta <- double(p)
        beta[sample(20)] <- 20:1

        y <- ifelse(x %*% beta + sigma*rnorm(n) > 0, 1, 0)

      } else if (family == "poisson") {

        beta <- double(p)
        beta[sample(20)] <- (20:1)/40

        y <- rpois(n, exp(x %*% beta))

      } else if (family == "multinomial") {

        beta <- matrix(0, p, m)
        ind <- sample(20)

        for (j in 1:20) {
          beta[j, sample(3, 1)] <- (1:20)[ind[j]]
        }

        lin_pred_exp <- exp(x %*% beta)
        prob <- lin_pred_exp/rowSums(lin_pred_exp)
        y <- apply(prob, 1, function(x) sample(1:m, 1, prob = x))

        n_lambda <- p*(m-1)
      }

      for (screening in c(TRUE, FALSE)) {
        cat("iter:", i, "\t",
            "family:", family, "\t",
            "rho:", rho, "\t",
            "screening:", screening, "\n")

        time <- system.time({
          fit <- SLOPE(x,
                       y,
                       q = q,
                       family = family,
                       lambda = "bh",
                       max_passes = 1e6,
                       screen = screening)
        })

        tmp <- data.frame(family = family,
                          correlation = rho,
                          screening = screening,
                          time = time[3],
                          path_length = length(fit$sigma))
        out <- rbind(out, tmp)
      }
    }
  }
}

rownames(out) <- NULL

saveRDS(out, "results/sim_performance_simulated_data.rds")
