library(SLOPE)
library(progress)

source("utils.R")

p <- 5000
n <- 200
q <- c(0.01, 0.001, 0.0001)
rho <- (0:8)/10
alg <- c("strong", "previous")
it <- 50

out <- data.frame()

set.seed(1055)

pb <- progress_bar$new(total = length(rho)*it*length(alg)*length(q),
                       format = "[:bar] :percent eta: :eta")

for (i in seq_along(rho)) {
  for (ii in seq_along(q)) {
    for (l in 1:it) {

      x <- generate_correlated_design(n, p, rho[i])

      beta <- double(p)
      k <- 50
      beta[1:k] <- rnorm(k)
      y <- x %*% beta + rnorm(n)

      for (j in seq_along(alg)) {
        pb$tick()

        time <- system.time({
          fit <- SLOPE(x,
                       y,
                       screen = TRUE,
                       screen_alg = alg[j],
                       lambda = "bh",
                       verbosity = 0,
                       q = q[ii])
        })

        tmp <- data.frame(alg = alg[j],
                          time = time[3],
                          rho = rho[i],
                          q = q[ii])

        out <- rbind(out, tmp)
      }
    }
  }
}

rownames(out) <- NULL

saveRDS(out, "results/sim_performance_alg.rds")

