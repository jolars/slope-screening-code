source("datasets.R")

library(SLOPE)

dorothea <- get_dataset("dorothea")

data <- list(
  dorothea = dorothea
)

out <- data.frame()

iter <- 0

for (i in 1:length(data)) {

  dataset <- names(data)[i]
  x <- data[[i]]$x
  y <- data[[i]]$y

  family <- switch(dataset,
                   e2006 = "gaussian",
                   dorothea = "binomial",
                   physician = "poisson",
                   poker = "multinomial")

  n <- nrow(x)
  p <- ncol(x)

  n_lambda <- switch(family, multinomial = p*(length(unique(y)) - 1), p)

  for (screening in c(TRUE, FALSE)) {
    iter <- iter + 1

    cat("iter:", iter, "/", length(data)*2,
        "\tdata:", dataset, family,
        "\tscreening:", screening, "\n")

    time <- system.time({
      fit <- SLOPE(x,
                   y,
                   family = family,
                   lambda = "bh",
                   q = 0.1*min(1, n/p),
                   screen = screening,
                   verbosity = 1)
    })

    tmp <- data.frame(dataset = dataset,
                      family = family,
                      screening = screening,
                      n = n,
                      p = p,
                      time = time[3])
    out <- rbind(out, tmp)
  }
}

rownames(out) <- NULL

saveRDS(out, "results/sim_performance_real_data_binomial.rds")
