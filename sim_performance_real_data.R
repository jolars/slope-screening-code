source("datasets.R")

library(SLOPE)

e2006 <- get_dataset("e2006")
dorothea <- get_dataset("dorothea")
physician <- get_dataset("physician")
news20 <- get_dataset("news20")

# reduce news20 dataset
set.seed(811)
ind <- sample(nrow(news20$x), 1000)
news20 <- list(x = news20$x[ind, ], y = news20$y[ind])

data <- list(
  e2006 = e2006,
  dorothea = dorothea,
  physician = physician,
  news20 = news20
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
                   news20 = "multinomial")

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

saveRDS(out, "results/sim_performance_real_data.rds")
