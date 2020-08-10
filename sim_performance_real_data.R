library(SLOPE)
library(rdatasets)
library(e1071)
library(Matrix)
library(SparseM)

temp_file <- tempfile(fileext = ".txt")

# e2006 test set
download.file(
  "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/QJEUKR/PGPWAK",
  temp_file
)

tmp <- e1071::read.matrix.csr(temp_file, fac = TRUE)

e2006 <- list(x = as(tmp$x, "dgCMatrix"), y = tmp$y)

data <- list(
  e2006 = e2006,
  dorothea = rdatasets::dorothea,
  physician = rdatasets::physician,
  zipcode = rdatasets::zipcode
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
                   zipcode = "multinomial")

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
                   screen = screening)
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

sim_performance_real_data <- out
overwrite <- file.exists("data/sim_performance_real_data.rda")
usethis::use_data(sim_performance_real_data, overwrite = overwrite)
