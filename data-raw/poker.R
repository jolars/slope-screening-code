temp_file <- tempfile()

# poker hand training data set
download.file(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/poker/poker-hand-training-true.data",
  temp_file
)

tmp <- read.csv(temp_file, header = FALSE)

poker <- list(x = tmp[, 1:10], y = tmp[, 11])

saveRDS(poker, "data/poker.rds")
