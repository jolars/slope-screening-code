get_dataset <- function(x) {
  if (x == "e2006") {

    library(e1071)
    library(Matrix)
    library(SparseM)

    temp_file <- tempfile(fileext = ".txt")

    # e2006-tfidf test set
    download.file(
      "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/QJEUKR/PGPWAK",
      temp_file
    )

    tmp <- e1071::read.matrix.csr(temp_file, fac = TRUE)

    out <- list(x = as(tmp$x, "dgCMatrix"), y = tmp$y)

  } else if (x == "log1p") {

    library(e1071)
    library(Matrix)
    library(SparseM)

    temp_file <- tempfile(fileext = ".bz2")

    # e2006-log1p test set
    download.file(
      "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/regression/log1p.E2006.test.bz2",
      temp_file
    )

    tmp <- e1071::read.matrix.csr(bzfile(temp_file), fac = TRUE)

    out <- list(x = as(tmp$x, "dgCMatrix"), y = tmp$y)

  } else {

    out <- readRDS(file.path("data", paste0(x, ".rds")))

  }

  out
}
