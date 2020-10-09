url <- "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/multiclass/news20.bz2"

temp_file <- tempfile(fileext = ".bz2")

download.file(
  "https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/multiclass/news20.bz2",
  temp_file
)

tmp <- e1071::read.matrix.csr(bzfile(temp_file), fac = TRUE)

out <- list(x = as(tmp$x, "dgCMatrix"), y = tmp$y)

saveRDS(out, "data/news20.rds")

