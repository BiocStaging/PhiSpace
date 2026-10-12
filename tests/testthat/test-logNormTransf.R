test_that("logNormTransf divides by centred library sizes and takes log2(x + 1)", {
  sce <- scuttle::mockSCE(ncells = 30, ngenes = 50)
  x <- SummarizedExperiment::assay(sce, "counts")
  sf <- colSums(x) / mean(colSums(x))
  out <- logNormTransf(sce)
  expect_equal(SummarizedExperiment::assay(out, "logcounts"),
               log2(sweep(x, 2, sf, "/") + 1))
  expect_equal(out$sizeFactor, sf)
})

test_that("logNormTransf keeps sparse counts sparse and centres given size factors", {
  sce <- scuttle::mockSCE(ncells = 30, ngenes = 50)
  dense <- SummarizedExperiment::assay(sce, "counts")
  SummarizedExperiment::assay(sce, "counts") <- methods::as(dense, "CsparseMatrix")
  sf <- seq(0.5, 2, length.out = 30)
  out <- logNormTransf(sce, sizeFactors = sf, targetAssay = "lognorm")
  res <- SummarizedExperiment::assay(out, "lognorm")
  expect_s4_class(res, "dgCMatrix")
  expect_equal(as.matrix(res), log2(sweep(dense, 2, sf / mean(sf), "/") + 1))
  expect_equal(out$sizeFactor, sf / mean(sf))
})

test_that("logNormTransf rejects non-positive size factors", {
  sce <- scuttle::mockSCE(ncells = 5, ngenes = 20)
  expect_error(logNormTransf(sce, sizeFactors = c(1, 0, 1, 1, 1)), "positive")
  expect_error(logNormTransf(sce, sizeFactors = 1:3), "one value per cell")
})
