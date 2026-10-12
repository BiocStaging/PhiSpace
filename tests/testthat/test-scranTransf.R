test_that("scranTransf replaces non-positive size factors by the smallest positive one", {
  sce <- scuttle::mockSCE(ncells = 6, ngenes = 20)
  # The old code read position 3 of the sorted vector from the unsorted one
  # and used 3 instead of 0.5
  sf <- c(2, -1, 3, 0.5, 0, 4)
  local_mocked_bindings(
    computeSumFactors = function(x, ...) {
      x$sizeFactor <- sf
      x
    },
    .package = "scran"
  )
  out <- scranTransf(sce, clusts = rep(1:2, 3))
  # logNormCounts() centres the size factors to mean 1
  expected <- c(2, 0.5, 3, 0.5, 0.5, 4)
  expect_equal(out$sizeFactor, expected / mean(expected))
  expect_true("logcounts" %in% SummarizedExperiment::assayNames(out))
})
