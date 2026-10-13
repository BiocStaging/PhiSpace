test_that("getLabels returns the highest-scoring labels", {
  scores <- matrix(c(1, 5, 2,
                     3, 0, 4), nrow = 2, byrow = TRUE,
                   dimnames = list(NULL, c("A", "B", "C")))
  expect_identical(getLabels(scores), c("B", "C"))
  expect_identical(getLabels(scores, labPerSample = 2),
                   rbind(c("B", "C"), c("C", "A")))
  expect_error(getLabels(scores, labPerSample = 0), "positive integer")
})

test_that("PhiSpace no longer masks methods::getClass", {
  expect_false("getClass" %in% getNamespaceExports("PhiSpace"))
})
