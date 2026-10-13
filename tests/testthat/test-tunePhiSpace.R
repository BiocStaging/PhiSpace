tuneRef <- function(){
  withr::local_seed(1)
  logNormTransf(scuttle::mockSCE(ncells = 120, ngenes = 200))
}

test_that("tunePhiSpace chooses the ncomp with the lowest loss when it cannot ask", {
  skip_if_not_installed("scuttle")
  ref <- tuneRef()
  expect_message(
    tuned <- tunePhiSpace(ref, phenotypes = "Cell_Cycle", ncompGrid = 1:4,
                          nfeatV = c(20, 50), Kfolds = 3, nfeat = 50),
    "lowest cross-validation loss"
  )
  errs <- tuned$tuneNcompErrs
  expect_identical(tuned$ncomp, errs$ncomp[which.min(errs$FrobRatio)])
  expect_identical(tuned$nfeat, 50)
  expect_identical(nrow(tuned$tuneNfeatErrs), 2L)
})

test_that("tunePhiSpace uses a given ncomp after cross-validation", {
  skip_if_not_installed("scuttle")
  ref <- tuneRef()
  tuned <- tunePhiSpace(ref, phenotypes = "Cell_Cycle", ncompGrid = 1:4,
                        tune_nfeat = FALSE, Kfolds = 3, ncomp = 3)
  expect_identical(tuned$ncomp, 3)
  expect_identical(nrow(tuned$tuneNcompErrs), 4L)
})

test_that("tunePhiSpace needs nfeat in a non-interactive session", {
  skip_if_not_installed("scuttle")
  ref <- tuneRef()
  expect_error(
    tunePhiSpace(ref, phenotypes = "Cell_Cycle", ncompGrid = 1:4,
                 nfeatV = c(20, 50), Kfolds = 3),
    "Give nfeat"
  )
  expect_error(
    suppressMessages(
      tunePhiSpace(ref, phenotypes = "Cell_Cycle", ncompGrid = 1:4,
                   nfeatV = c(20, 50), Kfolds = 3, nfeat = 0)
    ),
    "positive whole number"
  )
})
