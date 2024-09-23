# Setup ----------
tr <- SomaClassify::convert2TrainingData(sample.adat, "SampleGroup")
sh <- getSeqId(unique(unlist(sapply(smvs, names))), trim.version = TRUE) # nolint
sh <- unique(c(sh, getSeqId(unique(unlist(sh_list)), trim.version = TRUE)))
sh <- matchSeqIds(sh, names(tr))
tr <- dplyr::select(tr, all_of(sh), Response)

# Testing ----------
test_that("pcaPurify() returns aptamers and mu scores", {
  results <- pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 2, verbose = FALSE)
  expect_named(results, c("apts", "mu_scores"))
  expect_type(results, "list")
})

test_that("pcaPurify() works as expected with default args", {
  expect_snapshot_plot(
    testthat::capture_output(
      pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 6, verbose = FALSE)
    ),
    width = 1000, height = 450, "pcaPurify_defaults", gg = FALSE
  )
})

test_that("pcaPurify() works as expected with less iters", {
  expect_snapshot_plot(
    testthat::capture_output(
      pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 2, verbose = FALSE)
    ),
    width = 300, height = 450, "pcaPurify_lowIter", gg = FALSE
  )
})
