# Setup ----------
tr <- libml::create_train(sample.adat, group.var = SampleGroup)
sh <- getSeqId(unique(unlist(sapply(smvs, names))), trim.version = TRUE) # nolint
sh <- unique(c(sh, getSeqId(unique(unlist(sh_list)), trim.version = TRUE)))
sh <- matchSeqIds(sh, names(tr))
tr <- dplyr::select(tr, dplyr::all_of(sh), SampleGroup)

# Testing ----------
test_that("`pca_purify()` returns feat and mu scores", {
  results <- pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 2, verbose = FALSE)
  expect_named(results, c("feats", "mu_scores"))
  expect_type(results, "list")
})

test_that("`pca_purify()` works as expected with default args", {
  expect_snapshot_plot(
    testthat::capture_output(
      pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 6, verbose = FALSE)
    ),
    width = 1000, height = 450, "pcaPurify_defaults", gg = FALSE
  )
})

test_that("`pca_purify()1 works as expected with less iters", {
  expect_snapshot_plot(
    testthat::capture_output(
      pcaPurify(tr, features = sh_list$plasmaLysis, max.iter = 2, verbose = FALSE)
    ),
    width = 300, height = 450, "pcaPurify_lowIter", gg = FALSE
  )
})
