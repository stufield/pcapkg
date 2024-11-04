
data <- sim_adat |>
  libml::create_train(group.var = class_response)

x <- pca(log_rfu(log10(data)))

test_that("`pca_auc()` returns correct AUC", {
  withr::local_options(list(digits = 6L))
  expect_snapshot(pca_auc(x, 1L))
  expect_snapshot(pca_auc(x, 2L))
  expect_snapshot(pca_auc(x, 5L))
  expect_snapshot(pca_auc(x, 10L))
  expect_snapshot(pca_auc(x, 15L))
})

test_that("`pca_auc()` errors out when asked for a dim > available", {
  expect_error(
    pca_auc(x, 41),
    ".col %in% names(pca.data$projection) is not TRUE",
    fixed = TRUE
  )
})
