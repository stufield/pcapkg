
data <- sim_test_data |>
  SomaClassify::convert2TrainingData(group.var = class_response)
x <- pca(log10(data))

test_that("`pcaAUC()` returns correct AUC", {
  withr::local_options(list(digits = 6L))
  expect_snapshot(pcaAUC(x, 1L))
  expect_snapshot(pcaAUC(x, 2L))
  expect_snapshot(pcaAUC(x, 5L))
  expect_snapshot(pcaAUC(x, 10L))
  expect_snapshot(pcaAUC(x, 15L))
})

test_that("`pcaAUC()` errors out when asked for a dim > available", {
  expect_error(
    pcaAUC(x, 41),
    ".col %in% names(pca.data$projection) is not TRUE",
    fixed = TRUE
  )
})
