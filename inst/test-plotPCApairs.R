# Setup ----------
tr <- SomaClassify::convert2TrainingData(sim_test_data,
                                         group.var = class_response) |>
  ungroup() |>
  log10() |>
  centerScaleData(center = TRUE, scale = FALSE)
pca <- ungroup(tr) |>
  stripMeta() |>
  prcomp2()
apts <- withr::with_seed(123, sample(getAnalytes(sim_test_data), 5L))
apts2 <- withr::with_seed(678, sample(getAnalytes(sim_test_data), 5L))

# Testing ----------
test_that("plotPCApairs() works as expected when colors are specified", {
  expect_snapshot_plot(
    plotPCApairs(pca, apt.col = "green"),
    "plotPCApairs_colors", gg = FALSE
  )
})

test_that("plotPCApairs(aptamers = ) marks selected aptamers", {
  expect_snapshot_plot(
    plotPCApairs(pca, aptamers = apts, aptamers2 = apts2),
    "plotPCApairs_aptamers", gg = FALSE
  )
})
