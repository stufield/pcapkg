# Setup ----------
tr <- libml::create_train(simdata, group.var = class_response) |>
  ungroup() |>
  log_rfu() |>
  center_scale(center = TRUE, scale = FALSE)
pca <- ungroup(tr) |>
  feature_matrix() |>
  prcomp2()
set1 <- withr::with_seed(123, sample(get_analytes(simdata), 5L))
set2 <- withr::with_seed(678, sample(get_analytes(simdata), 5L))

# Testing ----------
test_that("`plotPCApairs()` works as expected when colors are specified", {
  expect_snapshot_plot(
    plotPCApairs(pca, rot.col = "green"),
    "plotPCApairs_colors", gg = FALSE
  )
})

test_that("`plotPCApairs(set1=)` marks selected features", {
  expect_snapshot_plot(
    plotPCApairs(pca, set1 = set1, set2 = set2),
    "plotPCApairs_feat_sets", gg = FALSE
  )
})
