pca <- center_scale(log10(sim_test_data), center = TRUE, scale = FALSE) |>
  strip_meta() |>
  prcomp2()

test_that("plotProjection() produces the expected plot when default args are used", {
  expect_snapshot_plot(plotProjection(pca),
                       "plotProjection_defaults")
})

test_that("plotProjection() produces the expected plot when 'col' is provided", {
  expect_snapshot_plot(plotProjection(pca, col = "green"),
                       "plotProjection_col")
})

test_that("plotProjection() produces the expected plot when 'classes' are provided", {
  expect_snapshot_plot(plotProjection(pca, classes = sim_test_data$class_response),
                       "plotProjection_classes")
})

test_that("plotProjection() produces an error when too few 'classes' are provided", {
  expect_error(plotProjection(pca, classes = sim_test_data$class_response[1:5]),
               "Inappropriate length of the `classes =` argument*")
})
