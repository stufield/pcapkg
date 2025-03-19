
# Setup ----
pca <- center_scale(log10_ft(simdata), center = TRUE, scale = FALSE) |>
  feature_matrix() |>
  prcomp2()


# Testing ----
test_that("`plot_projection()` produces the expected plot when default args are used", {
  expect_snapshot_plot(plot_projection(pca),
                       "plot_projection_defaults")
})

test_that("`plot_projection()` produces the expected plot when 'col' is provided", {
  expect_snapshot_plot(plot_projection(pca, col = "green"),
                       "plot_projection_col")
})

test_that("`plot_projection()` produces the expected plot when 'classes' are provided", {
  expect_snapshot_plot(plot_projection(pca, classes = simdata$class_response),
                       "plot_projection_classes")
})

test_that("`plot_projection()` produces an error when too few 'classes' are provided", {
  expect_error(plot_projection(pca, classes = simdata$class_response[1:5]),
               "Inappropriate length of the `classes =` argument*")
})
