pca <- center_scale(log_rfu(sim_adat), center = TRUE, scale = FALSE) |>
  strip_meta() |>
  prcomp2()

test_that("`plot_projection()` produces the expected plot when default args are used", {
  expect_snapshot_plot(plot_projection(pca),
                       "plot_projection_defaults")
})

test_that("`plot_projection()` produces the expected plot when 'col' is provided", {
  expect_snapshot_plot(plot_projection(pca, col = "green"),
                       "plot_projection_col")
})

test_that("`plot_projection()` produces the expected plot when 'classes' are provided", {
  expect_snapshot_plot(plot_projection(pca, classes = sim_adat$class_response),
                       "plot_projection_classes")
})

test_that("`plot_projection()` produces an error when too few 'classes' are provided", {
  expect_error(plot_projection(pca, classes = sim_adat$class_response[1:5]),
               "Inappropriate length of the `classes =` argument*")
})
