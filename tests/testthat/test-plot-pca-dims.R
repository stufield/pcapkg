# Setup ----------
pca <- center_scale(log_rfu(sim_adat), center = TRUE, scale = FALSE) |>
  feature_matrix() |>
  prcomp2()
scores <- withr::with_seed(101, rnorm(length(get_analytes(sim_adat))))
p_classes <- sim_adat$class_response
r_classes <- withr::with_seed(123, sample(sim_adat$class_response,
                                          nrow(pca$rotation),
                                          replace = TRUE))

# Testing ----------
test_that("`plot_pca_dims()` works as expected with default values", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, classes = NULL),
    "plot_pca_dims_defaults"
  )
})

test_that("`plot_pca_dims()` colors points when 'col' is specified", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, col = "red", classes = NULL),
    "plot_pca_dims_color"
  )
})

test_that("`plot_pca_dims()` controls point appearance via 'pt_cex' and 'pt.bg'", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "x", dims = 1:2L, classes = p_classes, pt_cex = 10),
    "plot_pca_dims_pointSize"
  )
})

test_that("`plot_pca_dims(add_ellipse = TRUE)` adds an ellipse to the plot", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, col = "red",
                  classes = NULL, add_ellipse = TRUE),
    "plot_pca_dims_ellipse"
  )
})

test_that("`plot_pca_dims(add_ellipse = TRUE)` adds an ellipse for each class", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, classes = r_classes,
                  add_ellipse = TRUE),
    "plot_pca_dims_classEllipse"
  )
})

test_that("plot_pca_dims() colors points when a vector is provided to 'col'", {
  col_vec <- withr::with_seed(345, unlist(sample(col_palette,
                                                 nrow(pca$rotation),
                                                 replace = TRUE)))
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, col = col_vec, classes = NULL),
    "plot_pca_dims_colorVector"
  )
})

test_that("plot_pca_dims() colors by class when 'classes' is provided", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "x", dims = 1:2L, classes = p_classes),
    "plot_pca_dims_classes"
  )
})

test_that("plot_pca_dims() colors the points by class for a rotation plot", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, classes = r_classes),
    "plot_pca_dims_rotationClasses"
  )
})

test_that("plot_pca_dims() colors the points by score when scores are provided", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "r", dims = 1:2L, classes = NULL, scores = scores),
    "plot_pca_dims_scores"
  )
})

test_that("plot_pca_dims() throws an error when not enough scores are provided", {
  expect_error(
    plot_pca_dims(pca, value = "x", dims = 1:2L, classes = NULL, scores = scores),
    "The length of `scores = ` must be the same length as the plot type*"
  )
})

test_that("plot_pca_dims() throws an error when not enough classes are provided", {
  expect_error(
    plot_pca_dims(pca, value = "r", dims = 1:2L, classes = r_classes[1:4]),
    "The length of `classes = ` must be the same length as the plot type"
  )
})

test_that("plot_pca_dims() produces a legend for 'rotation' plots w/ classes", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "rotation", dims = 1:2L, classes = r_classes),
    "plot_pca_dims_rotationLegend"
  )
})

test_that("plot_pca_dims() axes have customizable labels", {
  expect_snapshot_plot(
    plot_pca_dims(pca, value = "rotation", dims = 1:2L, classes = NULL,
                  main = "Title Test",
                  xlab = "x-axis test", ylab = "y-axis test"),
    "plot_pca_dims_axisLabels"
  )
})
