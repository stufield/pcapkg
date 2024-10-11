# Setup ----------
pca <- center_scale(log10(sim_test_data), center = TRUE, scale = FALSE) |>
  strip_meta() |>
  prcomp2()
scores <- withr::with_seed(101, rnorm(getAnalytes(sim_test_data, n = TRUE)))
p_classes <- sim_test_data$class_response
r_classes <- withr::with_seed(123, sample(sim_test_data$class_response,
                                          nrow(pca$rotation),
                                          replace = TRUE))

# Testing ----------
test_that("plotPCAdims() works as expected with default values", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, classes = NULL),
    "plotPCAdims_defaults"
  )
})

test_that("plotPCAdims() colors points when 'col' is specified", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, col = "red", classes = NULL),
    "plotPCAdims_color"
  )
})

test_that("plotPCAdims() controls point appearance via 'pt.cex' and 'pt.bg'", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "x", dims = 1:2L, classes = p_classes,
                pt.cex = 10),
    "plotPCAdims_pointSize"
  )
})

test_that("plotPCAdims(add.ellipse = TRUE) adds an ellipse to the plot", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, col = "red",
                classes = NULL, add.ellipse = TRUE),
    "plotPCAdims_ellipse"
  )
})

test_that("plotPCAdims(add.ellipse = TRUE) adds an ellipse for each class", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, classes = r_classes,
                add.ellipse = TRUE),
    "plotPCAdims_classEllipse"
  )
})

test_that("plotPCAdims() colors points when a vector is provided to 'col'", {
  col_vec <- withr::with_seed(345, unlist(sample(SomaPlotr::soma_colors,
                                                 nrow(pca$rotation),
                                                 replace = TRUE)))
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, col = col_vec, classes = NULL),
    "plotPCAdims_colorVector"
  )
})

test_that("plotPCAdims() colors by class when 'classes' is provided", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "x", dims = 1:2L, classes = p_classes),
    "plotPCAdims_classes"
  )
})

test_that("plotPCAdims() colors the points by class for a rotation plot", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, classes = r_classes),
    "plotPCAdims_rotationClasses"
  )
})

test_that("plotPCAdims() colors the points by score when scores are provided", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "r", dims = 1:2L, classes = NULL, scores = scores),
    "plotPCAdims_scores"
  )
})

test_that("plotPCAdims() throws an error when not enough scores are provided", {
  expect_error(
    plotPCAdims(pca, value = "x", dims = 1:2L, classes = NULL, scores = scores),
    "The length of `scores = ` must be the same length as the plot type*"
  )
})

test_that("plotPCAdims() throws an error when not enough classes are provided", {
  expect_error(
    plotPCAdims(pca, value = "r", dims = 1:2L, classes = r_classes[1:4]),
    "The length of `classes = ` must be the same length as the plot type"
  )
})

test_that("plotPCAdims() produces a legend for 'rotation' plots w/ classes", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "rotation", dims = 1:2L, classes = r_classes),
    "plotPCAdims_rotationLegend"
  )
})

test_that("plotPCAdims() axes have customizable labels", {
  expect_snapshot_plot(
    plotPCAdims(pca, value = "rotation", dims = 1:2L, classes = NULL,
                main = "Title Test",
                xlab = "x-axis test", ylab = "y-axis test"),
    "plotPCAdims_axisLabels"
  )
})
