# Setup ----------
withr::with_seed(123, {
  pca <- centerScaleData(log10(sim_test_data), center = TRUE, scale = FALSE) |>
    stripMeta() |> prcomp2()
  class <- sample(sim_test_data$class_response, 40L)
  all_apts <- getAnalytes(sim_test_data)
  apts1 <- all_apts[1:3]
  apts2 <- sample(all_apts[10:20], 3L)
  apts3 <- sample(all_apts[25:30], 3L)
  apts4 <- sample(all_apts[30:35], 3L)
  apts5 <- all_apts[37:40]
})


# Testing ----------
test_that("plotRotation() produces the expected plot when default args are used", {
  expect_snapshot_plot(plotRotation(pca),
                       "plotRotation_defaults")
})

test_that("plotRotation() produces the expected plot when 'col' is provided", {
  expect_snapshot_plot(plotRotation(pca, col = "blue"),
                       "plotRotation_color")
})

test_that("plotRotation() produces the expected plot when 'classes' are provided", {
  expect_snapshot_plot(plotRotation(pca, classes = class),
                       "plotRotation_classes")
})

test_that("plotRotation() produces the expected plot when 'aptamers' are provided", {
  expect_snapshot_plot(plotRotation(pca, aptamers = apts1),
                       "plotRotation_aptamers")
})

test_that("plotRotation() modifies point size when 'aptamer' vectors are provided", {
  expect_snapshot_plot(plotRotation(pca, aptamers = apts1,
                                    aptamers2 = apts2,
                                    aptamers3 = apts3,
                                    aptamers4 = apts4,
                                    aptamers5 = apts5),
                       "plotRotation_aptamers2")
})
