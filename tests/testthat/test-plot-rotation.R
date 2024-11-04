# Setup ----------
withr::with_seed(123, {
  pca <- center_scale(log_rfu(sim_adat), center = TRUE, scale = FALSE) |>
    strip_meta() |> prcomp2()
  class <- sample(sim_adat$class_response, 40L)
  all_apts <- get_analytes(sim_adat)
  apts1 <- all_apts[1:3L]
  apts2 <- sample(all_apts[10:20], 3L)
  apts3 <- sample(all_apts[25:30], 3L)
  apts4 <- sample(all_apts[30:35], 3L)
  apts5 <- all_apts[37:40L]
})


# Testing ----------
test_that("`plot_rotation()` produces the expected plot when default args are used", {
  expect_snapshot_plot(plot_rotation(pca),
                       "plot_rotation_defaults")
})

test_that("`plot_rotation()` produces the expected plot when 'col' is provided", {
  expect_snapshot_plot(plot_rotation(pca, col = "blue"),
                       "plot_rotation_color")
})

test_that("`plot_rotation()` produces the expected plot when 'classes' are provided", {
  expect_snapshot_plot(plot_rotation(pca, classes = class),
                       "plot_rotation_classes")
})

test_that("`plot_rotation()` produces the expected plot when 'aptamers' are provided", {
  expect_snapshot_plot(plot_rotation(pca, aptamers = apts1),
                       "plot_rotation_aptamers")
})

test_that("`plot_rotation()` modifies point size when 'aptamer' vectors are provided", {
  expect_snapshot_plot(plot_rotation(pca, 
                                     aptamers = apts1,
                                     aptamers2 = apts2,
                                     aptamers3 = apts3,
                                     aptamers4 = apts4,
                                     aptamers5 = apts5),
                       "plot_rotation_aptamers2")
})
