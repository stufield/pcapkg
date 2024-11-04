# Setup ----------
withr::with_seed(123, {
  pca <- center_scale(log_rfu(sim_adat), center = TRUE, scale = FALSE) |>
    strip_meta() |> prcomp2()
  class <- sample(sim_adat$class_response, 40L)
  all  <- get_analytes(sim_adat)
  set1 <- all[1:3L]
  set2 <- sample(all[10:20L], 3L)
  set3 <- sample(all[25:30L], 3L)
  set4 <- sample(all[30:35L], 3L)
  set5 <- all[37:40L]
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

test_that("`plot_rotation()` produces the expected plot when 'features' are provided", {
  expect_snapshot_plot(plot_rotation(pca, set2 = set1),
                       "plot_rotation_set1")
})

test_that("`plot_rotation()` modifies point size when 'features' vectors are provided", {
  expect_snapshot_plot(plot_rotation(pca,
                                     set1 = set1,
                                     set2 = set2,
                                     set3 = set3,
                                     set4 = set4,
                                     set5 = set5),
                       "plot_rotation_set2")
})
