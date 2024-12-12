# Setup ----------
data <- log_rfu(simdata[c(1:10L, 91:100L), ])
samps <- data$class_response
# Create fake matching SH analytes
new_seqs <- c(
  "seq.4124.24",   # cell lysis
  "seq.2811.27",   # platelet activation
  "seq.2242.51"    # complement activation
)
names(data)[31:33L] <- new_seqs   # assign into 'noise' features
pca <- center_scale(data, center = TRUE, scale = FALSE) |>
  feature_matrix() |>
  prcomp2()


# Testing ----------
test_that("`plot_sample_handling()` produces expected plot with defaults", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix_type = "p"),
    "plot_sample_handling_plasma"
  )
})

test_that("`plot_sample_handling()` produces expected serum plot with defaults", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix_type = "s"),
    "plot_sample_handling_serum"
  )
})

test_that("`plot_sample_handling(dims=)` can be modified to change PCA dims", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, dims = 3:4L, matrix_type = "p"),
    "plot_sample_handling_dims"
  )
})

test_that("`plot_sample_handling(extra.feat=)` can annotate additional features", {
  y <- withr::with_seed(765, sample(get_analytes(simdata), 5))
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix_type = "p", extra_feat = y),
    "plot_sample_handling_addApts"
  )
})

test_that("`plot_sample_handling(legend_pos=)` moves the plot legends", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix_type = "p",
                         legend_pos = c(0, 0)),
    "plot_sample_handling_legendPos"
  )
})

test_that("`plot_sample_handling()` output can be saved to a file", {
  file <- tempfile("test_write", fileext = ".png")
  plot_sample_handling(pca, samples = samps, matrix_type = "p",
                       legend_pos = c(1, 1), file = file)
  expect_true(file.exists(file))
  # Bug in ggsave() produces this file in testing -> remove
  unlink("Rplots.pdf")
})
