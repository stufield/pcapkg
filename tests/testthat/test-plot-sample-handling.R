# Setup ----------
data <- log10(sim_adat[c(1:10L, 91:100L), ])
samps <- data$class_response
# Create fake matching SH analytes
new_seqs <- c(
  "seq.4124.24",   # cell lysis
  "seq.2811.27",   # platelet activation
  "seq.2242.51"    # complement activation
)
names(data)[31:33L] <- new_seqs   # assign into 'noise' apts
pca <- center_scale(data, center = TRUE, scale = FALSE) |>
  strip_meta() |>
  prcomp2()

# Testing ----------

test_that("plot_sample_handling() produces the expected plasma plot with default args", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix.type = "p"),
    "plot_sample_handling_plasma", gg = FALSE
  )
})

test_that("plot_sample_handling() produces the expected serum plot with default args", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix.type = "s"),
    "plot_sample_handling_serum", gg = FALSE
  )
})

test_that("plot_sample_handling(dims=) can be modified to change PCA dims", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, dims = 3:4L, matrix.type = "p"),
    "plot_sample_handling_dims", gg = FALSE
  )
})

test_that("plot_sample_handling(add.apts = ) can annotate additional apts", {
  apts <- withr::with_seed(765, sample(get_analytes(sim_adat), 5))
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix.type = "p",
                       add.apts = apts),
    "plot_sample_handling_addApts", gg = FALSE
  )
})

test_that("plot_sample_handling(legend.pos = ) moves the plot legends", {
  expect_snapshot_plot(
    plot_sample_handling(pca, samples = samps, matrix.type = "p",
                       legend.pos = c(0, 0)),
    "plot_sample_handling_legendPos", gg = FALSE
  )
})

test_that("plot_sample_handling() output can be saved to a file", {
  file <- tempfile("test_write", fileext = ".jpeg")
  plot_sample_handling(pca, samples = samps, matrix.type = "p",
                     legend.pos = c(1, 1), filename = file)
  expect_true(file.exists(file))
  # Bug in ggsave() that produces this file in a testing env
  unlink("Rplots.pdf")
})
