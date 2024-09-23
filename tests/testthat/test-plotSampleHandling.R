# Setup ----------
data <- log10(sim_test_data[c(1:10L, 91:100L), ])
samps <- data$class_response
# Create fake matching SH analytes
new_seqs <- c(
  "seq.4124.24",   # cell lysis
  "seq.2811.27",   # platelet activation
  "seq.2242.51"    # complement activation
)
names(data)[31:33L] <- new_seqs   # assign into 'noise' apts
pca <- centerScaleData(data, center = TRUE, scale = FALSE) |>
  stripMeta() |>
  prcomp2()

# Testing ----------

test_that("plotSampleHandling() produces the expected plasma plot with default args", {
  expect_snapshot_plot(
    plotSampleHandling(pca, samples = samps, matrix.type = "p"),
    "plotSampleHandling_plasma", gg = FALSE
  )
})

test_that("plotSampleHandling() produces the expected serum plot with default args", {
  expect_snapshot_plot(
    plotSampleHandling(pca, samples = samps, matrix.type = "s"),
    "plotSampleHandling_serum", gg = FALSE
  )
})

test_that("plotSampleHandling(dims=) can be modified to change PCA dims", {
  expect_snapshot_plot(
    plotSampleHandling(pca, samples = samps, dims = 3:4L, matrix.type = "p"),
    "plotSampleHandling_dims", gg = FALSE
  )
})

test_that("plotSampleHandling(add.apts = ) can annotate additional apts", {
  apts <- withr::with_seed(765, sample(getAnalytes(sim_test_data), 5))
  expect_snapshot_plot(
    plotSampleHandling(pca, samples = samps, matrix.type = "p",
                       add.apts = apts),
    "plotSampleHandling_addApts", gg = FALSE
  )
})

test_that("plotSampleHandling(legend.pos = ) moves the plot legends", {
  expect_snapshot_plot(
    plotSampleHandling(pca, samples = samps, matrix.type = "p",
                       legend.pos = c(0, 0)),
    "plotSampleHandling_legendPos", gg = FALSE
  )
})

test_that("plotSampleHandling() output can be saved to a file", {
  file <- tempfile("test_write", fileext = ".jpeg")
  plotSampleHandling(pca, samples = samps, matrix.type = "p",
                     legend.pos = c(1, 1), filename = file)
  expect_true(file.exists(file))
  # Bug in ggsave() that produces this file in a testing env
  unlink("Rplots.pdf")
})
