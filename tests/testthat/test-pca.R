
# Setup ------
pca   <- pca(sim_adat)
scree <- plot_scree(pca)
feats <- add_seq(get_col_meta(sim_adat)$SeqId)
p     <- length(feats)
n     <- nrow(sim_adat)
meta  <- get_meta(sim_adat)


# Testing ----
test_that("the `pca` function returns shape object", {
  expect_s3_class(pca, "pca")
  expect_named(pca, c("projection", "rotation"))
})

test_that("the `pca` function returns the correct projection element", {
  expect_s3_class(pca$projection, "tbl_df")
  expect_equal(dim(pca$projection), # + 1 for new `.id` column during merge
               c(nrow(sim_adat), 1 + min(n, p) + length(meta)))
  expect_true(all(meta %in% names(pca$projection)))
  expect_true(".id" %in% names(pca$projection))
  expect_equal(pca$projection$.id, rownames(sim_adat))
  expect_true(all(paste0("PC", 1:p) %in% names(pca$projection)))
})

test_that("the `pca` function returns the correct rotation element", {
  expect_s3_class(pca$rotation, "tbl_df")
  expect_equal(pca$rotation$Feature, feats)
  expect_equal(dim(pca$rotation), c(p, ncol(get_col_meta(sim_adat)) + p + 1L))
  expect_true(all(paste0("PC", 1:p) %in% names(pca$rotation)))
  tbl <- dplyr::mutate(get_col_meta(sim_adat), Feature = add_seq(SeqId))
  expect_equal(names(tbl),
               names(dplyr::select(pca$rotation, -starts_with("PC"))) # nolint
  )
})

test_that("the `pca` function returns the correct attributes", {
  atts <- attributes(pca)
  expect_type(atts, "list")
  expect_equal(atts$names, c("projection", "rotation"))
  expect_named(atts, c("names", "class", "sdev", "single.vals", "basis"))
  expect_type(atts$sdev, "double")
  expect_length(atts$sdev, min(n, p))
  expect_type(atts$single.vals, "double")
  expect_length(atts$single.vals, min(n, p))
  expect_true(is.matrix(atts$basis))
  expect_equal(dim(atts$basis), c(n, p))
})

test_that("the data from the `plot_scree()` call has the correct values", {
  expect_s3_class(scree$data, "tbl_df")
  expect_equal(dim(scree$data), c(15L, 4L))
  expect_named(scree,
               c("data", "layers", "scales", "guides",
                 "mapping", "theme", "coordinates",
                 "facet", "plot_env", "layout", "labels"))
  expect_named(scree$data, c("Component", "value", "y", "cum_perc"))
  scree_data <- dplyr::select_if(scree$data, is.numeric) |> colSums()
  expect_equal(scree_data, c(Component = 120,
                             value     = 6.65450784823549e+06,
                             y         = 6.50392626461307e-01))
  expect_equal(scree$data$cum_perc,
               c("7%", "14%", "20%", "25%", "30%", "34%", "38%", "42%",
                 "46%", "50%", "53%", "57%", "59%", "62%", "65%"))
})
