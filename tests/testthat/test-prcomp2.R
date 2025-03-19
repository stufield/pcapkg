withr::local_options(list(digits = 6L))

# Setup ----
# create fake names
fake_name <- function(n = 1) {
  replicate(n, {
    paste0("ft_", paste0(sample(1000:9999, 1), "_", sample(1:99, 1)))
  })
}

# create fake rownames
fake_rn <- function(n = 1) {
  x <- round(runif(n, 10000000, 99999999))
  paste0(x, "_", sample(1:9, length(x), replace = TRUE))
}

# create random data
withr::with_seed(1, {
  n <- 20
  p <- 100
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(x) <- fake_name(p)
  rownames(x) <- fake_rn(n)
  pr <- prcomp2(x)
})


# Testing ----
test_that("`prcomp2()` returns the correct object", {
  expect_s3_class(pr, "prcomp")
  expect_length(pr, 5L)
  expect_named(
    pr,
    c("sdev", "rotation", "x", "basis", "single.vals")
  )
})

test_that("`prcomp2()` function returns the correct rotation", {
  expect_true(is.matrix(pr$rotation))
  expect_equal(dim(pr$rotation), c(p, n))
  expect_equal(rownames(pr$rotation), colnames(x))
  expect_equal(colnames(pr$rotation), paste0("PC", 1:n))
  expect_snapshot(abs(colSums(pr$rotation)))   # abs to work Mac/Linux
})

test_that("`prcomp2()` returns the correct projection values", {
  expect_true(is.matrix(pr$x))
  expect_equal(dim(pr$x), c(n, n))
  expect_equal(rownames(pr$x), rownames(x))
  expect_equal(colnames(pr$x), paste0("PC", 1:n))
  expect_snapshot(abs(colSums(pr$x)))   # abs to work Mac/Linux
})

test_that("`prcomp2()` returns the correct sdev", {
  expect_type(pr$sdev, "double")
  expect_length(pr$sdev, n)
  expect_equal(sum(pr$sdev), 46.3075433, tolerance = 1e-04)
})

test_that("`prcomp2()` returns the correct single.vals", {
  expect_type(pr$single.vals, "double")
  expect_length(pr$single.vals, n)
  expect_equal(sum(pr$single.vals), 201.8499, tolerance = 1e-04)
})

test_that("`prcomp2()` returns the correct basis", {
  expect_true(is.matrix(pr$basis))
  expect_equal(dim(pr$basis), c(n, n))
  expect_snapshot(abs(colSums(pr$basis)))   # abs to work Mac/Linux
})
