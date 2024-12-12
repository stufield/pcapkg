withr::local_options(list(digits = 6L))

# Setup ----
sim <- log_rfu(simdata)
sim$Response <- factor(sim$class_response)
feats <- attributes(sim)$sig_feats$class
x     <- supervised_peel(sim, set1 = feats)

# Testing ----
test_that("`supervised_peel` generates correct output", {
  expect_s3_class(x, "supervised_peel")
  expect_named(x, c("orig", "weighted", "unweighted", "peeled", "peeled_data",
                    "center", "scale", "logged", "feats", "orig_data", "call"))
  nms <- c("sdev", "rotation", "x", "basis", "single.vals")
  expect_named(x$orig, nms)
  expect_equal(rownames(x$orig$x), rownames(sim))
  expect_named(x$weighted, nms)
  expect_named(x$unweighted, nms)
  expect_named(x$peeled, nms)
  expect_equal(x$orig_data, sim)
  expect_true(x$logged)
  expect_true(x$center)
  expect_false(x$scale)
  expect_equal(lengths(x),
               c(orig = 5L, weighted = 5L, unweighted = 5L,
                 peeled = 5L, peeled_data = 41L, center = 1L,
                 scale = 1L, logged = 1L, feats = 5L,
                 orig_data = 56L, call = 3L))
  expect_equal(nrow(x$peeled_data), nrow(sim))
  expect_equal(lapply(x, class),
               list(orig        = "prcomp",
                    weighted    = "prcomp",
                    unweighted  = "prcomp",
                    peeled      = "prcomp",
                    peeled_data = "data.frame",
                    center      = "logical",
                    scale       = "logical",
                    logged      = "logical",
                    feats       = "list",
                    orig_data   = c("soma_adat", "data.frame"),
                    call        = "call"))
})

test_that("`supervised_peel()` generates correct values", {
  # Must be careful here due to LAPACK vs BLAS system library
  # differences in linear algebra; see La_library(); La_version()

  skip_on_os("mac")

  # announce so that skipped tests don't delete the snapshot.md
  announce_snapshot_file("_snaps/supervised_peel.md")
  expect_snapshot(vapply(x$orig, sum, 0.1))
  expect_snapshot(vapply(x$weighted, sum, 0.1))
  expect_snapshot(vapply(x$unweighted, sum, 0.1))

  # Do `$peeled` differently b/c SVD differs in the basis & rotation of PC40
  expect_equal(sum(x$peeled$sdev), 3.3185775046)
  expect_equal(sum(x$peeled$rotation[, -40]), 10.0673582261)  # rm 40th PC
  expect_equal(sum(x$peeled$x), -0.0000000013)
  expect_equal(sum(x$peeled$basis[, -40]), 0.0000000030)      # rm 40th PC
  expect_equal(sum(x$peeled$single.vals), 33.0194292655)

  expect_equal(sum(feature_matrix(x$peeled_data)), 13571.7299406929)
  expect_equal(x$peeled_data$Response, x$orig_data$Response)
})

test_that("`supervised_peel()` generates the expected plot", {
  expect_snapshot_plot(plot(x), "supervised_peel_plotDefaults", gg = FALSE)
})

test_that("`supervised_peel()` generates the expected plot when feature sets provided", {
  set3 <- attributes(sim)$sig_feats$noise[1:3L]
  set4 <- attributes(sim)$sig_feats$noise[7:9L]
  expect_snapshot_plot(
    plot(x, set1 = feats, set3 = set3, set4 = set4),
    "supervised_peel_plotAptamers", gg = FALSE
  )
})

test_that("`supervised_peel()` generates the expected plot when samples are provided", {
  samples <- withr::with_seed(345, sample(rownames(simdata), 5))
  expect_snapshot_plot(
    plot(x, samples = samples),
    "supervised_peel_plotSamples", gg = FALSE
  )
})
