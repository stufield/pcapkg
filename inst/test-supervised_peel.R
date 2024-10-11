withr::local_options(list(digits = 6L))

# Setup ----
sim <- log10(sim_test_data)
sim$Response <- factor(sim$class_response)
apts <- attributes(sim)$sig_feats$class
x    <- supervised_peel(sim, aptamers = apts)

# Testing ----
test_that("`supervised_peel` generates correct output", {
  expect_s3_class(x, "supervised_peel")
  expect_named(x, c("orig", "weighted", "unweighted", "peeled", "peeled_data",
                    "center", "scale", "logged", "apts", "orig_data", "call"))
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
               c(orig = 5, weighted = 5, unweighted = 5,
                 peeled = 5, peeled_data = 41, center = 1,
                 scale = 1, logged = 1, apts = 5,
                 orig_data = 56, call = 3))
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
                    apts        = "list",
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

  expect_equal(sum(strip_meta(x$peeled_data)), 13571.7299406929)
  expect_equal(x$peeled_data$Response, x$orig_data$Response)
})

test_that("`supervised_peel()` generates the expected plot", {
  expect_snapshot_plot(plot(x), "supervised_peel_plotDefaults", gg = FALSE)
})

test_that("`supervised_peel()` generates the expected plot when aptamers are provided", {
  apts2 <- attributes(sim)$sig_feats$surv
  apts3 <- attributes(sim)$sig_feats$noise[1:3]
  apts4 <- attributes(sim)$sig_feats$noise[7:9]
  expect_snapshot_plot(
    plot(x, aptamers = apts, aptamers3 = apts3, aptamers4 = apts4),
    "supervised_peel_plotAptamers", gg = FALSE
  )
})

test_that("`supervised_peel()` generates the expected plot when samples are provided", {
  samples <- withr::with_seed(345, sample(rownames(sim_test_data), 5))
  expect_snapshot_plot(
    plot(x, samples = samples),
    "supervised_peel_plotSamples", gg = FALSE
  )
})
