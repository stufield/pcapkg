
feat <- grep("^seq", names(sim_adat), value = TRUE)
for (i in feat) sim_adat[[i]] <- log10(sim_adat[[i]])
pca <- pca(sim_adat)

test_that("`get_pca_names()` returns correct 'rotation' object when value = positive", {
  rot1 <- get_pca_names(pca, "r", 1, 0.2)      # feature names
  expect_type(rot1, "character")
  expect_true(all(rot1 %in% names(sim_adat)))
  expect_length(rot1, 7L)
  expect_equal(
    rot1,
    c("seq.3459.49", "seq.3865.56", "seq.4487.88", "seq.5994.84",
      "seq.2277.95", "seq.4914.10", "seq.8797.98")
  )
})

test_that("`get_pca_names()` returns correct 'rotation' object when value = negative", {
  rot2 <- get_pca_names(pca, "r", 1, -0.04)      # feature names
  expect_type(rot2, "character")
  expect_true(all(rot2 %in% names(sim_adat)))
  expect_length(rot2, 11L)
  expect_equal(
    rot2,
    c("seq.5751.80", "seq.9011.72", "seq.2953.31", "seq.4330.4",
      "seq.3896.5", "seq.1130.49", "seq.8441.53", "seq.9360.55",
      "seq.6615.18", "seq.8993.16", "seq.9373.82")
  )
})

test_that("`get_pca_names()` returns correct 'rotation' object with defaults", {
  expect_equal(get_pca_names(pca, "r", 1, value = 0.1),
               get_pca_names(pca, value = 0.1))
  expect_equal(get_pca_names(pca, "r", 1, value = -0.04),
               get_pca_names(pca, value = -0.04))
})

test_that("`get_pca_names` returns correct 'projection' object when value = positive", {
  pro1 <- get_pca_names(pca, "p", 1, 0.2)
  expect_type(pro1, "character")
  expect_true(all(pro1 %in% rownames(sim_adat)))
  expect_length(pro1, 9L)
  expect_equal(
    pro1,
    c("74703475_1", "56899633_1", "46713227_1", "97975242_1",
      "35984450_1", "25885578_1", "35274802_1", "71305641_1", "88325979_1")
  )
})

test_that("`get_pca_names()` returns correct 'projection' object when value = negative", {
  pro2 <- get_pca_names(pca, "p", 1, -0.2)
  expect_type(pro2, "character")
  expect_true(all(pro2 %in% rownames(sim_adat)))
  expect_length(pro2, 8L)
  expect_equal(
    pro2,
    c("51966571_1", "68004202_1", "59046590_1", "18797344_1",
      "26463627_1", "52672763_1", "36965772_1", "75692351_1")
  )
})
