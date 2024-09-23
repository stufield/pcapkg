
test_that("pca print method returns known output", {
  expect_snapshot_output(pca(sim_test_data))
})

test_that("supervised peel print method returns known output", {
  sim <- log10(sim_test_data)
  sim$Response <- factor(sim$class_response)
  apts1 <- attributes(sim)$sig_feats$class
  apts2 <- attributes(sim)$sig_feats$reg
  apts3 <- attributes(sim)$sig_feats$surv
  sp    <- supervised_peel(sim, aptamers = apts1,
                           aptamers2 = apts2, aptamers3 = apts3)

  expect_snapshot_output(sp)
})
