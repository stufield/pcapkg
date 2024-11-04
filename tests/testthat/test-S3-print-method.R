
test_that("`pca` S3 print method returns known output", {
  expect_snapshot_output(pca(sim_adat))
})

test_that("`supervised_peel` S3 print method returns known output", {
  skip("Return test once `supervised_peel()` is back in")
  sim <- log_rfu(sim_adat)
  sim$Response <- factor(sim$class_response)
  set1 <- attributes(sim)$sig_feats$class
  set2 <- attributes(sim)$sig_feats$reg
  set3 <- attributes(sim)$sig_feats$surv
  sp   <- supervised_peel(sim, set1 = set1, set2 = set2, set3 = set3)

  expect_snapshot_output(sp)
})
