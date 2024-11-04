
test_that("`pca` S3 print method returns known output", {
  expect_snapshot_output(pca(sim_adat))
})

test_that("`supervised_peel` S3 print method returns known output", {
  skip("Return test once `supervised_peel()` is back in")
  sim <- log_rfu(sim_adat)
  sim$Response <- factor(sim$class_response)
  apts1 <- attributes(sim)$sig_feats$class
  apts2 <- attributes(sim)$sig_feats$reg
  apts3 <- attributes(sim)$sig_feats$surv
  sp    <- supervised_peel(sim, aptamers = apts1,
                           aptamers2 = apts2, aptamers3 = apts3)

  expect_snapshot_output(sp)
})
