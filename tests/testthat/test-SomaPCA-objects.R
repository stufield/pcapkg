
test_that("the `sh_list` object is correct and of the right form", {
  expect_named(sh_list, c("Complement", "plasmaLysis",
                          "Platelet", "serumLysis"))
  expect_true(
    all(vapply(sh_list, typeof, "") == "character")
  )
  expect_true(all(vapply(unlist(sh_list), is.apt, NA)))
  expect_equal(lengths(sh_list, use.names = FALSE), c(4L, 54L, 17L, 32L))
})

test_that("the `smvs` object is correct and of the right form", {
  expect_named(smvs, c("SerumCellAbuse", "PlasmaCellAbuse",
                       "PlasmaPlatelet", "Complement"))
  expect_true(
    all(vapply(smvs, function(.x) typeof(.x) == "double", NA))
  )
  expect_equal(range(unlist(smvs)), c(0.0435813115, 0.3117243628))
  expect_true(all(vapply(unlist(smvs), function(.x) .x > 0, NA)))
  expect_true(all(vapply(unlist(smvs), function(.x) .x < 0.32, NA)))
  expect_equal(lengths(smvs, use.names = FALSE), c(32L, 54L, 17L, 6L))
  expect_equal(vapply(smvs, sum, 0.1, USE.NAMES = FALSE),
               c(5.197537080, 6.080629428, 3.252799677, 0.929160989))
})
