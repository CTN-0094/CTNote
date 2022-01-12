test_that("LOCF-D imputation works", {
  expect_equal(
    "+oooooooooooooo",
    impute_missing_visits("+oooooooooooooo", method = "locfD")
  )
})


