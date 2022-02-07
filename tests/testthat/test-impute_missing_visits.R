# Test missing visit imputation
# Gabriel Odom
# 2022-01-11
# UPDATED: 2022-02-02

test_that("LOCF imputation works", {
  expect_equal(
    "++++**-------+--+++----+++++++",
    impute_missing_visits("++++*o-------+--+oo-o-o+o+oooo", method = "locf")
  )
})

test_that("LOCF-D imputation works", {
  expect_equal(
    "++++**-------+--+++----+++oooo",
    impute_missing_visits("++++*o-------+--+oo-o-o+o+oooo", method = "locfD")
  )
})

test_that("mode imputation works", {
  expect_equal(
    "++++*--------+--+------+-+----",
    impute_missing_visits("++++*o-------+--+oo-o-o+o+oooo", method = "mode")
  )
})



######  kNV  ##################################################################
test_that("basic kNV imputation works", {
  expect_equal(
    "++++**-------+--+oo---*++++*oo",
    impute_missing_visits(
      "++++*o-------+--+oo-o-o+o+o*oo",
      method = "kNV",
      quietly = TRUE
    )
  )
})


