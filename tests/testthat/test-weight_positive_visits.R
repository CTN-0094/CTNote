# Test weighting visits
# Gabriel Odom
# 2022-03-10

pattern_char <- "++o+*-------+--+-o-o-o+o+"

test_that("Default weights", {
  expect_equal(
    41.28,
    weight_positive_visits(pattern_char),
    tolerance = 0.0001
  )
})

test_that("Vary static weights", {
  expect_equal(
    52.8,
    weight_positive_visits(
      pattern_char,
      weights_num = c(`+` = 0.8, `*` = 0.4, `o` = 1, `-` = 0)
    ),
    tolerance = 0.0001
  )
})


newPosPenal_num <- seq(
  from = 1, to  = 5,
  length = stringr::str_length(pattern_char)
)
test_that("Increase positive penalty", {
  expect_equal(
    34.82667,
    weight_positive_visits(pattern_char, posPenalty_num = newPosPenal_num),
    tolerance = 0.0001
  )
})


newMissPenal_num <- rep(1, stringr::str_length(pattern_char))
newMissPenal_num[1:4] <- 3
test_that("Increase positive penalty", {
  expect_equal(
    43.392,
    weight_positive_visits(pattern_char, missPenalty_num = newMissPenal_num),
    tolerance = 0.0001
  )
})
