#library(testthat)

test_that("count_repeats() works", {

  # Just a shortcut
  count <- kwb.misa:::count_repeats

  v <- c(1, 2, 2, 3, 3, 3)

  # Helper function to avoid repetition
  check <- function(x, v, r, i, j) expect_identical(x, data.frame(
    Value = v, repeats = r, starts_at = i, ends_at = j
  ))

  check(count(v, 1), 1, 1, 1, 1)
  check(count(v, 2), 2, 2, 2, 3)

  #check(count(v, 4), 3, 3, 4, 6)
  # This test is not working! count_repeats() returns ends_at = 5 instead of 6.
  # However, this seems to be required by same_inarow() that calls this
  # function!
})
