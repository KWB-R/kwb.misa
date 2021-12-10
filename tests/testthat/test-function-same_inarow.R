#library(testthat)

test_that("same_inarow() works", {

  # Just a shortcut
  f <- kwb.misa:::same_inarow

  v1 <- c(1, 2, 2, 3, 3, 3)
  v2 <- 1:100

  expect_error(f())

  expected1 <- data.frame(
    Value = c("1", "2", "3"),
    repeats = c(1, 2, 3),
    starts_at = c(1, 2, 4),
    ends_at = c(1, 3, 6)
  )

  expected2 <- data.frame(
    Value = as.character(1:100),
    repeats = rep(1, 100),
    starts_at = as.numeric(1:100),
    ends_at = as.numeric(1:100)
  )

  expect_identical(f(v1), expected1)
  expect_identical(f(v2), expected2)

  expect_warning(f(c(TRUE), "different results!"))
  #kwb.misa:::same_inarow_v1(TRUE)
  #   Value repeats starts_at ends_at
  # 1     1       1         1       1
  #kwb.misa:::same_inarow_v2(TRUE)
  #   Value repeats starts_at ends_at
  # 1  TRUE       1         1       1

})
