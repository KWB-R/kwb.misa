#library(testthat)

test_that("same_inarow() works", {

  # Just a shortcut
  f <- kwb.misa:::same_inarow

  v1 <- c(1, 2, 2, 3, 3, 3)
  v2 <- 1:100

  expect_error(f())

  expected1 <- data.frame(
    Value = c(1, 2, 3),
    repeats = c(1L, 2L, 3L),
    starts_at = c(1L, 2L, 4L),
    ends_at = c(1L, 3L, 6L)
  )

  expected2 <- data.frame(
    Value = 1:100,
    repeats = rep(1L, 100),
    starts_at = as.integer(1:100),
    ends_at = as.integer(1:100)
  )

  expect_identical(f(v1), expected1)
  expect_identical(f(v2), expected2)
})
