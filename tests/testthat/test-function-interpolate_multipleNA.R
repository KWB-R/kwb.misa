#library(testthat)

test_that("interpolate_multipleNA() works", {

  f <- kwb.misa:::interpolate_multipleNA

  # There are warnings due to differences in same_inarow_v1() and
  # same_inarow_v2()
  expect_warning(result1 <- f(data_vector = 1:10, max_na = 3L))
  expect_warning(result2 <- f(data_vector = c(1, NA, NA, NA, 2), max_na = 3L))
  expect_warning(result3 <- f(data_vector = c(1, NA, NA, NA, 2), max_na = 2L))

  expect_identical(result1, list(
    1:10, c("NA's interpolated" = 0, "NA's in total" = 0)
  ))

  expect_identical(result2, list(
    c(1, 1.25, 1.5, 1.75, 2),
    c("NA's interpolated" = 3, "NA's in total" = 3)
  ))

  expect_identical(result3, list(
    c(1, NA, NA, NA, 2),
    c("NA's interpolated" = 0, "NA's in total" = 3)
  ))
})
