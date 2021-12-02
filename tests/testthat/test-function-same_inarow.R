test_that("same_inarow() works", {

  # Just a shortcut
  f <- kwb.misa:::same_inarow

  v <- c(1, 2, 2, 3, 3, 3)

  expect_error(f())

  expected <- data.frame(
    Value = c("1", "2", "3"),
    repeats = c(1, 2, 3),
    starts_at = c(1, 2, 4),
    ends_at = c(1, 3, 6)
  )

  expect_identical(f(v), expected)
})
