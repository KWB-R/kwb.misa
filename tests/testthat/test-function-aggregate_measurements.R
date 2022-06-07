#library(kwb.misa)
test_that("aggregate_measurements() works", {

  f1 <- aggregate_measurements

  # My version: using hsGroupByInterval() that I spent a lot of time with!
  f2 <- kwb.misa:::aggregate_measurements_hauke

  expect_error(f1())
  expect_error(f2())

  # Function to generate data as required on the fly
  to_data <- function(...) {
    data <- read.table(sep = ",", header = FALSE, text = c(...))
    data[[1L]] <- as.POSIXct(data[[1L]])
    stats::setNames(data, c("t", "d"))
  }

  # Input data 1: only "non-rounded" timestamps
  data1 <- to_data(
    "2022-06-07 00:00:10,1.0",
    "2022-06-07 00:01:10,2.0",
    "2022-06-07 00:02:10,3.0",
    "2022-06-07 00:03:10,4.0"
  )

  # Input data 2: as data1 + duplicated timestamps
  data2 <- to_data(
    "2022-06-07 00:00:10,1.0",
    "2022-06-07 00:01:10,2.0",
    "2022-06-07 00:02:10,3.0",
    "2022-06-07 00:03:10,4.0", # duplicated |
    "2022-06-07 00:03:10,5.0", # duplicated |- mean = 4, as in data1
    "2022-06-07 00:03:10,3.0"  # duplicated |
  )

  # Input data 3: as data2 but randomly ordered
  data3 <- data2[sample(nrow(data2)), ]

  # Expected result when grouping data1 by 1 minute
  expected1_60 <- to_data(
    "2022-06-07 00:00:00,1.0",
    "2022-06-07 00:01:00,2.0",
    "2022-06-07 00:02:00,3.0",
    "2022-06-07 00:03:00,4.0"
  )

  # Expected result when grouping data1 by 2 minutes
  expected1_120 <- to_data(
    "2022-06-07 00:00:00,1.5",
    "2022-06-07 00:02:00,3.5"
  )

  # Expected result when grouping data2 by 1 minute
  expected2_60 <- expected1_60

  # Expected result when grouping data2 by 2 minutes
  expected2_120 <- to_data(
    "2022-06-07 00:00:00,1.5",
    "2022-06-07 00:02:00,3.75" # 3.75 = (3 + 4 + 5 + 3)/4
  )

  # Expected results for data3
  expected3_60 <- expected2_60
  expected3_120 <- expected2_120

  # Function to check for equality, not identity
  check <- function(data, expected) expect_true(all.equal(data, expected))

  # checking data1, 60s
  check(f1(data1$t, data1$d, 60), expected1_60)
  check(f2(data1$t, data1$d, 60), expected1_60)

  # checking data2, 60s
  check(f1(data2$t, data2$d, 60), expected2_60)
  check(f2(data2$t, data2$d, 60), expected2_60)

  # checking data3, 60s
  #check(f1(data3$t, data3$d, 60), expected3_60) # fails!
  check(f2(data3$t, data3$d, 60), expected3_60)

  # checking data1, 120s
  #check(f1(data1$t, data1$d, 120), expected1_120) # fails!
  check(f2(data1$t, data1$d, 120), expected1_120)

  # checking data2, 120s
  #check(f1(data2$t, data2$d, 120), expected2_120) # fails!
  check(f2(data2$t, data2$d, 120), expected2_120)

  # checking data3, 120s
  #check(f1(data3$t, data3$d, 120), expected3_120) # fails!
  check(f2(data3$t, data3$d, 120), expected3_120)
})
