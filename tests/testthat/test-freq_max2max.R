test_that("freq_max2max throughs an error in case of non-numerical input array x", {
  expect_error(freq_max2max(c(TRUE)))
  expect_error(freq_max2max(c(FALSE)))

  expect_error(freq_max2max(c("A")))
  expect_error(freq_max2max(c("ABC")))

  expect_error(freq_max2max(c(NA)))
  expect_error(freq_max2max(c(NA_character_)))
  expect_error(freq_max2max(c(NA_complex_)))
  expect_error(freq_max2max(c(NA_real_)))
  expect_error(freq_max2max(c(NA_integer_)))
  expect_error(freq_max2max(c(NaN)))

  expect_error(freq_max2max(c(Inf)))
  expect_error(freq_max2max(c(-Inf)))
})


test_that("freq_max2max returns an array of length 4", {
  expect_length(freq_max2max(rep(c(-1,1),5)), 4)
  expect_length(freq_max2max(rep(c(-1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 5)), 4)
  expect_length(freq_max2max(1:11), 4)
  expect_length(freq_max2max(1:10), 4)
  expect_length(freq_max2max(rep(0,10)), 4)
  expect_error(freq_max2max(NULL))
})

test_that("freq_max2max returns correct output", {
  expect_equal(freq_max2max(1:10), c(NA_real_, NA_real_, NA_real_, 0), ignore_attr = TRUE)
  expect_equal(freq_max2max(rep(0,10)), c(NA_real_, NA_real_, NA_real_, 0), ignore_attr = TRUE)
  expect_equal(freq_max2max(rep(c(1,-1),5)),  c(NA_real_, NA_real_, NA_real_, 0), ignore_attr = TRUE)
  expect_equal(freq_max2max(rep(c(-1, 0., 0., 0., 0., 1, 0., 0., 0., 0.), 5)), c(0.1, 0, 0, 4.0), ignore_attr = TRUE)
  expect_equal(freq_max2max(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5)),  c(0.1, 0, 0, 4.0), ignore_attr = TRUE)
  expect_equal(freq_max2max(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5),  delta_t = 2),  c(0.05, 0, 0, 4.0), ignore_attr = TRUE)

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1* sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_max2max(x1),  c(0.05, 0, 0, 4), ignore_attr = TRUE)
  expect_equal(freq_max2max(x2),  c(0.05, 0, 0, 5), ignore_attr = TRUE)
})

