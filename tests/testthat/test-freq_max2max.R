test_that("freq_max2max throws an error in case of non-numerical input array x", {
  expect_error(freq_max2max(rep(TRUE,8)))
  expect_error(freq_max2max(rep(FALSE,7)))
  expect_error(freq_max2max(rep("A",6)))
  expect_error(freq_max2max(rep("ABC",6)))
  expect_error(freq_max2max(rep(NA,4)))
  expect_error(freq_max2max(rep(NA_character_,4)))
  expect_error(freq_max2max(rep(NA_complex_,4)))
  expect_error(freq_max2max(rep(NA_real_,4)))
  expect_error(freq_max2max(rep(NA_integer_,4)))
  expect_error(freq_max2max(rep(NaN,4)))
  expect_error(freq_max2max(rep(Inf,4)))
  expect_error(freq_max2max(rep(-Inf,3)))
  expect_error(freq_max2max(rep(1 + 1i,10)))})

test_that("freq_max2max throughs an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_max2max(rep(0,10), delta_t = "A"))
  expect_error(freq_max2max(rep(0,10), delta_t = TRUE))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NA)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NA_character_)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NA_complex_)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NA_real_)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NA_integer_)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(NaN)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(Inf)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(-Inf)))
  expect_error(freq_max2max(rep(0,10), delta_t = NULL))
  expect_error(freq_max2max(rep(0,10), delta_t = c(0.1, 0.2)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(-0.1)))
  expect_error(freq_max2max(rep(0,10), delta_t = c(1 + 1i)))
})

test_that("freq_max2max throws an error in case of delta_t <= 0", {
  expect_error(freq_max2max(rep(0,10), delta_t = 0))
  expect_error(freq_max2max(rep(0,10), delta_t = -1))
})

test_that("freq_max2max writes a message in case wrong names for optional arguments", {
  expect_message(freq_max2max(rep(0,10), spam = 20))
})


test_that("freq_max2max writes a message in case of var(x) = 0", {
  expect_message(freq_max2max(rep(0,10)))
  expect_message(freq_max2max(rep(10,10)))
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
  expect_equal(freq_max2max(1:10), list(freq_max2max = NA_real_, c_v = NA_real_, n_max = 0, maxima_sampled = integer(0)))
  expect_equal(freq_max2max(rep(0,10)), list(freq_max2max = NA_real_, c_v = NA_real_, n_max = NA_real_, maxima_sampled = integer(0)))
  expect_equal(freq_max2max(rep(c(1,-1),5)),  list(freq_max2max = NA_real_,  c_v = NA_real_, n_max = 0, maxima_sampled = integer(0)))
  expect_equal(freq_max2max(rep(c(-1, 0., 0., 0., 0., 1, 0., 0., 0., 0.), 5)), list(freq_max2max = 0.1, c_v = 0,   n_max = 4.0, maxima_sampled = c(6, 16, 26, 36)))
  expect_equal(freq_max2max(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5)), list(freq_max2max = 0.1, c_v = 0,  n_max = 4.0, maxima_sampled = c(6, 16, 26, 36)))
  expect_equal(freq_max2max(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5),  delta_t = 2),  list(freq_max2max = 0.05, c_v = 0, n_max = 4.0, maxima_sampled = c(6, 16, 26, 36)))

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1* sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_max2max(x1),  list(freq_max2max = 0.05, c_v = 0, n_max = 4, maxima_sampled = c(25,45,65,85)))
  expect_equal(freq_max2max(x2),  list(freq_max2max = 0.05,  c_v = 0, n_max = 5, maxima_sampled = c(6, 26, 46, 66, 86)))
})

