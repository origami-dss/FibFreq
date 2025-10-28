test_that("freq_threshold_crossing throughs an error in case of non-numerical input array x", {
  expect_error(freq_threshold_crossing(rep(TRUE,5)))
  expect_error(freq_threshold_crossing(rep(FALSE,5)))
  expect_error(freq_threshold_crossing(rep("A",5)))
  expect_error(freq_threshold_crossing(rep("ABC",2)))
  expect_error(freq_threshold_crossing(rep(NA,5)))
  expect_error(freq_threshold_crossing(rep(NA_character_,5)))
  expect_error(freq_threshold_crossing(rep(NA_complex_,5)))
  expect_error(freq_threshold_crossing(rep(NA_real_,5)))
  expect_error(freq_threshold_crossing(rep(NA_integer_,5)))
  expect_error(freq_threshold_crossing(rep(NaN,5)))
  expect_error(freq_threshold_crossing(rep(Inf,5)))
  expect_error(freq_threshold_crossing(rep(-Inf,5)))
  expect_error(freq_threshold_crossing(rep(1+1i,5)))

})

test_that("freq_threshold_crossing throughs an error in case of non-numerical or vector-valued input of thresh", {
  expect_error(freq_threshold_crossing(rep(0,10), thresh = "A"))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = TRUE))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NA)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NA_character_)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NA_complex_)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NA_real_)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NA_integer_)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(NaN)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(Inf)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(-Inf)))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = NULL))
  expect_error(freq_threshold_crossing(rep(0,10), thresh = c(0.1, 0.2)))
})



test_that("freq_threshold_crossing throughs an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = "A"))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = TRUE))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NA)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NA_character_)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NA_complex_)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NA_real_)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NA_integer_)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(NaN)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(Inf)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(-Inf)))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = NULL))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = c(0.1, 0.2)))
})

test_that("freq_threshold_crossing throws an error in case of delta_t <= 0", {
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = 0))
  expect_error(freq_threshold_crossing(rep(0,10), delta_t = -1))
})


test_that("freq_threshold_crossing returns a list of length 4", {
  expect_length(freq_threshold_crossing(rep(c(-1,1),5), thresh = 0.5), 4)
  expect_length(freq_threshold_crossing(1:11, delta_t = 2, thresh = 0.5), 4)
  expect_length(freq_threshold_crossing(1:10, thresh = 3), 4)
  expect_length(freq_threshold_crossing(rep(0,10), thresh = 0.5), 4)
})

test_that("freq_threshold_crossing returns correct output", {
  expect_equal(freq_threshold_crossing(1:10, thresh = 1), list(freq_threshold_crossing = NA_real_,  c_v = NA_real_,  n_thresh = NA_real_, threshold_crossings = integer(0)))
  expect_equal(freq_threshold_crossing(rep(0,10), thresh = 1), list(freq_threshold_crossing = NA_real_,   c_v = NA_real_, n_thresh = NA_real_, threshold_crossings = integer(0)))
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5), thresh = 0.5),  list(freq_threshold_crossing = 0.5,  c_v = 0, n_thresh = 4, threshold_crossings = c(3, 5, 7, 9)))
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5), thresh = 0.0),  list(freq_threshold_crossing = 0.5, c_v = 0, n_thresh = 4, threshold_crossings = c(3, 5, 7, 9)))
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5), thresh = 0.5, delta_t = 2),  list(freq_threshold_crossing = 0.25, c_v = 0, n_thresh = 4, threshold_crossings = c(3, 5, 7, 9)))
  expect_equal(freq_threshold_crossing(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5), thresh = 0.5),  list(freq_threshold_crossing = 0.1, c_v = 0, n_thresh = 5, threshold_crossings = c(6, 16, 26, 36, 46)))
  expect_equal(freq_threshold_crossing(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5),  thresh = 0, delta_t = 2),  list(freq_threshold_crossing = 0.05, c_v = 0, n_thresh = 5, threshold_crossings =c( 2, 12, 22, 32, 42)))
  expect_equal(freq_threshold_crossing(rep(c(-1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 5),  thresh = 0.1),  list(freq_threshold_crossing = 0.1, c_v = 0, n_thresh = 5, threshold_crossings = c (6, 16, 26, 36, 46)))

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1 * sin(0.20 * 2*pi * (1:100))

  expect_equal(freq_threshold_crossing(x1, thresh = 0),  list(freq_threshold_crossing = 0.05, c_v = 0, n_thresh = 4, threshold_crossings = c(21, 41, 61, 81)))
  expect_equal(freq_threshold_crossing(x2, thresh = 1),  list(freq_threshold_crossing = 0.05, c_v = 0, n_thresh = 4, threshold_crossings = c(21, 41, 61, 81)))

})

