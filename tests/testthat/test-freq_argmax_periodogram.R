test_that("freq_argmax_periodogram throughs an error in case of vector-valued or non-numerical input array x", {
  expect_error(freq_argmax_periodogram(c(TRUE)))
  expect_error(freq_argmax_periodogram(c(FALSE)))

  expect_error(freq_argmax_periodogram(c("A")))
  expect_error(freq_argmax_periodogram(c("ABC")))

  expect_error(freq_argmax_periodogram(c(NA)))
  expect_error(freq_argmax_periodogram(c(NA_character_)))
  expect_error(freq_argmax_periodogram(c(NA_complex_)))
  expect_error(freq_argmax_periodogram(c(NA_real_)))
  expect_error(freq_argmax_periodogram(c(NA_integer_)))
  expect_error(freq_argmax_periodogram(c(NaN)))

  expect_error(freq_argmax_periodogram(c(Inf)))
  expect_error(freq_argmax_periodogram(c(-Inf)))

  expect_error(freq_argmax_periodogram(NULL))
})


test_that("freq_argmax_periodogram throughs an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = TRUE))

  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NA)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NA_character_)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NA_complex_)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NA_real_)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NA_integer_)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(NaN)))

  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(Inf)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(-Inf)))

  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = NULL))

  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = c(0.1, 0.2)))

})


test_that("freq_argmax_periodogram throughs an error in case of delta_t <= 0", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = -1))
})



test_that("freq_argmax_periodogram throughs an error in case of welch_window is vector-valued or non logical", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = c(FALSE, TRUE)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = 1))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = Inf))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NULL))

})


test_that("freq_argmax_periodogram throughs an error in case of f_min is vector-valued or non-numerical or negative", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = c(0.1, 0.2)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = FALSE))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = Inf))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NULL))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = -1))

})



test_that("freq_argmax_periodogram throughs an error in case of f_max is vector-valued or non-numerical or non-positive", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = c(0.1, 0.2)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = FALSE))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NULL))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = 0))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = -1))

})

test_that("freq_argmax_periodogram throughs an error in case of f_min < f_max and contains a sampled frequency", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = 0.1, f_max = 0.1))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = 0.11, f_max = 0.12))

})


test_that("freq_argmax_periodogram returns an array of length 2", {
  expect_length(freq_argmax_periodogram(rep(c(-1,1),5)), 2)
  expect_length(freq_argmax_periodogram(1:11), 2)
  expect_length(freq_argmax_periodogram(1:10), 2)
  expect_length(freq_argmax_periodogram(rep(0,10)), 2)

})


test_that("freq_argmax_periodogram", {
  expect_equal(freq_argmax_periodogram(1:10)[1], 0.1, ignore_attr = TRUE)
  expect_equal(freq_argmax_periodogram(rep(0,10)), c(NA_real_, NA_real_), ignore_attr = TRUE)
  expect_equal(freq_argmax_periodogram(rep(c(1,-1),5)),  c(0.5, 1.0 ), ignore_attr = TRUE)
  expect_equal(freq_argmax_periodogram(rep(c(1,-1),5), delta_t = 2),  c(0.25, 1.0), ignore_attr = TRUE)
  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1 * sin(0.20 * 2*pi * (1:100))
  x3 <- 1 * sin(0.05 * 2*pi * (1:100)) + 3 * sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_argmax_periodogram(x1),  c(0.05, 1.0), ignore_attr = TRUE)
  expect_equal(freq_argmax_periodogram(x2),  c(0.05, 0.9), ignore_attr = TRUE)
  expect_equal(freq_argmax_periodogram(x3),  c(0.2, 0.9), ignore_attr = TRUE)

})

