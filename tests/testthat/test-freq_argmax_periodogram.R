test_that("freq_argmax_periodogram throws an error in case of vector-valued or non-numerical input array x", {
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


test_that("freq_argmax_periodogram throws an error in case of non-numerical or vector-valued input of delta_t", {
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


test_that("freq_argmax_periodogram throws an error in case of delta_t <= 0", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = -1))
})



test_that("freq_argmax_periodogram throws an error in case of welch_window is vector-valued or non logical", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = c(FALSE, TRUE)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = 1))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = Inf))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, welch_window = NULL))

})


test_that("freq_argmax_periodogram throws an error in case of f_min is vector-valued or non-numerical or negative", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = c(0.1, 0.2)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = FALSE))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = Inf))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = NULL))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_min = -1))

})



test_that("freq_argmax_periodogram throws an error in case of f_max is vector-valued or non-numerical or non-positive", {
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = c(0.1, 0.2)))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = FALSE))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = "A"))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NA))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NaN))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = NULL))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = 0))
  expect_error(freq_argmax_periodogram(rep(0,10), delta_t = 0.5, f_max = -1))

})

test_that("freq_argmax_periodogram throughs an error in case of f_max < f_min", {
  expect_error(freq_argmax_periodogram(rnorm(1000), f_min = 10, f_max =1,  delta_t = 0.001))
  expect_error(freq_argmax_periodogram(rnorm(1000), f_min = 10, f_max =9.9,  delta_t = 0.001))
})


test_that("freq_argmax_periodogram throughs an error in case of f_min >  1. / 2 / delta_t", {
  expect_error(freq_argmax_periodogram(rnorm(1000), f_min = 0.6))
  expect_error(freq_argmax_periodogram(rnorm(1000), f_min = 501, delta_t = 0.001))
})

test_that("freq_argmax_periodogram throughs an error in case of f_max <  1./ delta_t/ length(x)", {
  expect_error(freq_argmax_periodogram(rnorm(1000), f_max = 0.0006))
  expect_error(freq_argmax_periodogram(rnorm(1000), f_max = 0.99, delta_t = 0.001))
})

test_that("freq_argmax_periodogram throughs an error in case of the interval [f_min, f_max] contains less than 3 frequencies", {
  expect_warning(freq_argmax_periodogram(rnorm(1000), f_min = 0.098, f_max = 0.100))
  expect_warning(freq_argmax_periodogram(rnorm(1000), f_min = 10, f_max = 12, delta_t = 0.001))
})

test_that("freq_argmax_periodogram throws an error in case of bo sampled frequency in [f_min, f_max]", {
  expect_error(freq_argmax_periodogram(rnorm(1000), delta_t = 0.001, f_min = 15.1, f_max = 15.3))
  expect_error(freq_argmax_periodogram(rnorm(10),  f_min = 0.11, f_max = 0.12))
})


test_that("freq_argmax_periodogram returns an array of length 3", {
  expect_length(freq_argmax_periodogram(rep(c(-1,1),5)), 3)
  expect_length(freq_argmax_periodogram(1:11), 3)
  expect_length(freq_argmax_periodogram(1:10), 3)
  expect_length(freq_argmax_periodogram(rep(0,10)), 3)

})


test_that("Numetical results of freq_argmax_periodogram", {
  expect_equal(freq_argmax_periodogram(1:10)[1], list(freq_argmax_periodogram  = 0.1))

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1 * sin(0.20 * 2*pi * (1:100))
  x3 <- 1 * sin(0.05 * 2*pi * (1:100)) + 3 * sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_argmax_periodogram(x1),  list(freq_argmax_periodogram = 0.05, Max_P = 50, expl_var = 1.0))
  expect_equal(freq_argmax_periodogram(x2),  list(freq_argmax_periodogram = 0.05, Max_P = 450, expl_var = 0.9))
  expect_equal(freq_argmax_periodogram(x3),  list(freq_argmax_periodogram = 0.20, Max_P = 450, expl_var = 0.9))

})

