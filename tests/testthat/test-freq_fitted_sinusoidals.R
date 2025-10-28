

test_that("freq_fitted_sinusoidals throws an error in case of x is not vector-valued or contains non-numerical elements", {
  expect_error(freq_fitted_sinusoidals(c(TRUE), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(FALSE), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(c("A"), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c("ABC"), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(c(NA), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(NA_character_), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(NA_complex_), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(NA_real_), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(NA_integer_), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(NaN), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(c(Inf), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(c(-Inf), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(NULL, test_freqs = c(1.0)))
})

test_that("freq_fitted_sinusoidals throws an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = "A", test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = TRUE, test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NA)), test_freqs = c(1.0))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NA_character_)), test_freqs = c(1.0))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NA_complex_)), test_freqs = c(1.0))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NA_real_)), test_freqs = c(1.0))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NA_integer_)), test_freqs = c(1.0))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(NaN), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(Inf), test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(-Inf), test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = NULL, test_freqs = c(1.0)))

  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = c(0.1, 0.2), test_freqs = c(1.0)))

})


test_that("freq_fitted_sinusoidals throws an error in case of delta_t <= 0", {
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0, test_freqs = c(1.0)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = -1, test_freqs = c(1.0)))
})

test_that("freq_fitted_sinusoidals throws an error in case of test_freqs is not vector-valued or contains non-numerical or negative elements" , {
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(FALSE)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c("A")))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(NA)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(NaN)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(Inf)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(NULL)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 0.5, test_freqs = c(-1)))
})

test_that("freq_fitted_sinusoidals throws an error in case of test_freqs <= 0", {
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 1, test_freqs = c(-1.0)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 1, test_freqs = c(-1.0, 1.0)))
  expect_error(freq_fitted_sinusoidals(rep(0,10), delta_t = 1, test_freqs = c(0.0)))
})


test_that("freq_fitted_sinusoidals returns a list of length 4", {
  expect_length(freq_fitted_sinusoidals(rep(c(-1,1),5), delta_t = 5,  test_freqs = c(0.1)), 4)
  expect_length(freq_fitted_sinusoidals(1:11, delta_t = 3, test_freqs = c(0.2)), 4)
  expect_length(freq_fitted_sinusoidals(1:10,  delta_t = 3, test_freqs = c(0.3)), 4)
})



test_that("freq_fitted_sinusoidals returns the correct results", {
  expect_equal(freq_fitted_sinusoidals(rep(c(-1,1),5), delta_t = 5,  test_freqs = 0.1)[["expl_var"]], 1)

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1 * sin(0.20 * 2*pi * (1:100))
  x3 <- 1 * sin(0.05 * 2*pi * (1:100)) + 3 * sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_fitted_sinusoidals( x1,  test_freqs = 0.05)[["expl_var"]], 1)
  expect_equal(freq_fitted_sinusoidals( x2,  test_freqs = 0.05)[["expl_var"]], 0.9)
  expect_equal(freq_fitted_sinusoidals( x3,  test_freqs = 0.05)[["expl_var"]], 0.1)
  expect_equal(freq_fitted_sinusoidals( x2,  test_freqs = 0.2)[["expl_var"]], 0.1)
  expect_equal(freq_fitted_sinusoidals( x3,  test_freqs = 0.2)[["expl_var"]], 0.9)

})


