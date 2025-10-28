test_that("freq_Lorentz_fit throws an error in case of non-numerical input array x", {
  expect_error(freq_Lorentz_fit(rep(TRUE,5)))
  expect_error(freq_Lorentz_fit(rep(FALSE,5)))

  expect_error(freq_Lorentz_fit(rep("A",5)))
  expect_error(freq_Lorentz_fit(rep("ABC")))

  expect_error(freq_Lorentz_fit(rep(NA,5)))
  expect_error(freq_Lorentz_fit(rep(NA_character_,5)))
  expect_error(freq_Lorentz_fit(rep(NA_complex_,5)))
  expect_error(freq_Lorentz_fit(rep(NA_real_,5)))
  expect_error(freq_Lorentz_fit(rep(NA_integer_,5)))
  expect_error(freq_Lorentz_fit(rep(NaN,5)))

  expect_error(freq_Lorentz_fit(rep(Inf,5)))
  expect_error(freq_Lorentz_fit(rep(-Inf,5)))
  expect_error(freq_Lorentz_fit(NULL))

})

test_that("freq_Lorentz_fit throughs an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = "A"))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = TRUE))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NA)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NA_character_)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NA_complex_)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NA_real_)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NA_integer_)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(NaN)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(Inf)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(-Inf)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = NULL))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(0.1, 0.2)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(-0.1)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = c(1 + 1i)))
})

test_that("freq_Lorentz_fit throughs an error in case of non-numerical or vector-valued input of f_max", {
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = "A"))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = TRUE))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NA)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NA_character_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NA_complex_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NA_real_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NA_integer_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(NaN)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(-Inf)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = NULL))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(0.1, 0.2)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(-0.1)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_max = c(1 + 1i)))
})

test_that("freq_Lorentz_fit throughs an error in case of non-numerical or vector-valued input of f_min", {
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = "A"))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = TRUE))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NA)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NA_character_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NA_complex_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NA_real_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NA_integer_)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(NaN)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(Inf)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(-Inf)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = NULL))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(0.1, 0.2)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(-0.1)))
  expect_error(freq_Lorentz_fit(rnorm(10), f_min = c(1 + 1i)))
})


test_that("freq_Lorentz_fit throws an error in case of welch_window is vector-valued or non logical", {
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = c(FALSE, TRUE)))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = 1))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = "A"))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = NA))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = NaN))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = Inf))
  expect_error(freq_Lorentz_fit(rnorm(10), delta_t = 0.5, welch_window = NULL))

})

test_that("freq_Lorentz_fit throughs an error in case of f_max < f_min", {
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 10, f_max =1,  delta_t = 0.001))
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 10, f_max =9.9,  delta_t = 0.001))
})


test_that("freq_Lorentz_fit throughs an error in case of f_min >  1. / 2 / delta_t", {
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 0.6))
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 501, delta_t = 0.001))
})

test_that("freq_Lorentz_fit throughs an error in case of f_max <  1./ delta_t/ length(x)", {
  expect_error(freq_Lorentz_fit(rnorm(1000), f_max = 0.0006))
  expect_error(freq_Lorentz_fit(rnorm(1000), f_max = 0.99, delta_t = 0.001))
})

test_that("freq_Lorentz_fit throughs an error in case of the interval [f_min, f_max] contains less than 3 frequencies", {
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 0.098, f_max = 0.100))
  expect_error(freq_Lorentz_fit(rnorm(1000), f_min = 10, f_max = 12, delta_t = 0.001))
})

test_that("freq_Lorentz_fit returns an array of length 5", {

  expect_length(freq_Lorentz_fit(rep(c(-1,1), 500),  delta_t = 500), 5)

#  expect_length(freq_Lorentz_fit(rep(c(-1,1), 500), delta_t = 0.1, welch_window = TRUE), 5)

})

test_that("freq_Lorentz returns correct output", {

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- sin(0.20 * 2*pi * (1:100))
  expect_equal(freq_Lorentz_fit(x1)$freq_Lorentz,  0.05)
  expect_equal(freq_Lorentz_fit(x2)$freq_Lorentz, 0.20 )
  expect_equal(freq_Lorentz_fit(x2, welch_window = TRUE)$freq_Lorentz, 0.20, tolerance = 0.0001 )
})





