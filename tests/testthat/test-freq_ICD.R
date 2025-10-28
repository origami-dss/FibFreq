test_that("freq_ICD throughs an error in case of non-numerical input array x", {
  expect_error(freq_ICD(rep(TRUE,3)))
  expect_error(freq_ICD(rep(FALSE,3)))

  expect_error(freq_ICD(rep("A",5)))
  expect_error(freq_ICD(rep("ABC",5)))

  expect_error(freq_ICD(rep(NA,5)))
  expect_error(freq_ICD(rep(NA_character_,5)))
  expect_error(freq_ICD(rep(NA_complex_,5)))
  expect_error(freq_ICD(rep(NA_real_,5)))
  expect_error(freq_ICD(rep(NA_integer_,5)))
  expect_error(freq_ICD(rep(NaN,5)))

  expect_error(freq_ICD(rep(Inf,4)))
  expect_error(freq_ICD(rep(-Inf,3)))
  expect_error(freq_ICD(rep(1 - 1i, 10)))
  expect_error(freq_ICD(NULL))
})

test_that("freq_ICD throughs an error in case of an input array x with a length <=2", {
  expect_error(freq_ICD(c(1)))
  expect_error(freq_ICD(c(1,1)))
})


test_that("freq_ICD throughs an error in case of non-numerical or vector-valued input of thresh_min", {
  expect_error(freq_ICD(rep(0,10), thresh_min = "A"))
  expect_error(freq_ICD(rep(0,10), thresh_min = TRUE))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NA)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NA_character_)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NA_complex_)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NA_real_)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NA_integer_)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(NaN)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(Inf)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(-Inf)))
  expect_error(freq_ICD(rep(0,10), thresh_min = NULL))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(0.1, 0.2)))
  expect_error(freq_ICD(rep(0,10), thresh_min = c(1 + 1i)))
})

test_that("freq_ICD throughs an error in case of non-numerical or vector-valued input of amp_fac", {
  expect_error(freq_ICD(rep(0,10), amp_fac = "A"))
  expect_error(freq_ICD(rep(0,10), amp_fac = TRUE))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NA)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NA_character_)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NA_complex_)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NA_real_)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NA_integer_)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(NaN)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(Inf)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(-Inf)))
  expect_error(freq_ICD(rep(0,10), amp_fac = NULL))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(0.1, 0.2)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(-0.1)))
  expect_error(freq_ICD(rep(0,10), amp_fac = c(1 + 1i)))
})





test_that("freq_ICD throughs an error in case of delta_t_u < delta_t", {
  expect_error(freq_ICD(1:10, delta_t_u = 0.1, delta_t = 1))
})

  test_that("freq_ICD throughs an error in case of non-numerical or vector-valued input of delta_t_u", {
  expect_error(freq_ICD(rep(0,10), delta_t_u = "A"))
  expect_error(freq_ICD(rep(0,10), delta_t_u = TRUE))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NA)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NA_character_)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NA_complex_)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NA_real_)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NA_integer_)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(NaN)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(-Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = NULL))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(0.1, 0.2)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(-0.1)))
  expect_error(freq_ICD(rep(0,10), delta_t_u = c(1 + 1i)))
})

test_that("freq_ICD throughs an error in case of non-numerical or vector-valued input of delta_t_l", {
  expect_error(freq_ICD(rep(0,10), delta_t_l = "A"))
  expect_error(freq_ICD(rep(0,10), delta_t_l = TRUE))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NA)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NA_character_)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NA_complex_)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NA_real_)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NA_integer_)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(NaN)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(-Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = NULL))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(0.1, 0.2)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(-0.1)))
  expect_error(freq_ICD(rep(0,10), delta_t_l = c(1 + 1i)))
})

test_that("freq_ICD throughs an error in case of delta_t_l < delta_t", {
  expect_error(freq_ICD(1:10, delta_t_l = 0.1, delta_t = 1))
})



test_that("freq_ICD throughs an error in case of non-numerical or vector-valued input of delta_t", {
  expect_error(freq_ICD(rep(0,10), delta_t = "A"))
  expect_error(freq_ICD(rep(0,10), delta_t = TRUE))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NA)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NA_character_)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NA_complex_)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NA_real_)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NA_integer_)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(NaN)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(-Inf)))
  expect_error(freq_ICD(rep(0,10), delta_t = NULL))
  expect_error(freq_ICD(rep(0,10), delta_t = c(0.1, 0.2)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(-0.1)))
  expect_error(freq_ICD(rep(0,10), delta_t = c(1 + 1i)))
})


test_that("freq_ICD throughs an error in case of a data set with variance 0", {
  expect_message(freq_ICD(rep(0,10)))
  expect_error(freq_ICD(rep(1,10), delta_t = TRUE))
  expect_error(freq_ICD(rep(1L,10), delta_t = c(NA)))
})



test_that("freq_ICD throughs an error in case of a data set  with all values < thresh_min or all values > thresh_min", {
  expect_message(freq_ICD(runif(10)+1, thresh_min = 0.5))
  expect_message(freq_ICD(runif(10)+1, thresh_min = 2))
})


test_that("freq_ICD returns correct output", {
  expect_equal(freq_ICD(1:10), list(freq_ICD = NA_real_, c_v = NA_real_, no_of_peaks = 0, maxima_sampled = integer(0)))
  expect_equal(freq_ICD(rep(0,10)), list(freq_ICD = NA_real_, c_v = NA_real_, no_of_peaks = 0, maxima_sampled = integer(0)))
  expect_equal(freq_ICD(rep(c(1,-1),5)),  list(freq_ICD = NA_real_, c_v = NA_real_,   no_of_peaks = 0, maxima_sampled = integer(0)))

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * cos(0.05 * 2*pi * (1:100)) + 1* cos(0.20 * 2*pi * (1:100))
  expect_equal(freq_ICD(x1),  list(freq_ICD = 50, c_v = 0, no_of_peaks = 4, maxima_sampled = c(25, 45, 65, 85)))
  expect_equal(freq_ICD(x2, delta_t_u = 0.02, delta_t_l = 0.02, thresh_min = 2),  list(freq_ICD = 50,  c_v = 0, no_of_peaks = 4, maxima_sampled = c( 20, 40, 60, 80) ))
})



