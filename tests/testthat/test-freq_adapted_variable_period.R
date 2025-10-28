test_that("freq_adapted_variable_period throws an error in case of non-numerical input array x", {
  expect_error(freq_adapted_variable_period(rep(TRUE,3)))
  expect_error(freq_adapted_variable_period(rep(FALSE,4)))

  expect_error(freq_adapted_variable_period(rep("A",7)))
  expect_error(freq_adapted_variable_period(rep("ABC",6)))

  expect_error(freq_adapted_variable_period(rep(NA,5)))
  expect_error(freq_adapted_variable_period(rep(NA_character_,2)))
  expect_error(freq_adapted_variable_period(rep(NA_complex_,3)))
  expect_error(freq_adapted_variable_period(rep(NA_real_,4)))
  expect_error(freq_adapted_variable_period(rep(NA_integer_,6)))
  expect_error(freq_adapted_variable_period(rep(NaN,4)))

  expect_error(freq_adapted_variable_period(rep(Inf,3)))
  expect_error(freq_adapted_variable_period(rep(-Inf,3)))
  expect_error(freq_adapted_variable_period(NULL))
})

test_that("freq_adapted_variable_period throws an error in case of non-numerical or vector-valued value of delta_t", {
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = c(1,2)))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = "A"))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = 1i))

  expect_error(freq_adapted_variable_period(c(1:10), delta_t = Inf))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = -Inf))

  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NA))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NA_character_))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NA_complex_))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NA_real_))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NA_integer_))
  expect_error(freq_adapted_variable_period(c(1:10), delta_t = NaN))

  expect_error(freq_adapted_variable_period(c(1:10), delta_t = 0))

  expect_error(freq_adapted_variable_period(c(1:10), delta_t = -1 ))
  expect_error(freq_adapted_variable_period(NULL))

  })



test_that("freq_adapted_variable_period returns an array of length 4)", {
  expect_length(freq_adapted_variable_period(1:10), 4)
  expect_length(freq_adapted_variable_period(1:11), 4)
  expect_length(freq_adapted_variable_period(rep(0,10)), 4)
})

test_that("freq_adapted_variable_period returns correct output", {

  expect_equal(freq_adapted_variable_period(rep(c(1,-1),5))[[1]], 0.5)

  x1 <- sin(0.05 * 2*pi * (1:100))
  x2 <- 3 * sin(0.05 * 2*pi * (1:100)) + 1* sin(0.20 * 2*pi * (1:100))

  expect_equal(freq_adapted_variable_period(x1),  list(freq_AVP = 0.05, Max_AVP = 50, ts_length_considered = 100, expl_var = 1.0))

  expect_equal(freq_adapted_variable_period(x2),  list(freq_AVP = 0.05, Max_AVP =  451.23,  ts_length_considered = 100, expl_var =  0.9), tolerance = 0.05)
})
