test_that("freq_threshold_crossing throughs an error in case of non-numerical input array x", {
  expect_error(freq_threshold_crossing(c(TRUE)))
  expect_error(freq_threshold_crossing(c(FALSE)))

  expect_error(freq_threshold_crossing(c("A")))
  expect_error(freq_threshold_crossing(c("ABC")))

  expect_error(freq_threshold_crossing(c(NA)))
  expect_error(freq_threshold_crossing(c(NA_character_)))
  expect_error(freq_threshold_crossing(c(NA_complex_)))
  expect_error(freq_threshold_crossing(c(NA_real_)))
  expect_error(freq_threshold_crossing(c(NA_integer_)))
  expect_error(freq_threshold_crossing(c(NaN)))

  expect_error(freq_threshold_crossing(c(Inf)))
  expect_error(freq_threshold_crossing(c(-Inf)))
})


test_that("freq_threshold_crossing returns an array of length 4", {
  expect_length(freq_threshold_crossing(rep(c(-1,1),5)), 4)
  expect_length(freq_threshold_crossing(1:11), 4)
  expect_length(freq_threshold_crossing(1:10), 4)
  expect_length(freq_threshold_crossing(rep(0,10)), 4)
  expect_error(freq_threshold_crossing(NULL))
})

test_that("freq_threshold_crossing returns correct output", {
  expect_equal(freq_threshold_crossing(1:10), c(NA_real_, NA_real_, NA_real_, NA_real_), ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(0,10)), c(NA_real_, NA_real_, NA_real_, NA_real_), ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5)),  c(0.5, 0, 0, 0),ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5), thresh = 0.5),  c(0.5, 0, 0, 0.5),ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(1,-1),5), thresh = 0.5, delta_t = 2),  c(0.25, 0, 0, 0.5),ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5)),  c(0.1, 0, 0, 0.08), ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(-1, 0.1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 0.1), 5),  delta_t = 2),  c(0.05, 0, 0, 0.08), ignore_attr = TRUE)
  expect_equal(freq_threshold_crossing(rep(c(-1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 5),  thresh = 0.1),  c(0.1, 0, 0, 0.1), ignore_attr = TRUE)

})

