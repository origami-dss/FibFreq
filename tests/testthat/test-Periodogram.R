

test_that("Periodogram throws an error in case of non-numerical input array x", {
  expect_error(Periodogram(c(TRUE)))
  expect_error(Periodogram(c(FALSE)))

  expect_error(Periodogram(c("A")))
  expect_error(Periodogram(c("ABC")))

  expect_error(Periodogram(c(NA)))
  expect_error(Periodogram(c(NA_character_)))
  expect_error(Periodogram(c(NA_complex_)))
  expect_error(Periodogram(c(NA_real_)))
  expect_error(Periodogram(c(NA_integer_)))
  expect_error(Periodogram(c(NaN)))

  expect_error(Periodogram(c(Inf)))
  expect_error(Periodogram(c(-Inf)))
})

test_that("Periodogram throws an error in case of non-numerical or vector-valued value of delta_t", {
  expect_error(Periodogram(c(1:10), delta_t = c(1,2)))
  expect_error(Periodogram(c(1:10), delta_t = "A"))
  expect_error(Periodogram(c(1:10), delta_t = 1i))

  expect_error(Periodogram(c(1:10), delta_t = Inf))
  expect_error(Periodogram(c(1:10), delta_t = -Inf))

  expect_error(Periodogram(c(1:10), delta_t = NA))
  expect_error(Periodogram(c(1:10), delta_t = NA_character_))
  expect_error(Periodogram(c(1:10), delta_t = NA_complex_))
  expect_error(Periodogram(c(1:10), delta_t = NA_real_))
  expect_error(Periodogram(c(1:10), delta_t = NA_integer_))
  expect_error(Periodogram(c(1:10), delta_t = NaN))

  expect_error(Periodogram(c(1:10), delta_t = 0))

  expect_error(Periodogram(c(1:10), delta_t = -1 ))
})

test_that("Periodogram throws an error in case of non-logical value or vector-valued welch_window", {
  expect_error(Periodogram(c(1:10), welch_window = 1))
  expect_error(Periodogram(c(1:10), welch_window = 1.))
  expect_error(Periodogram(c(1:10), welch_window  = "A"))
  expect_error(Periodogram(c(1:10), welch_window  = 1i))

  expect_error(Periodogram(c(1:10), welch_window  = Inf))
  expect_error(Periodogram(c(1:10), welch_window  = -Inf))

  expect_error(Periodogram(c(1:10), welch_window  = NA))
  expect_error(Periodogram(c(1:10), welch_window  = NA_character_))
  expect_error(Periodogram(c(1:10), welch_window  = NA_complex_))
  expect_error(Periodogram(c(1:10), welch_window  = NA_real_))
  expect_error(Periodogram(c(1:10), welch_window  = NA_integer_))
  expect_error(Periodogram(c(1:10), welch_window  = NaN))

  expect_error(Periodogram(c(1:10), welch_window = c(TRUE, TRUE)))
  expect_error(Periodogram(NULL))
})


test_that("Periodogram returns an array of dim (length(input/2),2 )", {
  expect_length(Periodogram(1:10)[[1]], 5)
  expect_length(Periodogram(1:11)[[1]], 5)
  expect_length(Periodogram(1:10), 2)
})

test_that("Periodogram returns correct output", {
  expect_equal(Periodogram(1:10)[[1]],  1:5/10)
  expect_equal(Periodogram(rep(0,10))[[2]],  rep(0,5))
  expect_equal(Periodogram(rep(c(1,-1),5))[[2]],  c(0,0,0,0,20))
})
