library(testthat)
library(Poisson)
test_that("qpois_custom returns correct quantiles", {
  expect_equal(qpois_custom(0.5, lambda = 3), qpois(0.5, lambda = 3))
  expect_equal(qpois_custom(0.9, lambda = 3, lower.tail = FALSE), qpois(0.9, lambda = 3, lower.tail = FALSE))
})

test_that("qpois_custom handles edge cases", {
  expect_error(qpois_custom(1.5, lambda = 3), "p must be between 0 and 1")
  expect_error(qpois_custom(0.5, lambda = -3), "lambda > 0")
})
