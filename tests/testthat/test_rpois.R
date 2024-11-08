library(testthat)
library(Poisson)
test_that("rpois_custom generates correct length of random numbers", {
  result <- rpois_custom(5, lambda = 3)
  expect_length(result, 5)
  expect_true(all(result >= 0))  # Les valeurs doivent être positives
})

test_that("rpois_custom handles edge cases", {
  expect_error(rpois_custom(-1, lambda = 3), "n must be > 0")
  expect_error(rpois_custom(5, lambda = -3), "lambda > 0")
})
