library(testthat)
library(Poisson)


test_that("dpois_custom fonctionne correctement", {
  # Comparer avec la fonction dpois standard
  expect_equal(dpois_custom(3, lambda = 2), dpois(3, lambda = 2))
  expect_equal(dpois_custom(0, lambda = 1), dpois(0, lambda = 1))

  # Vérifier la valeur limite
  expect_equal(dpois_custom(0, lambda = 0), 1)
  expect_equal(dpois_custom(1, lambda = 0), 0)

  # Vérifier que les erreurs sont bien gérées
  expect_error(dpois_custom(-1, lambda = 1)) # x ne peut pas être négatif
  expect_error(dpois_custom(1, lambda = -1)) # lambda ne peut pas être négatif
})
