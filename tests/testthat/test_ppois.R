library(testthat)
library(Poisson)
test_that("ppois_custom fonctionne correctement", {
  # Comparer avec la fonction ppois standard
  expect_equal(ppois_custom(3, lambda = 2), ppois(3, lambda = 2))
  expect_equal(ppois_custom(0, lambda = 1), ppois(0, lambda = 1))

  # Vérifier la valeur limite
  expect_equal(ppois_custom(0, lambda = 0), 1)

  # Vérifier que les erreurs sont bien gérées
  expect_error(ppois_custom(-1, lambda = 1)) # x ne peut pas être négatif
  expect_error(ppois_custom(1, lambda = -1)) # lambda ne peut pas être négatif
})
