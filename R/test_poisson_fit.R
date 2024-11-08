#' Goodness-of-Fit Test for the Poisson Distribution
#'
#' This function performs a chi-squared test to check if a series of data follows a Poisson distribution with a given lambda parameter.
#'
#' @param data Vector of observed data, representing the number of occurrences of an event in given time intervals.
#' @param lambda Hypothetical parameter of the Poisson distribution (> 0), representing the expected average number of events.
#' @return An object of type `htest` with the results of the chi-squared test, including the test statistic and the p-value.
#' @examples
#' data <- rpois(50, lambda = 3)
#' test_poisson_fit(data, lambda = 3)
#' @export
test_poisson_fit <- function(data, lambda) {
  if (any(data < 0) || lambda <= 0) {
    stop("Data must be >= 0 and lambda > 0")
  }

  obs_freq <- table(factor(data, levels = 0:max(data)))
  obs_freq <- as.numeric(obs_freq)

  expected_freq <- sapply(0:max(data), function(x) length(data) * dpois_custom(x, lambda))

  chisq_test <- chisq.test(obs_freq, p = expected_freq, rescale.p = TRUE)
  return(chisq_test)
}
