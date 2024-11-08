#' Poisson Distribution: Probability Density
#'
#' Computes the probability P(X = x) for a random variable X that follows a Poisson distribution with a given rate parameter lambda.
#'
#' @param x Integer, the observed number of events for which the probability is calculated (must be >= 0).
#' @param lambda Numeric, the rate parameter of the Poisson distribution representing the average rate of events (must be > 0).
#' @return The probability P(X = x) as a numeric value.
#'
#' @examples
#' # Probability of observing exactly 5 events with an average rate of 3
#' dpois_custom(5, 3)
#' # Returns: 0.1008188
#' @export
dpois_custom <- function(x, lambda) {
  if (x < 0 || lambda <= 0) {
    stop("x must be >= 0 and lambda > 0")
  }
  (lambda^x * exp(-lambda)) / factorial(x)
}
