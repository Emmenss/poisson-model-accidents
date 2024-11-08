#' Poisson Distribution: Cumulative Probability
#'
#' Calculates the cumulative probability P(X ≤ q) or P(X > q) for a Poisson distribution.
#'
#' @param q Integer, the upper bound for which the cumulative probability is calculated (must be >= 0).
#' @param lambda Numeric, parameter of the Poisson distribution representing the average rate of events (must be > 0).
#' @param lower.tail Logical; if TRUE, returns P(X ≤ q), otherwise returns P(X > q).
#' @return The cumulative probability as a numeric value.
#'
#' @examples
#' # Probability of having 5 events or fewer with an average of 3
#' ppois_custom(5, 3)
#' # Returns: 0.9160821
#'
#' # Probability of having more than 5 events with an average of 3
#' ppois_custom(5, 3, lower.tail = FALSE)
#' # Returns: 0.08391788
#' @export
ppois_custom <- function(q, lambda, lower.tail = TRUE) {
  if (q < 0 || lambda <= 0) {
    stop("q must be >= 0 and lambda > 0")
  }

  prob <- 0
  for (k in 0:q) {
    prob <- prob + dpois_custom(k, lambda)
  }

  if (!lower.tail) {
    prob <- 1 - prob
  }

  return(prob)
}
