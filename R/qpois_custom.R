#' Poisson Distribution: Quantile
#'
#' Calculates the quantile for a given probability according to a Poisson distribution.
#'
#' @param p Numeric, target probability (must be between 0 and 1).
#' @param lambda Numeric, parameter of the Poisson distribution (must be > 0).
#' @param lower.tail Logical; if TRUE, returns the quantile for P(X ≤ x) ≥ p, otherwise for P(X > x) ≤ p.
#' @return The quantile as an integer.
#'
#' @examples
#' # Quantile for a cumulative probability of 0.8 with lambda = 3
#' qpois_custom(0.8, 3)
#' # Returns: 4
#'
#' # Quantile for a complementary probability of 0.8 with lambda = 3
#' qpois_custom(0.8, 3, lower.tail = FALSE)
#' # Returns: 1
#' @export
qpois_custom <- function(p, lambda, lower.tail = TRUE) {
  if (p < 0 || p > 1 || lambda <= 0) {
    stop("p must be between 0 and 1 and lambda > 0")
  }

  cumulative_prob <- 0
  x <- 0

  if (lower.tail) {
    while (cumulative_prob < p) {
      cumulative_prob <- cumulative_prob + dpois_custom(x, lambda)
      if (cumulative_prob >= p) {
        return(x)
      }
      x <- x + 1
    }
  } else {
    cumulative_prob <- 1
    while (cumulative_prob > p) {
      cumulative_prob <- cumulative_prob - dpois_custom(x, lambda)
      if (cumulative_prob <= p) {
        return(x)
      }
      x <- x + 1
    }
  }

  return(x)
}
