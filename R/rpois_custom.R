#' Poisson Distribution: Random Generation
#'
#' Generates random values following a Poisson distribution for a given lambda parameter.
#'
#' @param n Integer, number of random values to generate (must be > 0).
#' @param lambda Numeric, parameter of the Poisson distribution (must be > 0).
#' @return A vector of n integer values.
#'
#' @examples
#' # Generates 10 values following a Poisson distribution with lambda = 3
#' rpois_custom(10, 3)
#' # Example output: 3, 2, 1, 5, 4, 2, 3, 3, 4, 1
#' @export
rpois_custom <- function(n, lambda) {
  if (n <= 0 || lambda <= 0) {
    stop("n must be > 0 and lambda > 0")
  }

  result <- numeric(n)

  for (i in 1:n) {
    u <- runif(1)
    x <- 0
    cumulative_prob <- dpois_custom(x, lambda)

    while (u > cumulative_prob) {
      x <- x + 1
      cumulative_prob <- cumulative_prob + dpois_custom(x, lambda)
    }

    result[i] <- x
  }

  return(result)
}
