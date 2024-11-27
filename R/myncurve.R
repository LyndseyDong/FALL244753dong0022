utils::globalVariables("x")
#' @title myncurve
#'
#' @description
#' Visualizes the normal distribution for a given mean and standard deviation and calculates the probability for a specified upper bound.
#'
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound of the shaded region
#'
#' @return A list that has the mean, standard deviation, probability, and a graph of the shaded curve
#' @export
#'
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5, a=6)}
myncurve = function(mu, sigma, a) {
  # Plot the normal distribution curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        ylab = "Density", xlab = "x", main = "Normal Distribution")

  # Shade the area under the curve from -∞ to a
  x_shade <- seq(mu - 3 * sigma, a, length = 1000)
  y_shade <- dnorm(x_shade, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, x_shade, a), c(0, y_shade, 0), col = "lightblue")

  # Calculate P(X <= a)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # Return a list with the mean, sd, and probability
  return(list(mu = mu, sigma = sigma, probability = prob))
}
