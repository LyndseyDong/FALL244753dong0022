#' @title Confidence Interval Function
#'
#' @description
#' Calculates a confidence interval for the mean of the dataset "x" based on a specified significance level.
#'
#'
#' @param x Random data that you want to create a confidence interval for
#' @param alpha The alpha value of the function
#'
#' @return A confidence interval with the given data and alpha value
#' @export
#'
#' @examples
#' \dontrun{myci(x, alpha = 0.05)}
myci <- function(x, alpha = 0.05) {
  # Check if the input is numeric
  if (!is.numeric(x)) {
    stop("Input sample x must be numeric.")
  }

  # Calculate sample statistics
  n <- length(x)  # Sample size
  x_bar <- mean(x)  # Sample mean
  s <- sd(x)  # Sample standard deviation

  # Calculate the margin of error using the t-distribution
  t_critical <- qt(1 - alpha/2, df = n - 1)  # t-value for the specified confidence level
  margin_of_error <- t_critical * (s / sqrt(n))

  # Calculate the confidence interval
  lower_bound <- x_bar - margin_of_error
  upper_bound <- x_bar + margin_of_error

  # Return the confidence interval as a named vector
  return(c(lower = lower_bound, upper = upper_bound))
}
