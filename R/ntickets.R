#' @title ntickets Function
#'
#' @description
#' Determines the optimal number of tickets to sell for a flight, given the total number of seats and overbooking probability, and the probability of a passenger showing up.
#'
#'
#' @param N Number of seats in the flight
#' @param gamma The probability a plane will be truly overbooked
#' @param p The probability of a "show", meaning someone shows up
#'
#' @return A list containing N, gamma, p, nc (normal approximation), and nd (discrete distribution), where nc and nd is the calculation of the number of tickets to be sold. Creates two plots for the normal approximation and discrete distribution.
#' @export
#'
#' @examples
#' \dontrun{ntickets(400, 0.02, 0.95)}
#'
#' @importFrom stats pbinom
#' @importFrom stats qbinom
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats optimize
#'
#' @importFrom graphics abline
ntickets <- function(N, gamma, p)
{
  # Calculating Discrete Probability

  # Generating sequence of numbers plus 10% (to account for + 20 for 200 and +40 for 400)
  n = seq(N, floor(N + N/10), by = 1)

  # Getting the absolute value of binomial distribution (abs means no negative numbers)
  bd = abs(N - qbinom(1 - gamma, n, p))

  # Getting the minimum value from bd for the optimal index
  minimumValue = which.min(bd)

  # Storing that value in nd, getting it from n
  nd = n[minimumValue]
  nd

  # Discrete case using binomial distribution
  dc = 1 - gamma - pbinom(q = N, size = n, prob = p)

  # Plotting the discrete plot with points
  plot(n, dc, type = "b", pch = 19, col = "blue",
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold \n(", nd, ") gamma = ", gamma, " N = ", N, " discrete"))

  # Add a red horizontal line at the bottom
  abline(h = 0, col = "red", lwd = 1)

  # Add a vertical red line at the optimal number of tickets
  abline(v = nd, col = "red", lwd = 2, lty = 1)


  # Calculating Normal Approximation

  # Getting mean and sd
  mean = n * p
  sd = sqrt(n * p * (1 - p))

  # Continuous Case (probability)
  cc = 1 - gamma - pnorm(N, mean, sd)

  # Normal absolute difference between N and normal approximation
  normalDiff <- function(n)
  {
    abs(N + 0.5 - qnorm(p = 1 - gamma, mean = n * p, sd = sqrt(n * p * (1 - p))))
  }

  # Getting the optimal ticket number for normal approximation
  normIndex <- optimize(normalDiff, interval = c(N, N + N / 10))

  # Getting the optimal number of tickets according to normal approximation
  nc = normIndex$minimum

  # Plotting the normal approximation plot
  plot(n, cc, type = "l", col = "purple",
       xlab = "n",
       ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold \n(", nc, ") gamma = ", gamma, " N = ", N, " continous"))

  # Add a red horizontal line at the bottom
  abline(h = 0, col = "red", lwd = 1)

  # Add a vertical red line at the optimal number of tickets
  abline(v = nc, col = "red", lwd = 2, lty = 1)

  # Making a list of nd, nc, N, gamma, and p
  listToPrint = list(nd = nd, nc = nc , N = N, p = p, gamma = gamma)
  listToPrint
}
