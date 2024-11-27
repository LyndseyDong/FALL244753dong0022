#' @title Max Likelihood
#'
#' @description
#' Calculates the maximum likelihood estimate for the given likelihood function and dataset.
#'
#'
#' @param lfun The function to focus on
#' @param x The data being passed to the function
#' @param param The range (vector) of parameter values for the likelihood
#' @param ... Optional parameters
#'
#' @return The max likelihood for the provided parameters in a list and plots the graph
#' @export
#'
#' @examples
#' \dontrun{mymaxlike(x = c(4, 6, 7, 6, 5), param = seq(0, 20, length = 1000), lfun = logpoiss, xlab = expression(lambda), main = "Poisson", cex.main = 2)}
mymaxlike = function(lfun, x, param, ...)
{
  # How many param values are there
  np = length(param)

  # Outer function that produces a matrix
  z = outer(x, param, lfun) #A
  # z is a matrix where each x,param is replaced with the function evaluated at those values

  y = apply(z, 2, sum)
  # y is a vector made up of the column sums
  # Each y is the log likelihood for a new parameter value
  plot(param, y, col = "Blue", type = "l", lwd = 2, ...)
  # which gives the index for the value of y >= max
  # there could be a max between two values of the parameter, therefore 2 indices
  # The first max will take the larger indice
  i = max(which(y == max(y))) #B

  abline(v = param[i], lwd = 2, col = "Red")

  # Plots a nice point where the max likelihood is
  points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")

  axis(3, param[i], round(param[i], 2))

  # Check slopes. If it is a max the slope should change sign from + to -
  # We should get three + and two -vs

  ifelse(i - 3 >= 1 & i + 2 <= np,
         slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]), slope<-"NA")

  return(list(i=i,parami=param[i],yi=y[i],slope=slope))

}
