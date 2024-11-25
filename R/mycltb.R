#' Central Limit Theorem - Binomial
#'
#' @param n  Defines how many trials are performed in each binomial sample.
#' @param iter The number of samples that the function will generate.
#' @param p The probability of success in each trial (default value is 0.5)
#' @param ... Optional additional arguments that is passed to hist()
#'
#' @return Outputs a histogram plot showing the distribution with a theoretical normal curve
#' @export
#'
#' @examples
#' \dontrun{mycltb(n = 10, iter = 10000, p = 0.3)}
mycltb = function(n, iter, p = 0.5, ...)
{
  ## r-random sample from the Binomial
  y =rbinom(n * iter, size = n, prob = p)

  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)

  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w = apply(data, 2, mean)

  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param = hist(w, plot = FALSE)

  ## Since the histogram will be a density plot we will find the max density
  ymax = max(param$density)

  ## To be on the safe side we will add 10% more to this
  ymax = 1.1 * ymax

  ## Now we can make the histogram
  ## freq=FALSE means take a density
  hist(w, freq = FALSE, ylim = c(0, ymax),
       main = paste("Histogram of sample mean", "\n", "sample size= ", n, sep = ""),
       xlab = "Sample mean", ...)

  ## add a density curve made from the sample distribution
  # lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  curve(dnorm(x, mean = n * p, sd = sqrt(p * (1 - p))), add = TRUE, col = "Red", lty = 2, lwd = 3)
}
