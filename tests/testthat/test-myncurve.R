test_that("myncurve() returns correct mu", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$mu, 0)  # Check if mu is correct
})

test_that("myncurve() returns correct sigma", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$sigma, 1)  # Check if sigma is correct
})

test_that("myncurve() returns correct probability", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expected_prob <- pnorm(1, mean = 0, sd = 1)
  expect_equal(result$probability, expected_prob)  # Check probability
})
