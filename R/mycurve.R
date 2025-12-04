#' Draws and shades a normal curve region, returning probability
#'
#' @param mu Mean of the normal distribution
#' @param sigma Standard deviation of the normal distribution
#' @param a Value up to which to shade and calculate P(X ≤ a)
#'
#' @returns A list containing mu, sigma, and the probability
#' @examples
#' mycurve(mu = 10, sigma = 5, a = 6)
#'
#' @export
mycurve <- function(mu, sigma, a) {
  # Draw the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        from = mu - 3*sigma, to = mu + 3*sigma,
        main = expression(P(X <= a) ~ "for" ~ X %~% N(mu, sigma^2)),
        ylab = "Density", xlab = "X",
        col = "blue", lwd = 2)

  # Shade the area from -∞ to a
  x_vals <- seq(mu - 3*sigma, a, length = 1000)
  y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
  polygon(c(mu - 3*sigma, x_vals, a), c(0, y_vals, 0),
          col = "lightblue", border = NA)

  # Calculate the probability (area under curve)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # Display text on the plot
  text(mu, max(y_vals)/2, paste("P(X ≤", a, ") =", round(prob, 4)), col = "black")

  # Return results as a list
  return(list(mu = mu, sigma = sigma, probability = prob))
}

library(MATH4753F2SNISHANT)
mycurve(mu = 10, sigma = 5, a = 6)
