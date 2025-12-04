#' @title Compute adjusted R-squared from an lm object
#'
#' @param model An object of class lm
#' @return A single numeric value: adjusted R-squared
#' @export
#' @examples
#' fit <- lm(mpg ~ hp + wt, data = mtcars)
#' adj_r2(fit)
adj_r2 <- function(model) {
  stopifnot(inherits(model, "lm"))
  summary(model)$adj.r.squared
}
