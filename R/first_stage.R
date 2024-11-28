#' First Stage Regression
#'
#' Performs the first stage of two-stage least squares regression in the VAR context.
#'
#' @param x A VAR model object (class "varest").
#' @param instrument A character string for the instrument variable.
#' @param instrumented A character string for the instrumented variable.
#' @return A numeric vector of fitted values from the first-stage regression.
#' @export
first_stage <- function (x, instrument, instrumented){
  variables <- colnames(x$datamat)[1:x$K]
  p <- x$p
  instrument <- data[[instrument]]
  res <- c(rep(NA, times = p+1), residuals(x)[,instrumented])
  res_model <- lm(as.numeric(res) ~ as.numeric(instrument))
  res_model_hat <- c(rep(NA, times = p + 1), res_model$fitted.values)
  return(res_model_hat)
}

