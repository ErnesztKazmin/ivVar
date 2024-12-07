#' First Stage Regression
#'
#' Performs the first stage of two-stage least squares regression in the VAR context.
#'
#' @param x A VAR model object (class "varest").
#' @param instrument A vector representing the instrument variable values.
#' @param instrumented A character string for the instrumented variable.
#' @return A numeric vector of fitted values from the first-stage regression.
#' @export
first_stage <- function (x, instrument, instrumented){
  variables <- colnames(x$datamat)[1:x$K]
  p <- x$p
  res <- c(rep(NA, times = p), residuals(x)[,instrumented])
  res_model <- lm(res ~ instrument)
  res_model_hat <- c(rep(NA, times = p ), res_model$fitted.values)
  white <- lmtest::coeftest(res_model, vcov = vcovHC(res_model, type = "HC0"))
  final <- list(res_model_hat, white)
  return(final)
}

