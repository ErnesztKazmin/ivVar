#' Second Stage Regression
#'
#' Performs the second stage of two-stage least squares regression in the VAR context.
#'
#' @param x A VAR model object (class "varest").
#' @param res_model_hat A numeric vector of fitted values from the first stage.
#' @param instrumented A character string for the instrumented variable.
#' @return A data frame with coefficients from the second-stage regression.
#' @export
second_stage <- function(x, fs$res_model_hat, instrumented){
  p <- x$p
  residuals <- as.data.frame(residuals(x))
  models <- lapply(residuals, function(i) lm(i ~ fs$res_model_hat[-p:-1]))
  coefs <- as.data.frame(lapply(models, function(i) coef(i)[2]))
  return(`row.names<-`(coefs, "Coefficients"))
}
