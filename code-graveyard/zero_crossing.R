#' Compute zero crossing points for a given dataset
#'
#' \code{zero_crossing} Returns fitted and predicted values of periodically repeating data using sine and cosine functions.
#'
#' @param data_in a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param regression_formula a formula for the regression that we are computing for the baseline fit
#'
#' @return A data frame that reports the fitted zero crossing value of the trigonometric and the periodic fit
#'
#' @examples
#'
#' # To be filled in later





#input : data frame with year, month, product (temp)
#output: fitting values (95% Confidence Interval) (predict)



# We need to input in the baseline trend function, and the dataset.
# What this will do is detrend it and then compare and then estimate the zero crossing point for the trig and periodic functions


zero_crossing <- function(data_in,regression_formula) {


  # Fit data using a periodic approach:
  data_results <- periodic_fit(data_in,regression_formula)

  # Fit data using a trig approach:
  data_results_trig <- trigonometric_fit(data_in,regression_formula)

  # For the periodic it is the positive of the quadratic piece
  m <- tail(coef(data_results$fit),n=2)[1]
  beta <- tail(coef(data_results$fit),n=2)[2]

  zc <- max((beta - 2*m + sqrt(beta^2 + 4*m^2))/(2*beta),(beta - 2*m - sqrt(beta^2 + 4*m^2))/(2*beta))


  a1 <- tail(coef(data_results_trig$fit),n=2)[1]
  a2 <- tail(coef(data_results_trig$fit),n=2)[2]
  A <- sqrt(a1^2+a2^2)
  theta <- atan(a2/a1)
  zc_trig<- (2*pi-acos(-a1/A)+theta)/(2*pi)

  zero_crossing <- data.frame(value=c(zc,zc_trig),type=c("periodic","trig"))

  return(zero_crossing)
}
