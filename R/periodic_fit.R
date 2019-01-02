#' Compute a baseline and periodic fit to data.
#'
#' \code{periodic_fit} Returns fitted and predicted values of periodically repeating data.
#'
#' @param data_in a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param regression_formula a formula for the regression that we are computing for the baseline fit
#'
#' @return A list of two things: the fitted values at a 95% confidence level and the predicted value, and an object of class "lm" for analysis.
#'
#' @examples
#'
#' # To be filled in later

#' @import dplyr
#' @import lubridate

#' @export


#input : data frame with year, month, product (temp)
#output: fitting values (95% Confidence Interval) (predict)

periodic_fit <- function(data_in,regression_formula)
{

  # rename what we are fitting to product so we have the correct values
  fit_data <- data_in %>%
    mutate(fracTime = decimal_date(date)-year(date),
                                x=fracTime*(1-2*fracTime)*(1-fracTime),
                                y=fracTime^2*(1-fracTime)^2)



  regressionFormula <- update(regression_formula, ~. + x + y)

  fitResults <- lm(regressionFormula, data = fit_data)
  modelResults=data.frame("AIC"=AIC(fitResults),"BIC"=BIC(fitResults))
  newResults <- predict(fitResults, interval = 'confidence', level = 0.95) %>% data.frame()
  return(list("prediction"=newResults,"fit"=fitResults))
}
