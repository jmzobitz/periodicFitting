#' Compute a baseline and periodic fit to data.
#'
#' \code{trigonometric_fit_sample} Returns fitted and predicted values of periodically repeating data where a certain percentage of the data have been excluded in the fit.
#'
#' @param data_in a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param regression_formula a formula for the regression that we are computing for the baseline fit
#' @param percentage the percentage of the data we are excluding in the fit (via random selection).  Must be between 0 and 1
#'
#' @return A list of two things: the fitted values at a 95% confidence level and the predicted value of data not used in the fitting routine, and an object of class "lm" for analysis.
#'
#' @examples
#'
#' # To be filled in later

#' @import dplyr
#' @import lubridate

#' @export


#input : data frame with year, month, product (temp)
#output: fitting values (95% Confidence Interval) (predict)

trigonometric_fit_sample <- function(data_in,regression_formula,percentage)
{

  # rename what we are fitting to product so we have the correct values
  fit_data <- data_in %>%
    mutate(fracTime = decimal_date(date)-year(date),
           x=cos(2*pi*fracTime),
           y=sin(2*pi*fracTime))

  # Separate out a validation and corroboration dataset
  fit_data_validation <- fit_data %>%
    add_column(index=1:dim(data_in)[1]) %>% # Add a column so we can track the indices
    sample_frac(percentage)

  # Separate out the data we use to corroborate fit
  fit_data_corroboration <- fit_data %>%
    add_column(index=1:dim(data_in)[1]) %>%
    filter(!(index %in% fit_data_validation$index))

  regressionFormula <- update(regression_formula, ~. + x + y)

  fitResults <- lm(regressionFormula, data = fit_data_validation)


  # Compute the confidence interval, and also add in the data to the fitted frame

  newResults <- predict(fitResults, fit_data_corroboration,interval = 'confidence', level = 0.95) %>% data.frame() %>%
    add_column(date=fit_data_corroboration$date,
               time=fit_data_corroboration$time,
               value = fit_data_corroboration$value)

  return(list("prediction"=newResults,"fit"=fitResults))
}
