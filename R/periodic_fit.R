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
#' @import broom

#' @export


#input : data frame with year, month, product (temp)
#output: fitting values (95% Confidence Interval) (predict)

periodic_fit <- function(data_in,regression_formula)
{

  # Mutate the data
  fit_data <- data_in %>%
    mutate(fracTime = decimal_date(date)-year(date))

  # First do a linear fit of the long term trend:
  baseline.mod <- lm(regression_formula,data=fit_data)

  # Detrend, making sure we have zero mean:
  my_fit <- augment(baseline.mod) %>%
    mutate(fracTime = fit_data$fracTime)

  # Store the mean of the residual (for later):
  mean_resid <- mean(my_fit$.resid)

  # Now add on columns for the periodic components
  resid_fit_data <- my_fit %>%
    mutate(x = (1-fracTime)*fracTime*(1-2*fracTime),
           y=fracTime^2*(1-fracTime)^2,
           z = 1-6*fracTime^2+12*fracTime^3-6*fracTime^4)


  # Do the fit
  resid.mod <- lm(.resid ~ -1+x+y+z, data = resid_fit_data)
  resid.mod.results <- augment(resid.mod)


  ### Build back up code:
  # The vector of results
  newResults <- predict(baseline.mod, interval = 'confidence', level = 0.95) %>% data.frame() %>%
    add_column(date=fit_data$date,
               time=fit_data$time,
               value = fit_data$value) %>%  # Mutate back up (fitted results in both)
    mutate(fit = fit + resid.mod.results$.fitted)  # Take the baseline fit + fitted results + the mean residual

  n_params <- (coef(baseline.mod) %>% length()) + (coef(resid.mod) %>% length()) + 1 # sample variance is a parameter
  out_stats <- fit_stats(newResults$fit,newResults$value,n_params)



  return(list("prediction"=newResults,"fit"=out_stats))
}
