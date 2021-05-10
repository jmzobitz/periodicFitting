#' Compute a baseline and piecewise linear fit to data.
#'
#' \code{piecewise_linear_fit_rev} Returns fitted and predicted values of periodically repeating data.  This code first subtracts away the residual and does a segmented fit (segmented package)
#'
#' @param data_in a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param regression_formula a formula for the regression that we are computing for the baseline fit
#'
#' @return A list of two things: the fitted values at a 95% confidence level and the predicted value, and an object of class "lm" for analysis.
#'
#' @examples
#'
#' @import segmented
#' @import broom
#'


piecewise_linear_fit_rev <- function(data_in,regression_formula)
{

  # Mutate the data
  fit_data <- data_in %>%
    mutate(fracTime = decimal_date(date)-year(date))

  # First do a linear fit of the long term trend:
  baseline.mod <- lm(regression_formula,data=fit_data)

  # Detrend, making sure we have zero mean:
  my_fit <- augment(baseline.mod) %>%
    mutate(fracTime = fit_data$fracTime,
           .resid.zm = .resid - mean(.resid))

  # Store the mean of the residual (for later):
  mean_resid <- mean(my_fit$.resid)

  # Start the segmented fit:
  lin.resid <- lm(.resid~1+fracTime,data=my_fit)

  # Then apply the segmented routine:
  my.seg <- segmented::segmented(lin.resid,
                                 seg.Z = ~ fracTime,
                                 npsi = 2)

  my.seg.fitted <- predict(my.seg, interval = 'confidence', level = 0.95) %>% data.frame()

  print(slope(my.seg))
  ### Build back up code:
  # The vector of results
  newResults <- predict(baseline.mod, interval = 'confidence', level = 0.95) %>% data.frame() %>%
    add_column(date=fit_data$date,
               time=fit_data$time,
               value = fit_data$value) %>%
    mutate(fit = fit + my.seg.fitted$fit ) # Mutate back up (fitted results in both)

  n_params <- (coef(baseline.mod) %>% length()) + (coef(my.seg) %>% length()) + 1 # sample variance is a parameter
  out_stats <- fit_stats(newResults$fit,newResults$value,n_params)



  return(list("prediction"=newResults,"fit"=out_stats))
}
