#' Compute a baseline and piecewise linear fit to data.
#'
#' \code{piecewise_linear_fit} Returns fitted and predicted values of periodically repeating data.  This code first subtracts away the residual and does a segmented fit (segmented package)
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
#' @export

piecewise_linear_fit <- function(data_in,regression_formula)
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

  # Start the segmented fit:
  lin.resid <- lm(.resid~1+fracTime,data=my_fit)

  # Then apply the segmented routine:
  my.seg <- segmented::segmented(lin.resid,
                                 seg.Z = ~ fracTime,
                                 npsi = 2)

  ### Build back up code:
  # The vector of results
  newResults <- predict(baseline.mod, interval = 'confidence', level = 0.95) %>% data.frame() %>%
    add_column(date=fit_data$date,
               time=fit_data$time,
               value = fit_data$value)  # Mutate back up (fitted results in both)

# Catch if we don't estimate any breakpoints
  if (!is.null(my.seg$psi) ) {

    # Pull out the estimated coefficients
    breakpoints <- my.seg$psi %>% as_tibble() %>% select(`Est.`) %>%
      rename(value=`Est.`) %>%
      mutate(name = c("b1","b2")) %>%
      rbind(tibble(value =coef(my.seg)[2],name="m" ))


    # Name them so they correspond to our model
    b1 <- breakpoints %>% filter(name=='b1') %>% pull(value)
    b2 <- breakpoints %>% filter(name=='b2') %>% pull(value)

    # Add in spurious variables
    seg_my_fit_data <- my_fit %>%
      mutate(x1 = if_else(fracTime < b1,1,0),
             x2 = if_else(between(fracTime,b1,b2),1,0),
             x3 = if_else(fracTime > b2,1,0),
             c_var = x1+x2+x3,
             m_var = fracTime*x1+b1*x2+(b2-b1-1)*(fracTime-b1)*x2/(b2-b1) + (fracTime-1)*x3)

    # Now do th linear fit
    seg_lin_fit <- lm(.resid~-1+c_var+m_var,data=seg_my_fit_data)





    #Build things up with our model
    out_vec <- augment(seg_lin_fit)

    # Now build the vector back up:
    newResults <- newResults %>%
      mutate(fit = fit + out_vec$.fitted)

   # Compute the stats.  For the parameters we have the coefficients from the baseline model + b1, m, m1:

    n_params <- (coef(baseline.mod) %>% length()) + (4) + 1 # sample variance is a parameter
    out_stats <- fit_stats(newResults$fit,newResults$value,n_params)



  } else {

    out_stats <- tibble(r.squared = NA,
                        AIC=NA,
                        centered_rms = NA)

  }





  return(list("prediction"=newResults,"fit"=out_stats))
}
