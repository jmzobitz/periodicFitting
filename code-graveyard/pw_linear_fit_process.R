#' Compute a baseline and periodic fit to data.
#'
#' \code{pw_linear_fit_process} Returns a list of fitted and predicted values of periodically repeating data using a piecewise linear function.  This works over a list of data sites efficiently processing multiple locations
#'
#' @param data a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param formulas a list of formulas for the regression that we are computing for the baseline fit
#'
#' @return A list of two things: the fitted values at a 95% confidence level and the predicted value, and an object of class "lm" for analysis.
#'
#' @examples
#'
#' # To be filled in later

#' @import dplyr
#' @import purrr
#' @import broom



pw_linear_fit_process <- function(data,formulas) {



  # Do all possible combinations of data and formulas
  over_list <- cross2(data,formulas)

  # Now we need to name all of these:
  over_names <- cross2(names(data),names(formulas)) %>%
    map(lift(paste)) %>% unlist() %>% str_split(pattern= " ",simplify = TRUE) %>% data.frame() %>%
    rename(site=X1,model=X2)



  # Compute the model fit
  models_val<-invoke_map(over_list,.f=function(x,y){ piecewise_linear_fit(x,y)$fit })

  # Compute the AIC over each model
  model_stats_vals <- models_val %>% map_df(glance) %>%
    select(r.squared,df,logLik,AIC,BIC)

  model_stats <- over_names %>% cbind(model_stats_vals) %>%
    arrange(site)



  # We need to be careful because we have coefficients that aren't used, but there is a lot of commonality we can extract here

  # Compute the model fit
  fit_results_val<-invoke_map(over_list,.f=function(x,y){ piecewise_linear_fit(x,y)$prediction })

  # OK: next step is to see how we can get the fit and the names bound to each one

  # Because we did a segmented fit, we should compute the R2 and the AIC, BIC manually

  ll <- dnorm(x=fit_results_val$fit,mean=fit_results$value,sd=sd(fit_results$value),log=TRUE)
  n_params <- formula.tools::rhs.vars(formulas) %>% length() + 2
  n_obs <- dim(fit_results)[1]
  AIC <- -2*ll + 2*n_params
  BIC <- -2*ll + ln(n_obs)*n_params
  r.squared <- (cor(fit_results_val$fit,fit_results$value))^2
  df <- n_obs - n_params

  # This is a hacky cheat, but I can't get pmap to work so it mutates the model and product simulataneously - also adds in the date and the time for everything

  results_df <- fit_results_val %>%
    map2(over_names$site,~mutate(.x,site=.y)) %>%
    map2(over_names$model,~mutate(.x,model=.y)) %>% bind_rows() %>%
    mutate(site=as.character(site),model=as.character(model))


  # Collect everything up in a list
  results <- list(models=models_val,model_stats=model_stats,fitted=results_df)



  return(results)

}

