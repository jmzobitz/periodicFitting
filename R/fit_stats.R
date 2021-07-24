#' Compute model data statistics.
#'
#' \code{fit_stats} Computes the following model - data statistics
#' \itemize{
#'   \item AIC
#'   \item R squared value
#'   \item Centered root mean square value
#'   }
#'
#' @param model The modeled values
#' @param observations The observed values.  Must be the same size as model
#'
#' @param n_params the number of parameters
#' @return A data frame AIC, R2, and centered root mean square values
#'
#' @import dplyr
#' @export
#'


fit_stats <- function(model,observations,n_params) {

  n_obs <- length(observations)

  # https://stats.stackexchange.com/questions/87345/calculating-aic-by-hand-in-r

  ll <- -n_obs*(log(2*pi)+1+log((sum((model-observations)^2)/n_obs)))/2
  AIC <- -2*ll + 2*n_params

  #BIC <- -2*ll + log(n_obs)*n_params
  r.squared <- (cor(model,observations))^2

  centered_rms=sd( (observations-mean(observations))-((model-mean(model))))/sd(observations)

  fit_stats <- tibble(r.squared = r.squared,
                      AIC=AIC,
                      centered_rms = centered_rms,
                      ll = ll)

  return(fit_stats)





}
