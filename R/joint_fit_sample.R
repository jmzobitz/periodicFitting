#' Compute a baseline and periodic fit to data.
#'
#' \code{joint_fit_sample} Is an overarching analysis function that does a lot of heavy lifting:
#' \itemize{
#'   \item Computes a periodic and trigonometric fit through random thinning of the data sets(using periodic_fit_sample and trigonometric_fit_sample)
#'   \item Rolls up the results into a data frame for plotting.
#' }Returns a list of fitted and predicted values of periodically repeating data.  This works over a list of data sites efficiently processing multiple locations
#'
#' @param data a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param formula The specific regression formula you want to use.
#'
#' @return A data frame of fitted values than we can use for plotting
#'
#' @examples
#'
#' # This really is an overarching function that does the master fitting, since we are doing a lot of things repeatedly this seemed to be the easiest to avoid errors!
#'
#' @import dplyr
#' @export




joint_fit_sample <- function(data,formula) {

  sample_fraction = seq(0.1,0.9,by=0.1)

  models_val_periodic<-map_df(sample_fraction,~periodic_fit_sample(data,formula,.x)$prediction) %>% mutate(approach='periodic')

  models_val_trig <- map_df(sample_fraction,~periodic_fit_sample(data,formula,.x)$prediction) %>% mutate(approach='trigonometric')

  prediction <- rbind(models_val_periodic,models_val_trig)

  # Now compute the values needed to include on a Taylor plot, separated by approach:
  # Compute the necessary stuff to do the Taylor plots
  model_rsq <- prediction %>%
    group_by(approach,percentage) %>%
    summarize(sd_meas=1,
              sd_model=sd(fit)/sd(value),
              r=cor(fit,value),
              centered_rms=sd((value-mean(value))-((fit-mean(fit))))/sd(value)
    ) %>%
    mutate(x_coord = sd_model*r, y_coord = sd_model*sin(acos(r)))

  return(list("taylor" = model_rsq,"prediction"=prediction))



}
