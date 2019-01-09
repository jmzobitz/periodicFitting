#' Compute a baseline and periodic fit to data.
#'
#' \code{joint_fit_process} Is an overarching analysis function that does a lot of heavy lifting:
#' \itemize{
#'   \item Computes a periodic and trigonometric fit (using periodic_fit_process and trigonometric_fit_process)
#'   \item Analyzes the two fits and excludes out any fits where the the degrees of freedom is less than expected
#'   \item Writes a data table of AIC and BIC values for analysis.
#'   \item Returns fitted values for the periodic and trigonometric fits for plotting.
#' }Returns a list of fitted and predicted values of periodically repeating data.  This works over a list of data sites efficiently processing multiple locations
#'
#' @param data a data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param model_stats_filename where you want the model stats file to be stored
#'
#' @return A data frame of fitted values than we can use for plotting
#'
#' @examples
#'
#' # This really is an overarching function that does the master fitting, since we are doing a lot of things repeatedly this seemed to be the easiest to avoid errors!
#'
#' @import dplyr
#' @export




joint_fit_process <- function(data,model_stats_filename) {

  # Identify the formulas we are using
  formulas <- baseline_formulas$formulas

  formula_info <- baseline_formulas$formula_info


  # Fit data using a periodic approach:
  data_results <- periodic_fit_process(data,formulas)

  # Fit data using a trig approach:
  data_results_trig <- trigonometric_fit_process(data,formulas)


  # Return out the results of the periodic fit and the trig where we have consistent estimates:

  model_stats <- data_results$model_stats %>%
    left_join(formula_info,by=c("model"="name")) %>%
    filter(df.x==(df.y+2)) %>%
    select(-df.y) %>%
    add_column(approach="periodic")


  model_stats_trig <- data_results_trig$model_stats %>%
    left_join(formula_info,by=c("model"="name")) %>%
    filter(df.x==(df.y+2)) %>%
    select(-df.y) %>%
    add_column(approach="trigonometric")

  model_stats <- rbind(model_stats,model_stats_trig) %>%
    arrange(site,approach,AIC) %>%
    mutate(r.squared=round(r.squared,digits=2),
           logLik = round(logLik,digits=2),
           AIC=round(AIC,digits=2),
           BIC=round(BIC,digits=2)) %>%
    select(site,approach,model,r.squared,AIC)



  ### Save a table we could format
  write.table(model_stats, file = model_stats_filename, sep = " & ", eol= " \\\\\r",quote=FALSE)

  # Make a plot where the trigonometric and periodic are fit side by side in a facetted plot

  data_fits <- rbind(
    add_column(data_results$fitted,approach="periodic"),
    add_column(data_results_trig$fitted,approach="trigonometric")
  ) %>%
    filter(model %in% unique(model_stats$model)) # Only plot the good models

  # Now we want to filter out by each site ...


  return(data_fits)


}
