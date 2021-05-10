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

  data_results <- baseline_formulas %>%
    mutate(trigonometric = map(.x=formulas,.f=~trigonometric_fit(data,.x)),
           polynomial = map(.x=formulas,.f=~periodic_fit(data,.x)),
           piecewise = map(.x=formulas,.f=~piecewise_linear_fit(data,.x)) ) %>%
    pivot_longer(names_to="model",values_to="results",cols=c("trigonometric","polynomial","piecewise")) %>%
    hoist(results,predict="prediction",
          stats = "fit" ) %>%
    unnest_wider(col="stats") %>%
    rename(approach = name)  # Rename for later





  # Return out the results of the periodic fit and the trig where we have consistent estimates:



  model_stats <- data_results %>%
    select(approach,model,r.squared,AIC,centered_rms) %>%
    mutate(across(.cols=c("r.squared","AIC","centered_rms"),.fns=~round(.,digits=2) )) %>%
    mutate(approach=factor(approach,levels=c("constant","linear","quadratic","cubic","quartic")),
           model = factor(model,levels=c("trigonometric","polynomial","piecewise"))) %>%
    arrange(model,approach)


  model_table <- model_stats %>%
    pivot_wider(names_from="approach",names_glue = "{approach}.{.value}",
                values_from=c(r.squared,AIC,centered_rms) )



  ### Save a table we could format
  write.table(model_table, file = model_stats_filename, sep = " & ", eol= " \\\\\r",quote=FALSE)

  # Make a plot where the trigonometric and periodic are fit side by side in a facetted plot

  data_fits <- data_results %>%
    select(approach,model,predict) %>%
    unnest(cols=predict)


  # Now we want to filter out by each site ...



  return(data_fits)


}

