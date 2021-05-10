#' Compute a baseline and periodic fit to data.
#'
#' \code{joint_taylor_process} Is an overarching analysis function that does a lot of heavy lifting:
#' \itemize{
#'   \item Computes a periodic and trigonometric fit (using periodic_fit_process and trigonometric_fit_process)
#'   \item Analyzes the two fits and excludes out any fits where the the degrees of freedom is less than expected
#'   \item Identifies the necessary information to plot on a Taylor diagram
#' }Returns a list of fitted and predicted values of periodically repeating data.  This works over a list of data sites efficiently processing multiple locations
#'
#' @param data a listed data frame with three columns: date (a POSIX date), time (the decimal time) and the value
#' @param formulas a list of formulas we want to use in the fitting
#'
#' @return A data frame of fitted values than we can use for plotting
#'
#' @examples
#'
#' # This really is an overarching function that does the master fitting, since we are doing a lot of things repeatedly this seemed to be the easiest to avoid errors!
#'
#' @import dplyr


joint_taylor_process <- function(data,formulas) {


  # Do all possible combinations of data and formulas
  over_list <- cross2(data,formulas)

  # Now we need to name all of these:
  over_names <- cross2(names(data),names(formulas)) %>%
    map(lift(paste)) %>% unlist() %>% str_split(pattern= " ",simplify = TRUE) %>% data.frame() %>%
    rename(site=X1,model=X2)

  # Compute the model fit
  models_val<-invoke_map(over_list,.f=function(x,y){ joint_fit_sample(x,formula=y)$taylor })

  # Roll them up into a data frame
  results_df <- models_val %>%
    map2(over_names$site,~mutate(.x,site=as.character(.y))) %>%
    map2(over_names$model,~mutate(.x,model=as.character(.y))) %>% bind_rows() %>%
    mutate(site=as.character(site),model=as.character(model))


  return(results_df)
}


