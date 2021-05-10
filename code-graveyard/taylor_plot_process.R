#' Make a Taylor plot using a list of data
#'
#' \code{taylor_plot_process} Returns a ggplot for a Taylor diagram using a list of data, which can be then saved.  Prior to this exercise you need to have t
#'
#' @param data_results a data frame with eight columns:
#' \itemize{
#' \item fit: fitted values
#' \item lwr: lower confidence value
#' \item upr: upper confidence value
#' \item value: the original value
#' \item site: place (or dataset) utilized
#' \item model: type of baseline model
#' \item date: date (a POSIX date),
#' \item time: (the decimal time)
#' }
#'
#' Typically this will be returned by a periodic_fit_process command and then joined together.
#'
#'
#' @return A Taylor diagram
#'
#' @examples
#'
#' # To be filled in later

#' @import dplyr
#' @import ggforce
#' @import purrr


taylor_plot_process <- function(data_results) {



  # Compute the necessary stuff to do the Taylor plots
  model_rsq <- data_results %>%
    group_by(site,model) %>%
    summarize(sd_meas=1,
              sd_model=sd(fit)/sd(value),
              r=cor(fit,value),
              centered_rms=sd((value-mean(value))-((fit-mean(fit))))/sd(value)
    ) %>%
    mutate(x_coord = sd_model*r, y_coord = sd_model*sin(acos(r)))

  # Generate and save the Taylor plot
  t_plot <- taylor_plot()

  curr_plot <- t_plot +
    geom_point(data=model_rsq,aes(x=x_coord,y=y_coord,color=model),size=3) +
    facet_grid(~site)+
    labs(x="",y=expression(sigma[model])) +
    theme(axis.text = element_text(size=14),
          axis.title=element_text(size=28),
          title=element_text(size=26),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14)) +
    theme(strip.text.x = element_text(size=12),
          strip.text.y = element_text(size=12))

  return(curr_plot)
}
