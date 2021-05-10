#' Define a custom theme for making ggplots
#'
#' \code{theme_periodic} Sets up defaults for ggplots so I don't need to keep doing these over and over.
#'

#' @export


theme_periodic <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size=30),
          axis.title=element_text(size=36),
          title=element_text(size=36),
          legend.text=element_text(size=24),
          legend.title=element_text(size=26),
          strip.text.x = element_text(size=30),
          strip.text.y = element_text(size=30),
          strip.background = element_rect(colour="white", fill="white")) +
    theme( panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

}
