### Make a plot of the albedo fit results
# -> Get a legend entry for the measurements


library(tidyverse)
library(periodicFitting)

data <- mcd43A3 %>% split(.$site)

data_fits <- joint_fit_process(data,"manuscript-figures/albedo-results-table.txt")

formulas <- list("quadratic"=baseline_formulas$formulas$quadratic)
taylor_data <- joint_taylor_process(data,formulas)


p1 <- data_fits %>%
  ggplot() +
  geom_point(aes(x=time,y=value,color="measured"),size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  facet_grid(site~approach,scales="free_y") +
  labs(x="Date",y="Albedo",color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))


fileName <- paste0('manuscript-figures/albedo-results-plot.png')
ggsave(fileName,plot=p1,width=14)
# We need to take each of these and map the date and time to the columns


# # Now do a Taylor plot:
p2 <- taylor_plot() +
  geom_point(data=taylor_data,aes(x=x_coord,y=y_coord,color=cut_number(percentage,n=9)),size=3) +
  facet_grid(approach~site)+
  labs(x="",y=expression(sigma[model])) +
  theme(axis.text = element_text(size=14),
  axis.title=element_text(size=28),
  title=element_text(size=26),
  legend.text=element_text(size=12),
  legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size=12),
  strip.text.y = element_text(size=12))

fileName <- paste0('manuscript-figures/albedo-taylor-plot.png')
ggsave(fileName,plot=p2,width=14)
