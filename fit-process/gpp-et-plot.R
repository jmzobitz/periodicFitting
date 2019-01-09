### Make a plot of the fit results for ET and GPP


library(tidyverse)
library(periodicFitting)

data <- GPP_data %>% split(.$site)

data_fits <- joint_fit_process(data,"manuscript-figures/gpp-results-table.txt")

formulas <- list("quadratic"=baseline_formulas$formulas$quadratic)
taylor_data <- joint_taylor_process(data,formulas)

# Make a time series plot:
p1 <- data_fits %>%
  ggplot() +
  geom_point(aes(x=time,y=value,color="measured"),size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  facet_grid(site~approach,scales="free_y") +
  labs(x="Date",y="GPP",color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))


fileName <- paste0('manuscript-figures/gpp-results-plot.png')
ggsave(fileName,plot=p1,width=14)
# We need to take each of these and map the date and time to the columns

# # Make a Taylor plot:
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

fileName <- paste0('manuscript-figures/gpp-taylor-plot.png')
ggsave(fileName,plot=p2,width=14)


### NOW DO ET DATA
data <- ET_data %>% split(.$site)


data_fits <- joint_fit_process(data,"manuscript-figures/et-results-table.txt")

formulas <- list("quadratic"=baseline_formulas$formulas$quadratic)
taylor_data <- joint_taylor_process(data,formulas)


p3 <- data_fits %>%
  ggplot() +
  geom_point(aes(x=time,y=value,color="measured"),size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  facet_grid(site~approach,scales="free_y") +
  labs(x="Date",y="ET",color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))


fileName <- paste0('manuscript-figures/et-results-plot.png')
ggsave(fileName,plot=p3,width=14)


# # Make a Taylor plot:
p4 <- taylor_plot() +
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

fileName <- paste0('manuscript-figures/et-taylor-plot.png')
ggsave(fileName,plot=p4,width=14)

