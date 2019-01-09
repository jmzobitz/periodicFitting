### Make a plot of the CO2 and cumulative NEE Data with different results
### TO DO:
# -> Get a legend entry for the measurements
# Fix facet labels, legend on the bottom - maybe show model residuals?

library(tidyverse)
library(periodicFitting)


# Identify and collate the different datasets
colorado_filter <- colorado %>% filter(product=="cumNEE") # Remove the annual NEE

data <- rbind(colorado_filter,mauna_loa,synthetic_data) %>% split(.$product)

data_fits <- joint_fit_process(data,"manuscript-figures/co2-nee-synthetic-results-table.txt")

formulas <- list("quadratic"=baseline_formulas$formulas$quadratic)
taylor_data <- joint_taylor_process(data,formulas)


p1 <- data_fits %>%
  rename(product=site) %>%  # Rename columns
  ggplot() +
  geom_point(aes(x=time,y=value,color="measured"),size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  facet_wrap(product~approach,scales="free") +
  labs(x="Date",color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20))




fileName <- paste0('manuscript-figures/cumNEE-co2-synthetic-results-plot.png')
ggsave(fileName,plot=p1,width=14)


# ### Now do a Taylor plot:



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

fileName <- paste0('manuscript-figures/cumNEE-co2-synthetic-taylor-plot.png')
ggsave(fileName,plot=p2,width=14)

