### Make a plot of the CO2 and cumulative NEE Data with different results
### TO DO:
# -> Get a legend entry for the measurements

library(tidyverse)


# Cumulative NEE Data

colorado_filter <- colorado %>% filter(product=="cumNEE") # Remove the annual NEE

p1 <- data_results$fitted %>%
  filter(product=="cumNEE") %>%
  ggplot() +
  geom_point(aes(x=time,y=value),data=colorado_filter,size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  labs(x="Date",y=expression(~Net~Carbon~Uptake~(g~C~m^{-2})),color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20))


fileName <- paste0('manuscript-figures/cumNEE-results-plot.png')
ggsave(fileName,plot=p1,width=14)
# We need to take each of these and map the date and time to the columns


### CO2 data
# Cumulative NEE Data

p2<-data_results$fitted %>%
  filter(product=="co2") %>%
  ggplot() +
  geom_point(aes(x=time,y=value),data=mauna_loa,size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  labs(x="Date",y=expression(CO[2]~(ppm)),color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20))


fileName <- paste0('manuscript-figures/co2-results-plot.png')
ggsave(fileName,plot=p2,width=14)
# We need to take each of these and map the date and time to the columns

