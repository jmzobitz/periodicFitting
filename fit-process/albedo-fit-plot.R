### Make a plot of the albedo fit results
# -> Get a legend entry for the measurements


library(tidyverse)


# Cumulative NEE Data


p1 <- mcd43A3_results$fitted %>%
  ggplot() +
  geom_point(aes(x=time,y=value),data=mcd43A3,size=1) +
  geom_point(aes(x=time,y=fit,color=model),size=1) +
  facet_grid(site~.,scales="free_y") +
  labs(x="Date",y="Albedo",color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20),
        strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))


fileName <- paste0('manuscript-figures/albedo-results-plot.png')
ggsave(fileName,plot=p1,width=14)
# We need to take each of these and map the date and time to the columns


### CO2 data
# Cumulative NEE Data

p2<-data_results$fitted %>%
  filter(product=="co2") %>%
  ggplot() + geom_line(aes(x=time,y=fit,color=model),size=1) +
  geom_line(aes(x=time,y=value),data=mauna_loa,size=1) +
  labs(x="Date",y=expression(CO[2]~(ppm)),color="B(t) model") +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20))


fileName <- paste0('manuscript-figures/co2-results-plot.png')
ggsave(fileName,plot=p2,width=14)
# We need to take each of these and map the date and time to the columns

