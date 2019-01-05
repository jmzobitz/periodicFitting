# Make a simple plot of the NEE data + annual trends
library(tidyverse)
library(lubridate)

p1 <- colorado %>%
  filter(product=="cumNEE") %>%
  ggplot() +
  geom_line(aes(x=date,y=value),size=2) +
  labs(x="Date",y=expression(~Net~Carbon~Uptake~(g~C~m^{-2})),color="B(t) model")
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20))


fileName <- paste0('manuscript-figures/cumNEEplot.png')
ggsave(fileName,plot=p1,width=14)

p2 <- colorado %>%
  filter(product=="annNEE") %>%
  ggplot() +
  geom_point(aes(x=yday(date),y=value,color=factor(year(date))),size=1) +
  labs(x="Day of Year",y=expression(~Annual~Net~Carbon~Uptake~(g~C~m^{-2})),color="B(t) model")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=12),
        legend.title=element_text(size=16),
        legend.position="bottom") +
  scale_x_continuous(breaks = c(1,90,180,270,365)) +
  geom_hline(yintercept=0, color = "black")


fileName <- paste0('manuscript-figures/annNEEplot.png')
ggsave(fileName,plot=p2,height=10)

