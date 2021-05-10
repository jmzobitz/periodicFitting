# Make a simple plot of the NEE data + annual trends
library(tidyverse)
library(lubridate)


# Filter out the cumulative NEE product
p1 <- colorado %>%
  filter(product=="cumNEE") %>%
  ggplot() +
  geom_line(aes(x=date,y=value),size=2) +
  geom_hline(yintercept=0, color = "black")  +
  geom_smooth(aes(x=date,y=value),se=FALSE,method='lm') +
  labs(x="Date",y=expression(Total~Net~C~Uptake~(g~C~m^{-2})),color="B(t) model") +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  scale_x_date(date_breaks = "1 year",
               date_labels ="%Y",
               limits = c(as.Date("1999-01-01"),as.Date("2013-12-31"))) +
  scale_y_continuous(breaks = seq(-4000,0,by=500)) +
  theme(panel.grid.major.x =  element_line(colour = "grey50",linetype = 'dashed'))


fileName <- paste0('manuscript-figures/cumNEEplot.png')
ggsave(fileName,plot=p1,width=10,height=9)


# Filter out the annual NEE product
p2 <- colorado %>%
  filter(product=="annNEE") %>%
  ggplot() +
  geom_line(aes(x=yday(date),y=value,color=factor(year(date))),size=1) +
  labs(x="Day of Year",y=expression(~Annual~Net~C~uptake~(g~C~m^{-2})))+
  scale_x_continuous(breaks = c(1,90,180,270,365),
                     limits = c(1,365)) +
  geom_hline(yintercept=0, color = "black")  +
  geom_vline(xintercept=365/2, color = "black",linetype="dashed")  +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  guides(color=FALSE) +
  scale_x_continuous(breaks = seq(0,365,by=45)) +
  scale_y_continuous(breaks = seq(-300,50,by=50)) +
  scale_color_brewer(palette="Dark2")
p2

fileName <- paste0('manuscript-figures/annNEEplot.png')
ggsave(fileName,plot=p2,height=9,width=10)

