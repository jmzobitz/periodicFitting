### Make a plot of the CO2 and cumulative NEE Data with different results
### TO DO:
# -> Get a legend entry for the measurements
# Fix facet labels, legend on the bottom - maybe show model residuals?

library(tidyverse)
library(periodicFitting)
library(lubridate)

# Identify and collate the different datasets
colorado_filter <- colorado %>%
  filter(product=="cumNEE") # Remove the annual NEE

# Go through all the different baseline formulas and periodic functions to produce the results
data_fits <- joint_fit_process(colorado_filter,"manuscript-figures/cumnee-results-table.txt")


# Make the figure of the different fits
p1 <- data_fits %>%
  filter(approach %in% c("constant","linear","quadratic")) %>%
  mutate(approach=str_to_title(approach),
         model=str_to_title(model)) %>%
  mutate(model = factor(model,levels=c("Trigonometric","Polynomial","Piecewise"), labels=c("Trigonometric","Polynomial","Piecewise Linear")),
         approach = factor(approach,levels=c("Constant","Linear","Quadratic"))) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(x=date,y=value,shape="Measured Values"),size=2) +
  geom_line(aes(x=date,y=fit,color=approach),size=1) +
  geom_hline(yintercept=0, color = "black")  +
  facet_grid(model~.,scales="free_y") +
  labs(x="Date",y=expression(Total~Net~C~Uptake~(g~C~m^{-2})),color="B(t) model:",shape=NULL) +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  scale_x_date(date_breaks = "1 year",
               date_labels ="%Y",
               limits = c(as.Date("1999-01-01"),as.Date("2013-12-31"))) +
  scale_y_continuous(breaks = seq(-4000,0,by=500)) +
  scale_color_brewer(palette="Dark2") +
  theme(panel.grid.major.x =  element_line(colour = "grey50",linetype = 'dashed'))



fileName <- paste0('manuscript-figures/cumNEE-results-plot.png')
ggsave(fileName,plot=p1,height=22,width=10)

