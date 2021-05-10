### Make a plot of the CO2 data with the fitting
### TO DO:
# -> Get a legend entry for the measurements
# Fix facet labels, legend on the bottom - maybe show model residuals?

library(tidyverse)
library(periodicFitting)


# Go through all the different baseline formulas and periodic functions to produce the results
data_fits <- joint_fit_process(mauna_loa,"manuscript-figures/co2-results-table.txt")


# Make a plot of the fitting results
p1 <- data_fits %>%
  mutate(date = as.Date(date)) %>%
  filter(approach %in% c("constant","linear","quadratic")) %>%
  mutate(approach=str_to_title(approach),
         model=str_to_title(model)) %>%
  mutate(model = factor(model,levels=c("Trigonometric","Polynomial","Piecewise"),labels =c("Trigonometric","Polynomial","Piecewise Linear") ),
         approach = factor(approach,levels=c("Constant","Linear","Quadratic"))) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(x=date,y=value,shape="Measured Values"),size=2) +
  geom_line(aes(x=date,y=fit,color=approach),size=1) +
  facet_grid(model~.,scales="free_y") +
  labs(x="Date",y=expression(CO[2]~(ppm)),color="B(t) model:",shape=NULL) +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  scale_x_date(date_breaks = "5 years",
               date_labels ="%Y") +
  scale_y_continuous(breaks = seq(300,500,by=25)) +
  scale_color_brewer(palette="Dark2") +
  theme(panel.grid.major.x =  element_line(colour = "grey50",linetype = 'dashed'))



fileName <- paste0('manuscript-figures/co2-results-plot.png')
ggsave(fileName,plot=p1,height=22,width=10)


