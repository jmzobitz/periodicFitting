### Make a plot of the fit results for ET and GPP


library(tidyverse)
library(periodicFitting)

# Filter out the particular dataset used in the paper
data <- ET_data %>% filter(site=="AU-Lox")

# Go through all the different baseline formulas and periodic functions to produce the results
data_fits <- joint_fit_process(data,"manuscript-figures/et-results-table.txt")


# Make a time series plot:
p1 <- data_fits %>%
  filter(approach %in% c("constant","linear","quadratic")) %>%
  mutate(approach=str_to_title(approach),
         model=str_to_title(model)) %>%
  mutate(model = factor(model,levels=c("Trigonometric","Polynomial","Piecewise"),labels =c("Trigonometric","Polynomial","Piecewise Linear")),
         approach = factor(approach,levels=c("Constant","Linear","Quadratic"))) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(x=date,y=value,shape="Measured Values"),size=2) +
  geom_line(aes(x=date,y=fit,color=approach),size=1) +
  facet_grid(model~.) +
  labs(x="Date",y=expression(~ET~(mm~~H[2]~O~~m^{-2})),color="B(t) model:",shape=NULL) +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  scale_x_date(date_breaks = "1 year",
               date_labels ="%Y",
               limits = c(as.Date("2012-01-01"),as.Date("2018-12-31"))) +
  scale_y_continuous(breaks=seq(0,4,by=0.5)) +
  scale_color_brewer(palette="Dark2") +
  theme(panel.grid.major.x =  element_line(colour = "grey50",linetype = 'dashed'))



fileName <- paste0('manuscript-figures/et-results-plot.png')
ggsave(fileName,plot=p1,height=22,width=10)


