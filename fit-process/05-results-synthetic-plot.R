### Make a plot of the synthetic data results

library(tidyverse)
library(periodicFitting)


# Go through all the different baseline formulas and periodic functions to produce the results
data_fits <- joint_fit_process(synthetic_data,"manuscript-figures/synthetic-results-table.txt")

yee <- data_fits %>%
  mutate(fracTime = decimal_date(date)-year(date))

yee %>%
  filter(approach == "linear",
         model == "piecewise") %>%
  ggplot(aes(x=fracTime,y=fit)) + geom_point()

yee %>%
  filter(approach == "linear",
         model == "piecewise") %>%
  arrange((fit))

# 2.03 at .201
# 1.08 at 0
(2.03 - 1.08)/.201
p1 <- data_fits %>%
  mutate(date = as.Date(date)) %>%
  filter(approach %in% c("constant","linear","quadratic")) %>%
  mutate(approach=str_to_title(approach),
         model=str_to_title(model)) %>%
  mutate(model = factor(model,levels=c("Trigonometric","Polynomial","Piecewise"),labels =c("Trigonometric","Polynomial","Piecewise Linear")),
         approach = factor(approach,levels=c("Constant","Linear","Quadratic"))) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(x=date,y=value,shape="Measured Values"),size=2) +
  geom_line(aes(x=date,y=fit,color=approach),size=1) +
  facet_grid(model~.) +
  labs(x="Date",y="Synthetic Values",color="B(t) model:",shape=NULL) +
  theme_periodic() +
  theme(axis.text.x  = element_text(angle=315, vjust=0.45,hjust=0.25,size=24),
        legend.box="vertical", legend.margin=margin()) +
  scale_x_date(date_breaks = "1 year",
               date_labels ="%Y",
               limits = c(as.Date("2007-01-01"),as.Date("2014-12-31"))) +
  scale_color_brewer(palette="Dark2") +
  theme(panel.grid.major.x =  element_line(colour = "grey50",linetype = 'dashed'))

fileName <- paste0('manuscript-figures/synthetic-results-plot.png')
ggsave(fileName,plot=p1,height=22,width=10)

