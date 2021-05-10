# Create a representative plot of the different periodic fits.
aT <- 1
bT <- 1
m<- 5
c <- 1
b1 <- 0.2
b2 <- 0.8
c_pw <- 0

data <- tibble( time = seq(0,1,length.out = 1000),
                trigonometric = aT*sin(2*pi*time)+bT*cos(2*pi*time),
                polynomial = m*time *(time-1)*(2 *time-1)  + c* (1 - 30 *time^2 + 60* time^3 - 30 *time^4 ),
                ) %>%
mutate(piecewise = m*time+c_pw,
       piecewise = if_else(between(time,b1,b2),m*(b2-b1-1)/(b2-b1)*(time-b1)+m*b1+c_pw,piecewise),
       piecewise = if_else(time >= b2, m*(time-1)+c_pw,piecewise) )


p1 <- data %>%
  pivot_longer(names_to = "model",values_to="value",cols=c("trigonometric":"piecewise") ) %>%
  mutate(model = factor(model,levels=c("trigonometric","polynomial","piecewise"),labels =c("Trigonometric","Polynomial","Piecewise Linear"))) %>%
  ggplot(aes(x=time,y=value,color=model)) + geom_line(size=1) + geom_hline(yintercept = 0) +
  labs(x="\u03C4",y="P(\u03C4)",color="P(\u03C4) model:") +
  theme_periodic() +
  theme(legend.text=element_text(size=18),
legend.title=element_text(size=20)) +
  scale_color_brewer(palette="Dark2")

fileName <- paste0('manuscript-figures/sample_periodic.png')
ggsave(fileName,plot=p1,height=9,width=10)
