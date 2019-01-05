library(ggforce)

# Identify and collate the different datasets
colorado_filter <- colorado %>% filter(product=="cumNEE") # Remove the annual NEE

data <- rbind(colorado_filter,nicaragua,mauna_loa) %>% split(.$product)


# Add the data values onto each of the fitted results
model_w_data<-data_results$fitted  %>%
  split(.$model) %>%
  map2_df(list(bind_rows(data)),~mutate(.x,data=.y$value))


# Compute the necessary stuff to do the Taylor plots
data_rsq <- model_w_data %>%
  group_by(product,model) %>%
  summarize(sd_meas=1,
            sd_model=sd(fit)/sd(data),
            r=cor(fit,data),
            centered_rms=sd((data-mean(data))-((fit-mean(fit))))/sd(data)
  ) %>%
  mutate(x_coord = sd_model*r, y_coord = sd_model*sin(acos(r)))


# Generate and save the Taylor plot
t_plot <- taylor_plot()

curr_plot <- t_plot +
  geom_point(data=data_rsq,aes(x=x_coord,y=y_coord,color=model),size=3) +
  facet_grid(~product)+
  labs(x="",y=expression(sigma[model])) +
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=28),
        title=element_text(size=26),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size=12),
        strip.text.y = element_text(size=12))

fileName <- paste0('manuscript-figures/taylor-data-plot.png')
ggsave(fileName,plot=curr_plot,width=14)

