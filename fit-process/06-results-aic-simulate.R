# This is a long set of code that:
# (1) Takes the best baseline model fit for each dataset and fitting type
# (2) Randomly samples each dataset and computes the fit and the AIC.
# (3) Repeats n number of times

# This takes a LONG time to run and some of the ET data doesn't get the breakpoints estimated, causing the code to throw an error, so we have to just keep babysitting the code ( sigh )


# Get the AIC for each periodic fit, given a formula and a dataset
get_aic <- function(data,formula) {
  trigonometric <- trigonometric_fit(data,formula)$fit$AIC
  polynomial <- periodic_fit(data,formula)$fit$AIC
  piecewise <- piecewise_linear_fit(data,formula)$fit$AIC
  out <- tibble(trigonometric,polynomial,piecewise)

  return(out)
}

# Function that takes the entire dataset and computes the minimum AIC across all formulas  we run this first
get_min_formulas <- function(data) {

  out_aic <- baseline_formulas %>%
    mutate(aic = map(.x=formulas,.f=~get_aic(data,.x))) %>%
    unnest_wider(aic) %>%
    pivot_longer(names_to = "model",values_to="AIC",cols=c("trigonometric","polynomial","piecewise")) %>%
    group_by(model) %>%
    slice_min(order_by=AIC,with_ties=FALSE) %>%
    select(model,formulas) %>%
    ungroup()

  return(out_aic)
}



# Helper function to get the AIC without specifying a formula.
aic_eval <- function(data,formula,fn_name) {
  eval(parse(text=paste0("curr_fn <-", fn_name)) )
  curr_aic <- curr_fn(data,formula)$fit$AIC
  return(curr_aic)
}

# Given a list of formulas and datasets, map across them to get a dataframe of AIC  values
aic_compute <- function(data,in_formulas) {
  # Computes the AIC across a list of formulas for each model
  in_formulas %>%
    mutate(AIC = map2(.x=formulas,.y=function_name,.f=~aic_eval(data,.x,.y)) ) %>%
    select(model,AIC) %>%
    unnest(cols=AIC)
}




# Now we evaluate each of the datasets and their AIC



aic_compute_sample <- function(data,in_formulas,n_times) {

  # Make the dataset of different fractions of the data we remove
  my_data <- tibble(sample_fraction = seq(0.1,1,length.out = n_times) ) %>%
    mutate(sample_data = map(.x=sample_fraction,.f=~slice_sample(data,prop=.x)))



  # Now we loop through each of the datasets

  out_data <- my_data %>%
    mutate(AIC_results = map(.x=sample_data,.f=~aic_compute(.x,in_formulas))) %>%
    select(sample_fraction,AIC_results)

  return(out_data)

}




# Now make a function that maps on a dataset - cool!

aic_sample_process <- function(data,n_sims,n_times) {


  rerun(n_sims) %>%
    set_names(paste0("sim", 1:n_sims)) %>%
    map(~ get_aic_run(data,n_times)) %>%
    map_dfr(~ .x, .id = "simulation") %>%
    unnest(cols=AIC_results) %>%
    pivot_wider(names_from="model",values_from="AIC") %>%
    pivot_longer(names_to = "model_compare",values_to = "value",cols=c("piecewise","polynomial")) %>%
    mutate(AIC_diff = value-trigonometric) %>%  # Compare the trig to other runs
    select(simulation,sample_fraction,model_compare,AIC_diff)


}



# Now we can be ready to map!
# Identify and collate the different datasets
colorado_filter <- colorado %>% filter(product=="cumNEE") # Remove the annual NEE

et_filter <- ET_data %>% filter(site=="AU-Lox")

my_data <- tibble(names = c("colorado","co2","ET","synthetic"),
                  data = list(colorado_filter,mauna_loa,et_filter,synthetic_data)
)

# Get the minimum formulas for the data
my_formulas <- my_data %>%
  mutate(min_formulas= map(.x=data,.f=~(get_min_formulas(.x)   %>%
                                          inner_join(tibble(function_name = c("trigonometric_fit","periodic_fit","piecewise_linear_fit"),
                                                            model = c("trigonometric","polynomial","piecewise")),by="model")
  )
  ) )

n_sims <- 500
n_times <- 10



# Set up the vector of results
out_list <- vector("list",length=n_sims) %>%
  set_names(paste0("sim", 1:n_sims))

for (i in 324:n_sims) {
  #for (i in seq_along(out_list)) {
  print(i)
  aic_simulation_data <- my_formulas %>%
    mutate(out_result = map2(.x=data,.y=min_formulas,.f=~aic_compute_sample(.x,.y,n_times))) %>%
    select(names,out_result)

  out_list[[i]] <- aic_simulation_data
}

# Now map these up
out_simulations <- out_list
for(i in seq_along(out_simulations)) {
  out_simulations[[i]] <- out_simulations[[i]] %>% mutate(simulation=names(out_simulations[i]))
}

# Do a double unnest
aic_simulation_data <- bind_rows(aic_simulation_data) %>% unnest(cols=c(out_result)) %>% unnest(cols=c(AIC_results))


# Let's get ready to map! (commented because of the errors in the  piecewise linear fit that come from breakpoints not being estimated)
#aic_simulation_data <- my_data %>% mutate(my_res = map(.x=data,.f=~aic_sample_process(.x,n_sims,n_times)))

# Save your files
save(aic_simulation_data,file = 'manuscript-figures/aic-results.Rda')

# Load them up again and make your plot
load('manuscript-figures/aic-results.Rda')
median_results <- aic_simulation_data %>%
  group_by(names,sample_fraction,model) %>%
  summarize(q_values = quantile(AIC,probs=c(0.25,0.5,0.75),na.rm=TRUE),
            q_names = c("q0.25","q0.5","q0.75") ) %>%
  rename(dataset=names,
         pct = sample_fraction) %>%
  ungroup() %>%
  pivot_wider(names_from="q_names",values_from="q_values")



p1 <- median_results %>%
  mutate(dataset2 = factor(dataset,labels = c("CO[2]","Total~Net~C~Uptake","ET","Synthetic") ),
         model=str_to_title(model)) %>%
  mutate(model = factor(model,levels=c("Trigonometric","Polynomial","Piecewise"))) %>%
  #filter(model=="piecewise") %>%
  ggplot(aes(x=as.factor(100*pct),color=model)) +
  geom_hline(yintercept=0, color = "black")  +
  geom_line(aes(y=q0.5,group=model),size=3) +
  geom_point(aes(y=q0.5),size=6) +
  facet_grid(dataset~.,scales="free_y") +
  labs(y='AIC',x = "Percentage of original data (%)",color="P(\u03C4) model:") +
  theme_periodic() +
  facet_grid(dataset2~.,scales="free_y",labeller = label_parsed) +
  theme(legend.box="vertical", legend.margin=margin()) +
  scale_color_brewer(palette="Dark2")


fileName <- paste0('manuscript-figures/aic-sample-plot.png')
ggsave(fileName,plot=p1,height=22,width=12)

