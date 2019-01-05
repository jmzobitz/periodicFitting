library(tidyverse)
library(periodicFitting)
# Let's organize this to do the AIC/BIC for a host of models

# Identify and collate the different datasets
colorado_filter <- colorado %>% filter(product=="cumNEE") # Remove the annual NEE

data <- rbind(colorado_filter,nicaragua,mauna_loa) %>% split(.$product)

# Identify the models we are going to use
formulas <- list("constant" = value ~ 1,
                  "linear" = value ~ 1+time,
                 "quadratic" = value ~ 1+time+I(time^2),
                 "cubic" = value ~ 1+time+I(time^2) + I(time^3),
                 "quartic" = value ~ 1+time+I(time^2) + I(time^3) + I(time^4))


# Do all possible combinations of data and formulas
over_list <- cross2(data,formulas)

# Now we need to name all of these:
over_names <- cross2(names(data),names(formulas)) %>%
  map(lift(paste)) %>% unlist() %>% str_split(pattern= " ",simplify = TRUE) %>% data.frame() %>%
  rename(product=X1,model=X2)



# Compute the model fit
models_val<-invoke_map(over_list,.f=function(x,y){ periodic_fit(x,y)$fit })

# Compute the AIC over each model
aic_unlist <- models_val %>% map(AIC) %>% unlist()

aic_val <- over_names %>% add_column(aic=aic_unlist) %>%
  arrange(product)


# We need to be careful because we have coefficients that aren't used, but there is a lot of commonality we can extract here

# Compute the model fit
fit_results_val<-invoke_map(over_list,.f=function(x,y){ periodic_fit(x,y)$prediction })

# OK: next step is to see how we can get the fit and the names bound to each one




# This is a hacky cheat, but I can't get pmap to work so it mutates the model and product simulataneously
results_df <- fit_results_val %>%
  map2(over_names$product,~mutate(.x,product=.y)) %>%
  map2(over_names$model,~mutate(.x,model=.y)) %>% bind_rows() %>%
  mutate(product=as.character(product),model=as.character(model)) %>%
  split(.$model) %>%
  map2_df(list(bind_rows(data)),~mutate(.x,date=.y$date,time=.y$time))

data_results <- list(models=models_val,aic=aic_val,fitted=results_df)

# Save the data to be used later
use_data(data_results,overwrite = TRUE)
