# Let's organize this to do the AIC/BIC for a host of models

# Identify and collate the different datasets
data <- rbind(colorado,nicaragua,mauna_loa) %>% split(.$product)

# Identify the models we are going to use
formulas <- list("linear" = value ~ 1+time,
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
aic_val <- models_val %>% map(AIC) %>% unlist()

# Now bind the AIC, names together
out_values <- data.frame(over_names,aic_val) %>%
  arrange(product)

# We need to be careful because we have coefficients that aren't used, but there is a lot of commonality we can extract here

# Compute the model fit
fit_results_val<-invoke_map(over_list,.f=function(x,y){ periodic_fit(x,y)$prediction })


