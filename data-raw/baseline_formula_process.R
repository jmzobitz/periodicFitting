# Define the baseline formulas we are using throughout this setup:


# Identify the models we are going to use
baseline_formulas <- tibble(name=c("constant","linear","quadratic","cubic","quartic"),
                            df=1:5,
                            formulas = c(value ~ 1,value ~ 1+time,value ~ 1+time+I(time^2),value ~ 1+time+I(time^2) + I(time^3),value ~ 1+time+I(time^2) + I(time^3) + I(time^4)))

use_data(baseline_formulas,overwrite = TRUE)


