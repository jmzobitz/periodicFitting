# Define the baseline formulas we are using throughout this setup:


# Identify the models we are going to use
formulas <- list("constant" = value ~ 1,
                 "linear" = value ~ 1+time,
                 "quadratic" = value ~ 1+time+I(time^2),
                 "cubic" = value ~ 1+time+I(time^2) + I(time^3),
                 "quartic" = value ~ 1+time+I(time^2) + I(time^3) + I(time^4))

formula_info <- data.frame(name=c("constant","linear","quadratic","cubic","quartic"),df=1:5)


baseline_formulas <- list("formulas"=formulas,"formula_info"=formula_info)

use_data(baseline_formulas,overwrite = TRUE)
