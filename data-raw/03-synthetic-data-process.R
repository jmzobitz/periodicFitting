# Make a synthetic data set for testing, using three linear functions

library(tidyverse)
library(periodicFitting)
library(lubridate)
# Create a representative plot of the different periodic fits.
m<- 5
c <- 1
b1 <- 0.2
b2 <- 0.6
c_pw <- 1

n_points <- 30

data <- tibble( time = seq(0,1,length.out = n_points),
                ) %>%
  mutate(piecewise = m*time+c_pw,
         piecewise = if_else(between(time,b1,b2),m*(b2-b1-1)/(b2-b1)*(time-b1)+m*b1+c_pw,piecewise),
         piecewise = if_else(time >= b2, m*(time-1)+c_pw,piecewise) )



n_times <- 10
synthetic_data <- do.call("rbind", replicate(n_times, data, simplify = FALSE))  %>%
  mutate(date = seq(ymd('2007-01-01'),ymd('2016-12-31'),length.out=n_times*n_points),
         time = decimal_date(date),
         product = 'synthetic') %>%
  rename(value = piecewise) %>%
  relocate(date,time,product,value) %>%
  mutate(value = value + 3-0.001*time + 0.1*rnorm(n_times*n_points))


use_data(synthetic_data,overwrite = TRUE)


