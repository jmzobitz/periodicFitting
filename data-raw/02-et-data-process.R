# AUTHOR: JMZ
# Modified: 1/4/19
# Script code to create ET for each site.  Allows us to access the data later

# Read in APPEARS albedo data for comparison

# VERIFY FLAGS ARE CORRECT AND the SCALE FACTOR

library(tidyverse)
library(lubridate)
library(devtools)


# Pull in the mod GPP data
mod_ET_data <- read_csv('data-raw/et-periodically-repeating-data/ET-Periodically-Repeating-Data-MOD16A2-006-results.csv') %>%
  select(ID,Date,
         matches("_ET_500m|_bitmask")) %>%
  rename(site=ID,date=Date,value=3,flag=4) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date),product='ET',flag=str_detect(flag,pattern="0b00000000")) # I think this is the highest quality flag ...


# Pull in the myd GPP data
myd_ET_data <- read_csv('data-raw/et-periodically-repeating-data/ET-Periodically-Repeating-Data-MYD16A2-006-results.csv') %>%
  select(ID,Date,
         matches("_ET_500m|_bitmask")) %>%
  rename(site=ID,date=Date,value=3,flag=4) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date),product='ET',flag=str_detect(flag,pattern="0b00000010")) # I think this is the highest quality flag ...


# Now join these up together and clean up
ET_data <- rbind(myd_ET_data,mod_ET_data) %>% filter(flag & time >= 2012) %>%
  mutate(value=value*0.1) %>%  # I think we need to multiply by the scale factor ...
  select(site,date,time,value,product) %>%
  arrange(site,date)



use_data(ET_data,overwrite = TRUE)


