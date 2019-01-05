# AUTHOR: JMZ
# Modified: 1/4/19
# Script code to create ET for each site.  Allows us to access the data later

# Read in APPEARS albedo data for comparison

# VERIFY FLAGS ARE CORRECT

library(tidyverse)
library(lubridate)
library(devtools)


# Pull in the mod GPP data
mod_GPP_data <- read_csv('data-raw/gpp-only-periodically-repeating-data/GPP-Only-Periodically-Repeating-Data-MOD17A2H-006-results.csv') %>%
  select(ID,Date,
         matches("_Gpp_500m|_bitmask")) %>%
  rename(site=ID,date=Date,value=3,flag=4) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date),product='GPP',flag=str_detect(flag,pattern="0b00000000")) # I think this is the highest quality flag ...


# Pull in the myd GPP data
myd_GPP_data <- read_csv('data-raw/gpp-only-periodically-repeating-data/GPP-Only-Periodically-Repeating-Data-MYD17A2H-006-results.csv') %>%
  select(ID,Date,
         matches("_Gpp_500m|_bitmask")) %>%
  rename(site=ID,date=Date,value=3,flag=4) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date),product='GPP',flag=str_detect(flag,pattern="0b00000010")) # I think this is the highest quality flag ...


# Now join these up together and clean up
GPP_data <- rbind(myd_GPP_data,mod_GPP_data) %>% filter(flag & time >= 2012) %>%
  select(site,date,time,value,product) %>%
  arrange(site,date)



use_data(GPP_data,overwrite = TRUE)


