# AUTHOR: JMZ
# Modified: 1/4/19
# Script code to create albedo data (white sky albedo) for each site.  Allows us to access the data later

# Read in APPEARS albedo data for comparison


library(tidyverse)
library(lubridate)
library(devtools)


# Pull in the black sky albedo for band 2
modisAlbedoData_BSA <- read_csv('data-raw/albedo-periodically-repeating-ms/Albedo-Periodically-Repeating-MS-MCD43A3-006-results.csv') %>%
  select(ID,Date,
         matches("_Albedo_BSA_Band")) %>%
  rename(site=ID,date=Date) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date),product='BS_Albedo') %>%
  gather(key=band_str,value=value,-time,-date,-product,-site) %>%  # Gather by site and time
  filter(str_detect(band_str, "Band2")) %>% # Add columns for the kernel and reflectance band
  select(-band_str)


modisAlbedo_flag <- read_csv('data-raw/albedo-periodically-repeating-ms/Albedo-Periodically-Repeating-MS-MCD43A3-006-results.csv') %>%
  select(ID,Date,
         matches("_Mandatory_Quality_Band")) %>%
  select(-matches("bitmask|MODLAND|Fill")) %>% # Remove the extra pieces
  rename(site=ID,date=Date) %>%   # Rename the columns
  mutate(date=ymd(date),time = decimal_date(date)) %>%
  gather(key=band_str,value=flag,-time,-date,-site) %>%  # Gather by site and time
  filter(str_detect(band_str, "Band2")) %>% # Add columns for the kernel and reflectance band
  select(-band_str)

# From the APPEARS webpage:
# *Mandatory QA bit index:
#   0 = processed, good quality (full BRDF inversions)
# 1 = processed, see other QA (magnitude BRDF inversions)
# 255 = Fill Value


# here if QA = 0, we have good quality.
# only take if we have QA = 0 across all bands


# Now join the two datasets together, filter out good flags
mcd43A3 <- modisAlbedoData_BSA %>%
  left_join(modisAlbedo_flag,by=c("site","date","time")) %>%
  filter(flag==0) %>%
  select(-flag)


use_data(mcd43A3,overwrite = TRUE)


