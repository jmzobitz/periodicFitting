# AUTHOR: JMZ
# Modified: 1/1/19
# Script code to datasets - I am using Andy and Brandon's files


library(tidyverse)
library(lubridate)
library(devtools)

#

### DATA SET 1: MAUNA LOA CO2
# I downloaded the data and then modified it in Excel to strip trailing white space, makes it easier to read in.
mauna_loa <- read_csv('data-raw/co2_mm_mlo-jz.csv',na="-99.99")  %>%
  select(decimal_date,average) %>%
  mutate(time = date_decimal(decimal_date),co2=average  ) %>%
  select(-decimal_date,-average) %>% na.omit() %>%
  mutate(time=floor_date(time,unit="month"))
use_data(mauna_loa,overwrite = TRUE)

### DATA SET 2: CUMULUATIVE NEE FROM COLORADO
colorado <- read_csv('data-raw/neeData-PB.csv') %>%
  mutate(time=date_decimal(1998+time)) %>%
  mutate(time=round_date(time,unit="day"))  # Round up the days
use_data(colorado,overwrite = TRUE)

### DATA SET 3: GPP and ET from Nicaragua
et_data <- read_csv('data-raw/ET_1km_Farm_01_Area_579_meters(2018).csv') %>% select(year,month,ET_1km)

# Make the GPP data timestamps similar to the ET data
gpp_data <- read_csv('data-raw/Gpp_1km_Farm_01_Area_579_meters.csv') %>% select(year,month,time,Gpp_1km)


nicaragua <- et_data %>% left_join(gpp_data,by=c("year","month")) %>%
  mutate(time=date_decimal(year+time)) %>% select(-year,-month) %>%
  gather(key=product,value=value,Gpp_1km,ET_1km) %>% arrange(time) %>%
  mutate(time=round_date(time,unit="day"))

use_data(nicaragua,overwrite = TRUE)

