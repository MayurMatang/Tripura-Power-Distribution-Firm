####################  Load libraries ####################

library(tidyverse)
library(here)
library(lubridate)
library(readxl)

####################  Load SAI datasets ####################

sai_1 <- read_excel(here("data-raw/SAI FY 20-23/HT LT & CONSUMER RELATED FAULT REPORT UNDER ED-KAILASHAHAR.xlsx"), sheet=1)

sai_2 <- read_excel(here("data-raw/SAI FY 20-23/HT LT & CONSUMER RELATED FAULT REPORT UNDER ED-KAILASHAHAR.xlsx"), sheet=2)

sai_3 <- read_excel(here("data-raw/SAI FY 20-23/HT LT & CONSUMER RELATED FAULT REPORT UNDER ED-KAILASHAHAR.xlsx"), sheet=3)

# Merge SAI datasets
merged_sai <- rbind(sai_1, sai_2, sai_3)


####################  Clean  ####################

# Add division, circle & UID columns

merged_sai$Circle <- "UNOKOTI"
merged_sai$Division <- "KAILASHAHAR"
merged_sai$UID = paste0("sai_",c(1:nrow(merged_sai))) 

# Make a date time column
merged_sai$date_time <- ymd_hm(paste(merged_sai$Date, merged_sai$`Supply Fail Time`))

####################  Write File  ####################
write.csv(merged_sai, "data-preped/merged_sai.csv")



