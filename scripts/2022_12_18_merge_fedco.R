################# Load libraries #################
library(tidyverse)
library(here)
library(lubridate)


################# Load & merge FEDCO datasets #################
fedco_20 <- read_csv(here("data-raw", "FEDCO FY 20-21", "Complaints Dump data-FEDCO_FY2020-21.csv"))
fedco_21 <- read_csv(here("data-raw", "FEDCO FY 21-22", "Complaints Dump data-FEDCO_FY2021-22.csv"))
fedco_22 <- read_csv(here("data-raw", "FEDCO FY 22-23", "Complaints Dump data-FEDCO_FY2022-23.csv"))
merged_fedco <- rbind(fedco_20, fedco_21, fedco_22)


################# Format Date Time coumns #################
merged_fedco <- merged_fedco |> 
  mutate(Complaintlogtime_clean = 
           parse_date_time(Complaintlogtime, c("%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S")),
         Closedate_clean = 
           parse_date_time(Closedate, c("%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S")))


################# Write csv file #################
write.csv(merged_fedco, "data-preped/merged_fedco.csv")
