####################  Load libraries ####################
library(tidyverse)
library(here)
library(lubridate)


############## Import merged TSECL & Consumption data ##############
merged_tsecl <- read_csv(here("data-preped", "merged_tsecl.csv"))
merged_consumption <- read_csv(here("data-preped", "merged_consumption.csv"))


######################## Clean data ########################

# remove row number column
merged_tsecl <- merged_tsecl[,-1]                
merged_consumption <- merged_consumption[,-1]

# Add Acc_month_year column in TSECL
merged_tsecl <- merged_tsecl |> 
  mutate(Month = month(ComplaintDate_clean, label = T, abbr=T),
         Year = year(ComplaintDate_clean)) |> 
  unite("Acc_Month_Year", c(ACCNO, Month, Year),remove = F)

# Add Acc_month_year column in Consumption
merged_consumption <- merged_consumption |> 
  unite("Acc_Month_Year", c(Account_No, Month, Year),remove = F)


############## Merge TSECL & Consumption data  ##############
merged_tsecl_consumption <- left_join(merged_tsecl, merged_consumption,
                                      by = "Acc_Month_Year")


#########################  End  #########################
merged_consumption_new <- merged_consumption |> 
  pivot_wider(
    names_from = c(Month, Year), 
    values_from = c(Unit, Bill)          # one row is one account number now
  )


