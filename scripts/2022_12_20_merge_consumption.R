######################## Load Libraries ########################
library(tidyverse)
library(here)
library(lubridate)
library(readxl)

######################## Import data ########################
df1 <- read_excel(here("data-raw/Consumption/1. Unit_Level_Consumption_Report_August_2020_March_2021.xlsx"))

df2 <- read_excel(here("data-raw/Consumption/5. Unit_Level_Consumption_Report_April_2021_March_2022.xlsx"))

df3 <- read_excel(here("data-raw/Consumption/6. Unit_Level_Consumption_Report_April_2022_Oct 2022.xlsx"))

######################## Clean data ########################

# Rename column names in df1
colnames(df1) <- c('SDOCODE','Subdivision','Account_No','Rural/Urban','Consumer_Type','Consumer_Category','Unit_Aug_2020','Unit_Sep_2020','Unit_Oct_2020','Unit_Nov_2020','Unit_Dec_2020','Unit_Jan_2021','Unit_Feb_2021','Unit_Mar_2021', 'Bill_Aug_2020','Bill_Sep_2020','Bill_Oct_2020','Bill_Nov_2020','Bill_Dec_2020','Bill_Jan_2021','Bill_Feb_2021','Bill_Mar_2021', 'Balance')

# Rename column names in df2
colnames(df2) <- c('SDOCODE','Subdivision','Account_No','Rural/Urban','Consumer_Type','Consumer_Category','Unit_Apr_2021','Unit_May_2021','Unit_Jun_2021','Unit_Jul_2021','Unit_Aug_2021','Unit_Sep_2021','Unit_Oct_2021', 'Unit_Nov_2021','Unit_Dec_2021','Unit_Jan_2022', 'Unit_Feb_2022','Unit_Mar_2022','Bill_Apr_2021','Bill_May_2021','Bill_Jun_2021','Bill_Jul_2021','Bill_Aug_2021','Bill_Sep_2021','Bill_Oct_2021', 'Bill_Nov_2021','Bill_Dec_2021','Bill_Jan_2022', 'Bill_Feb_2022','Bill_Mar_2022', 'Balance')

# Rename column names in df3
colnames(df3) <- c('SDOCODE','Subdivision','Account_No','Rural/Urban','Consumer_Type','Consumer_Category','Unit_Apr_2022','Unit_May_2022','Unit_Jun_2022','Unit_Jul_2022','Unit_Aug_2022','Unit_Sep_2022','Unit_Oct_2022','Bill_Apr_2022','Bill_May_2022','Bill_Jun_2022','Bill_Jul_2022','Bill_Aug_2022','Bill_Sep_2022','Bill_Oct_2022','Balance')


######################## Transform Data ########################

# Pivot df1
df1_long <- df1[,-23]                  # remove balance column

df1_long <- df1_long |> pivot_longer(          
  cols = 7:22,
  names_to = c("Type","Month","Year"),
  names_sep = "_",
  values_to = "UnitConsumed/BilledAmount") |> 
  pivot_wider(
    names_from = 'Type', 
    values_from = 'UnitConsumed/BilledAmount')

df1_long$SDOCODE <- as.character(df1_long$SDOCODE)
df1_long$Account_No <- as.character(df1_long$Account_No)

# Pivot df2
df2_long <- df2[,-31]                  # remove balance column

df2_long <- df2_long |> pivot_longer(          
  cols = 7:30,
  names_to = c("Type","Month","Year"),
  names_sep = "_",
  values_to = "UnitConsumed/BilledAmount") |> 
  pivot_wider(
    names_from = 'Type', 
    values_from = 'UnitConsumed/BilledAmount')

df2_long$SDOCODE <- as.character(df2_long$SDOCODE)

# Pivot df3
df3_long <- df3[,-21]                  # remove balance column

df3_long <- df3_long |> pivot_longer(          
  cols = 7:20,
  names_to = c("Type","Month","Year"),
  names_sep = "_",
  values_to = "UnitConsumed/BilledAmount") |> 
  pivot_wider(
    names_from = 'Type', 
    values_from = 'UnitConsumed/BilledAmount')


######################## Combine all df_long rows ########################
merged_consumption <- rbind(df1_long,df2_long,df3_long)


######################## Clean data ########################

# Clean rural/urban column
merged_consumption$`Rural/Urban`[merged_consumption$`Rural/Urban` == "u"] <- "U"  

# Clean subdivision names
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                                 "BANAMALIPUR_I"] <- "BANAMALIPUR 1"

merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "BANAMALIPUR_II"] <- "BANAMALIPUR 2"

merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "BISHALGARH_1"] <- "BISHALGARH 1"

merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "BISHALGARH_2"] <- "BISHALGARH 2"

merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "BORDOWALI_III_Rural"] <- "BORDOWALI RURAL"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "BORDOWALI_VI_Urban"] <- "BORDOWALI URBAN"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "CAPITAL_COMPLEX"] <- "CAPITAL COMPLEX"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "DHARMANAGAR_I"] <- "DHARMANAGAR 1"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "DHARMANAGAR_II"] <- "DHARMANAGAR 2"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "TELIAMURA_I"] <- "TELIAMURA 1"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "TELIAMURA_II"] <- "TELIAMURA 2"
merged_consumption$Subdivision[merged_consumption$Subdivision ==
                                 "DURGACHOWMUHANI_R"] <- "DURGACHOWMOHANI"


######################## Save the file ########################
write.csv(merged_consumption, "data-preped/merged_consumption.csv")

write.xlsx(merged_consumption, 'data-preped/merged_consumption.xlsx')
save(merged_consumption, file = "merged_consumption.Rda")
