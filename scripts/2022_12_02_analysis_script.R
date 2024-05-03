# Load libraries
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)

# Load data
merged_data_tsecl <- read_csv(here("data-preped","merged_data.csv"))

# Clean data
merged_data_tsecl <- merged_data_tsecl[,-1]  #remove row numbers in column 1
blank_rows <- merged_data_tsecl[c(22554, 22555, 22556), ] 
merged_data_tsecl <- merged_data_tsecl[-c(22554, 22555, 22556),] # remove blank rows

# Import & merge FEDCO datasets
fedco_20 <- read_csv(here("data-raw", "FEDCO FY 20-21", "Complaints Dump data-FEDCO_FY2020-21.csv"))
fedco_21 <- read_csv(here("data-raw", "FEDCO FY 21-22", "Complaints Dump data-FEDCO_FY2021-22.csv"))
fedco_22 <- read_csv(here("data-raw", "FEDCO FY 22-23", "Complaints Dump data-FEDCO_FY2022-23.csv"))
merged_data_fedco <- rbind(fedco_20, fedco_21, fedco_22)

# Clean complainttype & subcomplaints in TSECL
merged_data_tsecl |> group_by(ComplaintType, SubComplaintType) |>  count() |> print(n=Inf)

merged_data_tsecl$ComplaintType[merged_data_tsecl$SubComplaintType == "All Area no power-HT issue"] <- "Other Complaint"
merged_data_tsecl$ComplaintType[merged_data_tsecl$SubComplaintType == "All Area no power-LT issue"] <- "Other Complaint"

merged_data_tsecl |> group_by(ComplaintType, SubComplaintType) |>  count() |> print(n=Inf)

# Clean complainttype & subcomplaints in FEDCO
merged_data_fedco |> group_by(Category_name, Subcategory_A, Subcategory_B) |>  count() |> print(n=Inf)

merged_data_fedco$Category_name[merged_data_fedco$Category_name == "BILLING"] <- "Bill Related"
merged_data_fedco$Category_name[merged_data_fedco$Category_name == "CONNECTION"] <- "Commercial Request"
merged_data_fedco$Category_name[merged_data_fedco$Category_name == "METER"] <- "Meter Related"
merged_data_fedco$Category_name[merged_data_fedco$Category_name == "OTHERS"] <- "Other Complaint"
merged_data_fedco$Category_name[merged_data_fedco$Subcategory_A == "SERVICE CABLE FAULT"] <- "Service Line Related"
merged_data_fedco$Category_name[merged_data_fedco$Subcategory_A == "H T Line Complaint"] <- "Other Complaint"
merged_data_fedco$Category_name[merged_data_fedco$Subcategory_A == "LT LINE COMPLAINTS"] <- "Other Complaint"

merged_data_fedco$Subcategory_A[merged_data_fedco$Subcategory_A == "BILL CORRECTION"] <- "Bill Is Wrong"
merged_data_fedco$Subcategory_A[merged_data_fedco$Subcategory_A == "BILL DISPUTE"] <- "Bill Is Wrong"
merged_data_fedco$Subcategory_A[merged_data_fedco$Subcategory_A == "LT LINE COMPLAINTS"] <- "All Area no power-LT issue"
merged_data_fedco$Subcategory_A[merged_data_fedco$Subcategory_A == "H T Line Complaint"] <- "All Area no power-HT issue"
merged_data_fedco$Subcategory_A[merged_data_fedco$Subcategory_A == "VOLTAGE RELATED"] <- "Voltage Fluctuation"

# Adding/Deleting columns in TSECL & FEDCO datasets to have same columns across both datasets
merged_data_tsecl_new <- merged_data_tsecl[,-c(1,3:9, 12:15, 19, 20, 23:29)]  #Delete irrelevant columns

merged_data_tsecl_new <- merged_data_tsecl_new |>   # Adding columns
  mutate(Division = "", Circle="", SubComplaintType_B="", Priority="", 
         UID = paste0("tsecl_",c(1:nrow(merged_data_tsecl_new)))) 
  
merged_data_tsecl_new <- merged_data_tsecl_new[, c(13,2,3,1,4,9,10,5,6,11,12,7,8)] #Change column positions


merged_data_fedco_new <- merged_data_fedco[, -c(1:11,14:16, 22,23, 25:31, 33:35)] #Delete irrelevant columns

merged_data_fedco_new <- merged_data_fedco_new |>   
  mutate(Division="", Circle="", SDOCODE="",   # Adding columns
         UID = paste0("fedco_",c(1:nrow(merged_data_fedco_new)))) |> 
  rename(PHONE = Caller_number, MOBILENO = Cons_mobileno, SubdivisionName=Subdivision,  # Renaming Columns
         ComplaintType=Category_name, SubComplaintType=Subcategory_A, SubComplaintType_B=Subcategory_B,
         ComplaintDate=Complaintlogtime, ComplaintCloseDate=Actualclosetime)

merged_data_fedco_new <- merged_data_fedco_new[, c(13, 1,2,12,8, 10, 11, 4,5,6,7,3,9)] #Change column positions

# Merging TSECL & FEDCO
merged_tsecl_fedco <- rbind(merged_data_tsecl_new, merged_data_fedco_new)

# Clean Circle Division Subdivision names








subdivision_mastersheet <- read_csv(here("data-raw", "subdivison_mastersheet.csv")) #import subdivision list 





----------------------------

# Clean subcomplaints in FEDCO
complaint_categories_fedco <- fedco_all |> group_by(Category_name, Subcategory_A, Subcategory_B) |>  count()

subdivision_list$SDOCODE %in% unique(merged_data_clean$SDOCODE)
unique(merged_data_clean$SDOCODE) %in% subdivision_list$SDOCODE

unavailable_sdo <- merged_data_clean |> filter(SDOCODE %in% 
  unique(merged_data_clean$SDOCODE)[1:56]) |> 
  select(SubdivisionName, SDOCODE) |> 
  distinct()

merged_data_clean |> filter(SDOCODE %in% unique(merged_data_clean$SDOCODE)[1:56]) |> 
  pull(SubdivisionName) |> 
  unique()

merged_data_clean |> filter(!SDOCODE %in% unique(merged_data_clean$SDOCODE)[1:56]) |> 
  pull(SubdivisionName) |> 
  unique()
  
write.csv(unavailable_sdo, "unavailable_sdo.csv")


# Clean data-time in ComplaintDate & ComplaintCloseDate
merged_data_clean$ComplaintDateClean <- parse_date_time(merged_data_clean$ComplaintDate, orders = c('ymd HM', 'dmy HM'))
merged_data_clean$ComplaintCloseDateClean <- parse_date_time(merged_data_clean$ComplaintCloseDate, orders = c('ymd HM', 'dmy HM'))

# Create column for seasons
merged_data_clean <- merged_data_clean |> mutate(Season = 
 ifelse(month(ComplaintDateClean) >= 5 & month(ComplaintDateClean) <= 9, "Monsoon",
 ifelse(month(ComplaintDateClean) == 10 | month(ComplaintDateClean) == 11, "Post-Monsoon",
 ifelse(month(ComplaintDateClean) == 3 | month(ComplaintDateClean) ==4, "Summer",
 ifelse(month(ComplaintDateClean) == 1 | month(ComplaintDateClean) ==2 | month(ComplaintDateClean) ==12, "Winter","None"      
                      )))))


--------------Analysis---------------
# Absolute volume of complaints from 2018-22 & yearly growth rates
abs_com_date <- merged_data_clean |> mutate(date = date(ComplaintDateClean)) |>  
  group_by(date) |> 
  summarise(n = n())

ggplot(abs_com_date, aes(x=date, y=n)) + geom_line() + scale_x_continuous(n.breaks = 3)

abs_com_year <- merged_data_clean |> mutate(year = year(ComplaintDateClean)) |>  
  group_by(year) |> 
  summarise(n = n())

ggplot(abs_com_year, aes(x=year, y=n)) + geom_line() + geom_point() + scale_x_continuous(n.breaks = 3)

abs_com_month <- merged_data_clean |> mutate(year = year(ComplaintDateClean), month = month(ComplaintDateClean)) |>  
  group_by(year,month) |> 
  summarise(n = n())

ggplot(abs_com_month, aes(x=month, y=n, color=as.factor(year))) + geom_line() + scale_x_continuous(n.breaks = 12)

com_growthrate <- merged_data_clean |> mutate(year = year(ComplaintDateClean)) |>  
  group_by(year) |> 
  summarise(n = n()) |> 
  mutate(growth = (n - lag(n))/lag(n))

ggplot(com_growthrate, aes(x=year, y=growth)) + geom_line() + scale_x_continuous(n.breaks = 3)

# Line graph showing proportion of complaints filed across different mediums: 1912,BudhyutBandhu,IVRS, WhatsApp, TSECL web portal, FB, Twitter
merged_data_clean  |>  drop_na(ComplaintSource) |> 
  mutate(year = year(ComplaintDateClean)) |> 
  group_by(year, ComplaintSource)  |> 
  summarise(n = n())  |> 
  mutate(percent = n/sum(n))  |> 
  arrange(desc(percent)) |> 
  ggplot(aes(x=year, y=percent, color=ComplaintSource)) + 
    geom_line() + scale_x_continuous(n.breaks = 3)
    
# Bar graph showing volume of complaints during the following time intervals: 12AM-06AM, 6AM-10AM, 10AM-2PM, 2PM-5PM, 5PM-8PM, 8PM-12AM
df_time <- merged_data_clean  |>  drop_na(ComplaintDateClean) |> 
  mutate(hour = hour(ComplaintDateClean), 
         minute = minute(ComplaintDateClean),
         time = as.numeric(paste(hour, minute, sep = ".")), 
         time_range = 
    ifelse(time >= 00.00 & time <= 05.59, "12AM-06AM",
    ifelse(time >= 06.00 & time <= 9.59, "06AM-10AM",       
    ifelse(time >= 10.00 & time <= 13.59, "10AM-02PM",       
    ifelse(time >= 14.00 & time <= 16.59, "02PM-05PM",
    ifelse(time >= 17.00 & time <= 19.59, "05PM-08PM",
    ifelse(time >= 20.00 & time <= 23.59, "08PM-12AM", "none"))))))) |> 
  unique(time_range)
  
  group_by(year, ComplaintSource)  |> 
  summarise(n = n())  |> 
  mutate(percent = n/sum(n))  |> 
  arrange(desc(percent)) |> 
  ggplot(aes(x=year, y=percent, color=ComplaintSource)) + 
  geom_line() + scale_x_continuous(n.breaks = 3)



# Bar graph showing frequency across the 8 complaint types (refer to CRM) (does this vary across years or across mediums, if   yes, will be interesting to point that out)
merged_data_clean %>% drop_na(ComplaintType) |> 
  group_by(ComplaintType) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent)) |> 
  ggplot(aes(x=reorder(ComplaintType, -percent, decreasing=T), y=percent)) + 
  geom_col() + 
  coord_flip() +
  labs(x="Complaint Type")

merged_data_clean %>% drop_na(ComplaintType) |>
  filter(ComplaintType!="Other Complaint") |> 
  group_by(year=lubridate::year(ComplaintDateClean), ComplaintType) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) |> 
  ggplot(aes(x=reorder(ComplaintType, -n, decreasing=T), y=n)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(x="Complaint Type") + facet_grid(~year)

# Do we notice any seasonal patterns? If not we can drop this slide
merged_data_clean %>% drop_na(ComplaintType) |> 
  group_by(Season,ComplaintType) %>%
  summarise(n = n())  |> 
  ggplot(aes(x=reorder(ComplaintType, -n, decreasing=T), y=n)) + 
  geom_col() + 
  coord_flip() +
  labs(x="Complaint Type") +
  facet_wrap(~Season)

merged_data_clean %>% filter(ComplaintType=="Other Complaint") %>%
  group_by(Season,SubComplaintType) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) |> 
  ggplot(aes(x=reorder(SubComplaintType, -n, decreasing=T), y=n)) + 
  geom_col() + 
  coord_flip() +
  labs(x="Other Complaint") +
  facet_wrap(~Season)

# X, Y, Z complaints are particularly salient in A, B, C circles
merged_data_test <- merged_data_test %>% 
  mutate(SubdivisionName = toupper(SubdivisionName)) # Upper case column
  
merged_data_test$SubdivisionName <- gsub("ESD OFFICE,", "", merged_data_test$SubdivisionName) #Removing "ESD OFFICE" 
merged_data_test$SubdivisionName <- gsub("ESD", "", merged_data_test$SubdivisionName) #Removing "ESD" 
merged_data_test$SubdivisionName <- gsub("-", "", merged_data_test$SubdivisionName) #Removing "-" 
merged_data_test$SubdivisionName <- gsub("_", "", merged_data_test$SubdivisionName) #Removing "_" 

merged_data_test$SubdivisionName <- gsub(" ", "", merged_data_test$SubdivisionName) # Remove whitespace


----------------------
# Subdivision 
subdivision <- merged_data %>% group_by(SubdivisionName) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Complaint Type
merged_data %>% group_by(ComplaintType) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

merged_data %>% filter(ComplaintType == "Other Complaint") %>%
  group_by(SubComplaintType) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))
              
# Complaint Source
merged_data %>% group_by(ComplaintSource) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Outage Type
merged_data %>% group_by(OutageType) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Cause
merged_data %>% group_by(Cause) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Consumer Type 
merged_data %>% group_by(ConsumerType) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Year/Month
merged_data %>% group_by(month_year) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  arrange(desc(percent))

# Extract year, month  and time from ComplaintDate & ComplaintCloseDate
merged_data_test[['ComplaintDate']] <- as.POSIXct(merged_data_test[['ComplaintDate']],
                                                  format = "%Y-%m-%d %H:%M:%S")


merged_data_test$ComplaintDate_year <- lubridate::year(merged_data_test$ComplaintDate)
merged_data_test$ComplaintDate_Month <- lubridate::month(merged_data_test$ComplaintDate)
merged_data_test$ComplaintDate_Date <- lubridate::day(merged_data_test$ComplaintDate)
merged_data_test$ComplaintDate_Time <- lubridate::hms(merged_data_test$ComplaintDate)


# Fuzzy Matching
df1 <- subdivision_mastersheet |> select(Subdivision)
df2 <- merged_data_tsecl |> rename(Subdivision = SubdivisionName) |> select(SDOCODE, Subdivision)

stringdist_join(df1, df2,                                #perform fuzzy matching left join
                by= 'Subdivision',                       #match based on subdivision name
                mode='right',                            #use right join
                method = "lv",                           #use jw distance metric
                max_dist=99, 
                distance_col='dist')  |> 
  group_by(Subdivision.y)  |> 
  slice_min(order_by=dist, n=1)




