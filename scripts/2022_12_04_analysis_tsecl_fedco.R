# Load libraries
library(tidyverse)
library(here)
library(kableExtra)
library(lubridate)

# Load merged TSECL & FEDCO data
merged_tsecl_fedco <- read_csv(here("data-preped","merged_tsecl_fedco.csv"))

# Number of complaints across years and months in a table (add color to cell showing )

table(year(merged_tsecl_fedco$ComplaintDate_clean), month(merged_tsecl_fedco$ComplaintDate_clean)) |> 
  kbl(caption = "Complaints across Year & Months") %>%
  kable_classic(full_width = F, html_font = "Cambria") |> 
  add_header_above(c(" " = 1, "Months" = 12)) |> 
  kable_styling(bootstrap_options = c("striped", "hover"))

# Descriptive charts of Complaint Type variables

merged_tsecl_fedco |> group_by(ComplaintType) |>  # Complaint Types in absolute
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(ComplaintType) |>  # Complaint Types in proportion
  summarise(n = n()) |> 
  ggplot(aes(x = 1, fill = ComplaintType, y=n)) + geom_col(position = "fill")  + theme_classic()

merged_tsecl_fedco |> group_by(ComplaintType) |>  # Complaint Types w/o Other Complaint
  filter(ComplaintType != "Other Complaint") |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> filter(ComplaintType == "Other Complaint") |>  # 'Other Complaint' in Subcomplaint Type
  group_by(SubComplaintType) |>  
  filter(!is.na(SubComplaintType)) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(SubComplaintType) |>                # Subcomplaint Types
  filter(!is.na(SubComplaintType)) |> 
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_classic()


merged_tsecl_fedco |> group_by(SubComplaintType) |>                # Subcomplaint Types > 1000
  filter(!is.na(SubComplaintType)) |> 
  summarise(n = n()) |> arrange(desc(n)) |>
  filter(n > 1000) |> 
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(SubComplaintType) |>  # SubComplaint Types in proportion
  filter(!is.na(SubComplaintType)) |> 
  summarise(n = n()) |> 
  filter(n > 1000) |> 
  ggplot(aes(x = 1, fill = SubComplaintType, y=n)) + geom_col(position = "fill")  + theme_classic()

merged_tsecl_fedco |> filter(SubComplaintType == "Other Technical") |># Subcomplaintype_B under 'Other Technical'
  group_by(SubComplaintType_B) |>
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(SubComplaintType_B, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> filter(SubComplaintType == "OTHERS") |># Subcomplaintype_B under 'Others'
  group_by(SubComplaintType_B) |>
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(SubComplaintType_B, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> filter(!is.na(SubComplaintType_B)) |>     # All Subcomplaintype_B
  group_by(SubComplaintType_B) |>
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(SubComplaintType_B, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(SubComplaintType_B) |>     # All Subcomplaintype_B > 500
  filter(!is.na(SubComplaintType_B)) |>
  summarise(n = n()) |> arrange(desc(n)) |> 
  filter(n > 500) |> 
  ggplot(aes(x = reorder(SubComplaintType_B, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Priority) |>     # Priority of complaint
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(Priority, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Priority) |>     # Priority of complaint w/o NA
  filter(!is.na(Priority)) |>
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(Priority, n), y = n)) + geom_col() + coord_flip() + theme_classic()

# Descriptive charts of subdivision, division & circle variables

merged_tsecl_fedco |> group_by(SubdivisionName_clean) |>   # Subdivisions with most number of complaint
  summarise(n = n()) |> arrange(desc(n)) |> 
  filter(n > 500) |> 
  ggplot(aes(x = reorder(SubdivisionName_clean, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(SubdivisionName_clean) |>   # Subdivisions with least number of complaint
  filter(!is.na(SubdivisionName_clean)) |> 
  summarise(n = n()) |> arrange(n) |> 
  filter(n < 500) |> 
  ggplot(aes(x = reorder(SubdivisionName_clean, -n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Division) |>   # Divisions
  filter(!is.na(Division)) |> 
  summarise(n = n()) |> arrange(n) |> 
  ggplot(aes(x = reorder(Division, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Division) |>   # Divisions with most number of complaint
  filter(!is.na(Division)) |> 
  summarise(n = n()) |> arrange(n) |> 
  filter(n > 1000) |>
  ggplot(aes(x = reorder(Division, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Division) |>   # Divisions with least number of complaint
  filter(!is.na(Division)) |> 
  summarise(n = n()) |> arrange(n) |> 
  filter(n < 1000) |>
  ggplot(aes(x = reorder(Division, -n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(Circle) |>   # Circle
  filter(!is.na(Circle)) |> 
  summarise(n = n()) |> arrange(n) |> 
  ggplot(aes(x = reorder(Circle, n), y = n)) + geom_col() + coord_flip() + theme_classic()

# Complaints across Franchisee

merged_tsecl_fedco |> group_by(`TSECL / FEDCO / SAI`) |>   # TSECL = 173312, FEDCO = 41294
  filter(!is.na(`TSECL / FEDCO / SAI`)) |>                  # 'TSECL / FEDCO / SAI' Column is unreliable
  summarise(n = n()) |> arrange(desc(n)) |> 
  ggplot(aes(x = reorder(`TSECL / FEDCO / SAI`, n), y = n)) + geom_col() + coord_flip() + theme_classic()

merged_tsecl_fedco |> group_by(str_detect(UID, "tsecl_")) |>              # TSECL = 174446, FEDCO = 40294
  summarise(n = n()) |> arrange(desc(n))       # Shaheen  confirm subdivions franchisee from Ujjal 


# Complaints filed across years, months & dates

merged_tsecl_fedco |> group_by(year(ComplaintDate_clean)) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = `year(ComplaintDate_clean)`, y = n)) + 
  geom_col() + theme_classic()

merged_tsecl_fedco |> group_by(year(ComplaintDate_clean), month(ComplaintDate_clean)) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = `month(ComplaintDate_clean)`, y = n, color = factor(`year(ComplaintDate_clean)`))) + 
  geom_line() + theme_classic() + scale_x_continuous(n.breaks = 12)

merged_tsecl_fedco |> group_by(day(ComplaintDate_clean), month(ComplaintDate_clean)) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = `day(ComplaintDate_clean)`, y = n, color = factor(`month(ComplaintDate_clean)`))) + 
  geom_line() + theme_classic() + scale_x_continuous(n.breaks = 31)


# Complaints filed across time  (hour extraction is incorrect)

merged_tsecl_fedco |> group_by(hour(ComplaintDate_clean)) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = `hour(ComplaintDate_clean)`, y = n)) + 
  geom_line() + theme_classic() + scale_x_continuous(n.breaks = 8)

merged_tsecl_fedco |> Complaint

-----------------------------
 # TO-DO List
  - Number of complaints each year 
- Number of complaints each month (by year)
- Medium of complaint
- Number of complaints by category 
- Average/Total time to resolution
- Number of complaints by geography (20% of the complaints are driven by x, y, z sub-divisions)/ per-capita complaints across geography 
- Circle
- Division 
- Sub-division
-------------------------------------------------------------
print(n=Inf)
filter(!is.na(SubComplaintType)) |> 
filter(n > 1000) |>
filter(!is.na(SubComplaintType)) |> 
filter(ComplaintType == "Other Complaint") |>

test <- merged_tsecl_fedco |> filter(SubdivisionName_clean== NA) |> 
  select(SDOCODE.x, SubdivisionName_clean, Division, Circle)




merged_tsecl_fedco <- merged_tsecl_fedco |> 
  rename(Subdivision = SubdivisionName_clean, SDOCODE=SDOCODE.y, 
         Division=Division.y, Circle=Circle.y)




  
  --------------Analysis---------------


# Create column for seasons
merged_data_clean <- merged_data_clean |> mutate(Season = 
                                                   ifelse(month(ComplaintDateClean) >= 5 & month(ComplaintDateClean) <= 9, "Monsoon",
                                                          ifelse(month(ComplaintDateClean) == 10 | month(ComplaintDateClean) == 11, "Post-Monsoon",
                                                                 ifelse(month(ComplaintDateClean) == 3 | month(ComplaintDateClean) ==4, "Summer",
                                                                        ifelse(month(ComplaintDateClean) == 1 | month(ComplaintDateClean) ==2 | month(ComplaintDateClean) ==12, "Winter","None"      
                                                                        )))))


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
df2 <- merged_tsecl |> rename(Subdivision = SubdivisionName) |> select(SDOCODE, Subdivision)

stringdist_join(df1, df2,                                #perform fuzzy matching left join
                by= 'Subdivision',                       #match based on subdivision name
                mode='right',                            #use right join
                method = "lv",                           #use jw distance metric
                max_dist=99, 
                distance_col='dist')  |> 
  group_by(Subdivision.y)  |> 
  slice_min(order_by=dist, n=1)