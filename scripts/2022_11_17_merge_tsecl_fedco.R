# Load libraries
library(tidyverse)
library(here)
library(lubridate)

# Load data
merged_tsecl <- read_csv(here("data-preped","merged_tsecl.csv"))


# Clean date time 
merged_tsecl <- merged_tsecl[-c(22554, 22555, 22556),-1]  #remove row numbers & blank rows

merged_tsecl <- merged_tsecl |> 
  mutate(ComplaintCloseDate_clean = 
  parse_date_time(ComplaintCloseDate, c("%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M")),
  ComplaintDate_clean = 
    parse_date_time(ComplaintDate, c("%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M")))


# Import & merge FEDCO datasets
fedco_20 <- read_csv(here("data-raw", "FEDCO FY 20-21", "Complaints Dump data-FEDCO_FY2020-21.csv"))
fedco_21 <- read_csv(here("data-raw", "FEDCO FY 21-22", "Complaints Dump data-FEDCO_FY2021-22.csv"))
fedco_22 <- read_csv(here("data-raw", "FEDCO FY 22-23", "Complaints Dump data-FEDCO_FY2022-23.csv"))
merged_fedco <- rbind(fedco_20, fedco_21, fedco_22)

# Clean date time
merged_fedco <- merged_fedco |> 
  mutate(Complaintlogtime_clean = 
           parse_date_time(Complaintlogtime, c("%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S")),
         Closedate_clean = 
           parse_date_time(Closedate, c("%d-%m-%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S")))


# Clean complainttype & subcomplaints in TSECL
merged_tsecl |> group_by(ComplaintType, SubComplaintType) |>  count() |> print(n=Inf)

merged_tsecl$ComplaintType[merged_tsecl$SubComplaintType == "All Area no power-HT issue"] <- "Other Complaint"
merged_tsecl$ComplaintType[merged_tsecl$SubComplaintType == "All Area no power-LT issue"] <- "Other Complaint"

merged_tsecl |> group_by(ComplaintType, SubComplaintType) |>  count() |> print(n=Inf)

# Clean complainttype & subcomplaints in FEDCO
merged_fedco |> group_by(Category_name, Subcategory_A, Subcategory_B) |>  count() |> print(n=Inf)

merged_fedco$Category_name[merged_fedco$Category_name == "BILLING"] <- "Bill Related"
merged_fedco$Category_name[merged_fedco$Category_name == "CONNECTION"] <- "Commercial Request"
merged_fedco$Category_name[merged_fedco$Category_name == "METER"] <- "Meter Related"
merged_fedco$Category_name[merged_fedco$Category_name == "OTHERS"] <- "Other Complaint"
merged_fedco$Category_name[merged_fedco$Subcategory_A == "SERVICE CABLE FAULT"] <- "Service Line Related"
merged_fedco$Category_name[merged_fedco$Subcategory_A == "H T Line Complaint"] <- "Other Complaint"
merged_fedco$Category_name[merged_fedco$Subcategory_A == "LT LINE COMPLAINTS"] <- "Other Complaint"

merged_fedco$Subcategory_A[merged_fedco$Subcategory_A == "BILL CORRECTION"] <- "Bill Is Wrong"
merged_fedco$Subcategory_A[merged_fedco$Subcategory_A == "BILL DISPUTE"] <- "Bill Is Wrong"
merged_fedco$Subcategory_A[merged_fedco$Subcategory_A == "LT LINE COMPLAINTS"] <- "All Area no power-LT issue"
merged_fedco$Subcategory_A[merged_fedco$Subcategory_A == "H T Line Complaint"] <- "All Area no power-HT issue"
merged_fedco$Subcategory_A[merged_fedco$Subcategory_A == "VOLTAGE RELATED"] <- "Voltage Fluctuation"

# Adding/Deleting columns in TSECL & FEDCO datasets to have same columns across both datasets
merged_tsecl_new <- merged_tsecl[,-c(1,3:9, 12:15, 19:22, 23:29)]  #Delete irrelevant columns

merged_tsecl_new <- merged_tsecl_new |>   # Adding columns
  mutate(SubComplaintType_B="", Priority="", 
         UID = paste0("tsecl_",c(1:nrow(merged_tsecl_new)))) 
  
merged_tsecl_new <- merged_tsecl_new[, c(11,2,3,5,6,9,10,8,7,1,4)] #Change column positions


merged_fedco_new <- merged_fedco[, -c(1:11,14:17, 22,23, 25:30, 31:35)] #Delete irrelevant columns

merged_fedco_new <- merged_fedco_new |>   
  mutate(UID = paste0("fedco_",c(1:nrow(merged_fedco_new))),
         SDOCODE = "") |>  # Adding unique id column
  rename(PHONE = Caller_number, MOBILENO = Cons_mobileno, SubdivisionName=Subdivision,  # Renaming Columns
         ComplaintType=Category_name, SubComplaintType=Subcategory_A, SubComplaintType_B=Subcategory_B,
         ComplaintDate_clean=Complaintlogtime_clean, ComplaintCloseDate_clean=Closedate_clean)

merged_fedco_new <- merged_fedco_new[, c(10,1,2,3,4,5,6,8,9,11,7)] #Change column positions

# Merging TSECL & FEDCO
merged_tsecl_fedco <- rbind(merged_tsecl_new, merged_fedco_new)


# Clean Subdivision names 
merged_tsecl_fedco <- merged_tsecl_fedco |> mutate(
    SubdivisionName_clean = str_remove(pattern = "ESD Office,"
                                                ,string = SubdivisionName),
    SubdivisionName_clean = str_remove(pattern = "ESD",
                                                string = SubdivisionName_clean),
    SubdivisionName_clean = str_to_upper(SubdivisionName_clean),
    SubdivisionName_clean = str_remove_all(pattern = "_",
                                           string = SubdivisionName_clean),
    SubdivisionName_clean = str_remove_all(pattern = "-",
                                           string = SubdivisionName_clean),
    SubdivisionName_clean = str_trim(string = SubdivisionName_clean, side="left"),
    SubdivisionName_clean = str_squish(SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "AMTOLI", replacement="AMTALI",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BOGAFA", replacement="BAGAFA",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BANAMALI I", replacement="BANAMALIPUR 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BANAMALIPUR 1I", replacement="BANAMALIPUR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BANAMALI II", replacement="BANAMALIPUR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BANAMALIPURI", replacement="BANAMALIPUR 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BANAMALIPURII", replacement="BANAMALIPUR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BILONIA", replacement="BELONIA",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BISHALGARH I", replacement="BISHALGARH 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BISHALGARH1", replacement="BISHALGARH 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BISHALGARH2", replacement="BISHALGARH 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BISHGARHII", replacement="BISHALGARH 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BISRAMGANJ", replacement="BISHRAMGANJ",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BORDOWALIIIIRURAL", replacement="BORDOWALI RURAL",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BORDOWALIVIURBAN", replacement="BORDOWALI URBAN",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BORDUALI URBAN", replacement="BORDOWALI URBAN",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "BORDUALI RURAL", replacement="BORDOWALI RURAL",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "CAPITALCOMPLEX", replacement="CAPITAL COMPLEX",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "CC", replacement="CAPITAL COMPLEX",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "CHAMPAKNAGARMANDWIKHUMLWNG", replacement="CHAMPAKNAGAR",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "DHARMANAGARI", replacement="DHARMANAGAR 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "DHARMANAGARII", replacement="DHARMANAGAR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "DHARMANAGAR 1I", replacement="DHARMANAGAR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "DURGACHOUMOHONI", replacement="DURGACHOWMOHANI",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "DURGACHOWMUHANIR", replacement="DURGACHOWMOHANI",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "GANDACHERRA", replacement="GANDACHARA",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "HRISHYAMUKHJOLAIBARI", replacement="HRISHYAMUKH",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "JATANBARIKARBOOK", replacement="JATANBARI",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "JUBARAJNAGARDHARMANAGARII", replacement="JUBARAJNAGAR",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "JUBARAJNAGARDHARMANAGAR 1I", replacement="JUBARAJNAGAR",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "KAILASAHARI", replacement="KAILASHAHAR 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "KAILASAHARII", replacement="KAILASHAHAR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "KAILASHAHAR 1I", replacement="KAILASHAHAR 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "MADHUPURSEKERKOTE", replacement="MADHUPUR",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "MELAGARH", replacement="MELAGHAR",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "PADMABILL", replacement="PADMABIL",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "PRATAPGHAR", replacement="PRATAPGARH",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "SALEMADURGACHOWMOHANI", replacement="SALEMA",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "TELIAMURAI", replacement="TELIAMURA 1",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "TELIAMURAII", replacement="TELIAMURA 2",
                                        string = SubdivisionName_clean),
    SubdivisionName_clean = str_replace(pattern = "TELIAMURA 1I", replacement="TELIAMURA 2",
                                        string = SubdivisionName_clean))

merged_tsecl_fedco$SubdivisionName_clean[merged_tsecl_fedco$SubdivisionName_clean=="BANAMALIPUR 1I"] <-"BANAMALIPUR 2"
merged_tsecl_fedco$SubdivisionName_clean[merged_tsecl_fedco$SubdivisionName_clean=="JUBARAJNAGARDHARMANAGAR 2"] <-"JUBARAJNAGAR"
merged_tsecl_fedco |> pull(SubdivisionName_clean) |>  unique() |> sort()
  

# Join subdivision mastersheet to insert division & circle in merged_tsecl_fedco 
subdivision_mastersheet <- read_csv(here("OtherFiles", "subdivison_mastersheet.csv")) #import subdivision list 

merged_tsecl_fedco <- merge(x = merged_tsecl_fedco, y = subdivision_mastersheet, 
                                 by.x = "SubdivisionName_clean", by.y = "Subdivision", all.x = T) # Left join

merged_tsecl_fedco <- merged_tsecl_fedco[, -c(12:14)]  # Remove irrelevant columns

# Fixing NAs in SubDivision/Diviosion/Circle Names
merged_tsecl_fedco |> filter(is.na(SubdivisionName_clean)) |> 
  select(SDOCODE.x, SubdivisionName_clean, Division, Circle)

merged_tsecl_fedco$SubdivisionName_clean[merged_tsecl_fedco$SDOCODE.x == 1007102] <- "SALEMA"
merged_tsecl_fedco$Division[merged_tsecl_fedco$SDOCODE.x == 1007102] <- "KAMALPUR"
merged_tsecl_fedco$Circle[merged_tsecl_fedco$SDOCODE.x == 1007102] <- "DHALAI"

merged_tsecl_fedco$SubdivisionName_clean[merged_tsecl_fedco$SDOCODE.x == 1002103] <- "BODHJUNGNAGAR"
merged_tsecl_fedco$Division[merged_tsecl_fedco$SDOCODE.x == 1002103] <- "JIRANIA"
merged_tsecl_fedco$Circle[merged_tsecl_fedco$SDOCODE.x == 1002103] <- "CIRCLE 2 (W. TRIPURA)"

# Create csv files
write.csv(merged_fedco,"merged_fedco.csv")
write.csv(merged_tsecl_fedco,"merged_tsecl_fedco.csv")

########################Cleaning Completed########################