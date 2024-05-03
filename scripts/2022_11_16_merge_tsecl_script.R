# Load libraries
library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(fs)

# File structure 
fs::dir_tree()

# File names of the raw data files 
list.files(here("data-raw", "FY 20-21 CSV")) -> files_20_21
list.files(here("data-raw", "FY 21-22 CSV")) -> files_21_22
list.files(here("data-raw", "FY 22-23 CSV")) -> files_22_23

# File addresses 
all_files_address <- c(here ("data-raw", "FY 20-21 CSV", files_20_21),
                       here("data-raw", "FY 21-22 CSV", files_21_22),
                       here("data-raw", "FY 22-23 CSV", files_22_23))


get_column_names <- function(file_add) {
                      read_csv(file_add) |> 
                        names()}

reduce(map(all_files_address,get_column_names), c) |> table() # if the frequency of all column names are same then we have the same column names across all files.

read_and_add_col <- function(file_add){
                      read_csv(file_add, col_types = list("i", "i", "c", "c", "c", "c","c","c", "n","n","n","c","c","n","c","f","f","f","f","n","c", "c","n","n","f","f","c","c")
) |>
                        mutate(
                          month_year = stringr::str_remove_all(stringr::str_remove_all(stringr::str_extract(file_add, ".............$"), "csv"), "[:punct:]") # cross check with the complaint Date variable if the month year from file name are same 
                        )
                    }

map_dfr(all_files_address, read_and_add_col) -> collated_grievrance_data
 
summary(collated_grievrance_data) # NAs in various columns


#################### Clean dataset ####################

# Convert factor columns to character
collated_grievrance_data$SubdivisionName <- as.character(collated_grievrance_data$SubdivisionName)
collated_grievrance_data$ComplaintType <- as.character(collated_grievrance_data$ComplaintType)
collated_grievrance_data$SubComplaintType <- as.character(collated_grievrance_data$SubComplaintType)
collated_grievrance_data$ComplaintSource <- as.character(collated_grievrance_data$ComplaintSource)
collated_grievrance_data$OutageType <- as.character(collated_grievrance_data$OutageType)
collated_grievrance_data$ConsumerType <- as.character(collated_grievrance_data$ConsumerType)

collated_grievrance_data <-
  collated_grievrance_data[-c(22554, 22555, 22556), ]  #remove row numbers & blank rows

# converting to date time columns
collated_grievrance_data <- collated_grievrance_data |>
  mutate(
    ComplaintCloseDate_clean =
      parse_date_time(ComplaintCloseDate, c("%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M")),
    ComplaintDate_clean =
      parse_date_time(ComplaintDate, c("%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M"))
  )

# Clean Subdivision names
collated_grievrance_data <- collated_grievrance_data |> mutate(
  SubdivisionName_clean = str_remove(pattern = "ESD Office,"
                                     , string = SubdivisionName),
  SubdivisionName_clean = str_remove(pattern = "ESD",
                                     string = SubdivisionName_clean),
  SubdivisionName_clean = str_to_upper(SubdivisionName_clean),
  SubdivisionName_clean = str_remove_all(pattern = "_",
                                         string = SubdivisionName_clean),
  SubdivisionName_clean = str_remove_all(pattern = "-",
                                         string = SubdivisionName_clean),
  SubdivisionName_clean = str_trim(string = SubdivisionName_clean, side =
                                     "left"),
  SubdivisionName_clean = str_squish(SubdivisionName_clean),
  SubdivisionName_clean = str_replace(
    pattern = "AMTOLI",
    replacement = "AMTALI",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BOGAFA",
    replacement = "BAGAFA",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BANAMALI I",
    replacement = "BANAMALIPUR 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BANAMALIPUR 1I",
    replacement = "BANAMALIPUR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BANAMALI II",
    replacement = "BANAMALIPUR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BANAMALIPURI",
    replacement = "BANAMALIPUR 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BANAMALIPURII",
    replacement = "BANAMALIPUR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BILONIA",
    replacement = "BELONIA",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BISHALGARH I",
    replacement = "BISHALGARH 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BISHALGARH1",
    replacement = "BISHALGARH 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BISHALGARH2",
    replacement = "BISHALGARH 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BISHGARHII",
    replacement = "BISHALGARH 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BISRAMGANJ",
    replacement = "BISHRAMGANJ",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BORDOWALIIIIRURAL",
    replacement = "BORDOWALI RURAL",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BORDOWALIVIURBAN",
    replacement = "BORDOWALI URBAN",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BORDUALI URBAN",
    replacement = "BORDOWALI URBAN",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "BORDUALI RURAL",
    replacement = "BORDOWALI RURAL",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "CAPITALCOMPLEX",
    replacement = "CAPITAL COMPLEX",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "CC",
    replacement = "CAPITAL COMPLEX",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "CHAMPAKNAGARMANDWIKHUMLWNG",
    replacement = "CHAMPAKNAGAR",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "DHARMANAGARI",
    replacement = "DHARMANAGAR 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "DHARMANAGARII",
    replacement = "DHARMANAGAR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "DHARMANAGAR 1I",
    replacement = "DHARMANAGAR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "DURGACHOUMOHONI",
    replacement = "DURGACHOWMOHANI_R",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "DURGACHOWMUHANIR",
    replacement = "DURGACHOWMOHANI_R",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "GANDACHERRA",
    replacement = "GANDACHARA",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "HRISHYAMUKHJOLAIBARI",
    replacement = "HRISHYAMUKH",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "JATANBARIKARBOOK",
    replacement = "JATANBARI",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "JUBARAJNAGARDHARMANAGARII",
    replacement = "JUBARAJNAGAR",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "JUBARAJNAGARDHARMANAGAR 1I",
    replacement = "JUBARAJNAGAR",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "KAILASAHARI",
    replacement = "KAILASHAHAR 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "KAILASAHARII",
    replacement = "KAILASHAHAR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "KAILASHAHAR 1I",
    replacement = "KAILASHAHAR 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "MADHUPURSEKERKOTE",
    replacement = "MADHUPUR",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "MELAGARH",
    replacement = "MELAGHAR",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "PADMABILL",
    replacement = "PADMABIL",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "PRATAPGHAR",
    replacement = "PRATAPGARH",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "SALEMADURGACHOWMOHANI",
    replacement = "SALEMA",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "TELIAMURAI",
    replacement = "TELIAMURA 1",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "TELIAMURAII",
    replacement = "TELIAMURA 2",
    string = SubdivisionName_clean
  ),
  SubdivisionName_clean = str_replace(
    pattern = "TELIAMURA 1I",
    replacement = "TELIAMURA 2",
    string = SubdivisionName_clean
  )
)

collated_grievrance_data$SubdivisionName_clean[collated_grievrance_data$SubdivisionName_clean ==
                                     "BANAMALIPUR 1I"] <- "BANAMALIPUR 2"
collated_grievrance_data$SubdivisionName_clean[collated_grievrance_data$SubdivisionName_clean ==
                                     "JUBARAJNAGARDHARMANAGAR 2"] <- "JUBARAJNAGAR"
collated_grievrance_data$SubdivisionName_clean[collated_grievrance_data$SubdivisionName_clean ==
                                                 "DURGACHOWMOHANI"] <- "DURGACHOWMOHANI_R"
collated_grievrance_data |> pull(SubdivisionName_clean) |>  unique() |> sort()


#import subdivision database to merge
subdivision_mastersheet <-
  read_csv(here("OtherFiles", "subdivison_mastersheet.csv"))

collated_grievrance_data <- merge(
  # Left join
  x = collated_grievrance_data,
  y = subdivision_mastersheet,
  by.x = "SubdivisionName_clean",
  by.y = "Subdivision",
  all.x = T
)

collated_grievrance_data <- collated_grievrance_data[,-33]  # Remove serial number column

# Fixing NAs in Subdivision Division Circle
collated_grievrance_data$SubdivisionName_clean[collated_grievrance_data$SDOCODE.x == 1007102] <-
  "SALEMA"
collated_grievrance_data$Division[collated_grievrance_data$SDOCODE.x == 1007102] <-
  "KAMALPUR"
collated_grievrance_data$Circle[collated_grievrance_data$SDOCODE.x == 1007102] <- "DHALAI"
collated_grievrance_data$SubdivisionName_clean[collated_grievrance_data$SDOCODE.x == 1002103] <-
  "BODHJUNGNAGAR"
collated_grievrance_data$Division[collated_grievrance_data$SDOCODE.x == 1002103] <-
  "JIRANIA"
collated_grievrance_data$Circle[collated_grievrance_data$SDOCODE.x == 1002103] <-
  "CIRCLE 2 (W. TRIPURA)"


# Clean Categories
# create category for no supply
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "All Area no power-HT issue"] <- "All Area No Power"
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "All Area no power-LT issue"] <- "All Area No Power"

# Merge similar theft sub category
collated_grievrance_data$SubComplaintType[collated_grievrance_data$SubComplaintType == "Misuse Of Power Supply/Extra Service Line By Consu"] <-
  "Misuse Of Power Supply/Extra Service Line By Consumer"

# Merge all meter subcomplaints to meter related category
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "Meter Is Not Assembled Correctly"] <-
  "Meter Related"
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "Meter Is Out Of Order/Burnt/Stolen"] <-
  "Meter Related"
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "Meter Is Running Fast"] <-
  "Meter Related"

# merge subcomplaint types
collated_grievrance_data$SubComplaintType[collated_grievrance_data$SubComplaintType == "Bent Broken Pole"] <-
  "Bent/Broken Pole"
collated_grievrance_data$SubComplaintType[collated_grievrance_data$SubComplaintType == "Broken Damaged Wire"] <-
  "Broken/Damaged Wire"

# Change 'other technical' from subcategory to category
collated_grievrance_data$ComplaintType[collated_grievrance_data$SubComplaintType == "Other Technical" &
                             collated_grievrance_data$ComplaintType == "Other Complaint"] <-
  "Other Technical"

# Change Complaint & Subcomplaint Type to upper case
collated_grievrance_data$ComplaintType <- toupper(collated_grievrance_data$ComplaintType)
collated_grievrance_data$SubComplaintType <-
  toupper(collated_grievrance_data$SubComplaintType)


# Check complaint & Subcomplaint type
collated_grievrance_data |>
  group_by(ComplaintType, SubComplaintType) |>
  summarise(n = n()) |>  arrange(desc(ComplaintType)) |>
  print(n = Inf)

# Clean Outage Type
collated_grievrance_data$OutageType <- toupper(collated_grievrance_data$OutageType)

# Clean Cause
collated_grievrance_data$Cause <- toupper(collated_grievrance_data$Cause)

collated_grievrance_data$Cause[collated_grievrance_data$Cause == "<<=== SELECT ===>>"] <- NA
collated_grievrance_data$Cause[collated_grievrance_data$Cause == "TRANSFORMER OVER LOAD"] <- "TRANSFORMER OVERLOAD"
collated_grievrance_data$Cause[collated_grievrance_data$Cause == "HIGH VOLTAGE ( NEUTRAL FAIL)"] <- "HIGH VOLTAGE (NEUTRAL FAIL)"
collated_grievrance_data$Cause[collated_grievrance_data$Cause == "HIGH VOLTAGE ( UNBALANCE LOAD )"] <- "HIGH VOLTAGE (UNBALANCE LOAD)"

collated_grievrance_data$Cause[collated_grievrance_data$Cause == "TREE BRANCHES TOUCH TO THE PHASE"] <- "TREE BRANCH TOUCHING PHASE"
collated_grievrance_data$Cause[collated_grievrance_data$Cause == "LOW VOLTAGE ( D.O PROBLEM)"] <- "LOW VOLTAGE (D.O. PROBLEM)"
collated_grievrance_data$Cause[collated_grievrance_data$Cause == "ALIGNING G.O"] <- "ALIGNING G.O."
collated_grievrance_data$Cause <- str_squish(collated_grievrance_data$Cause)   

# Clean Rectification
collated_grievrance_data$Rectification[collated_grievrance_data$Rectification == "<<=== Select ===>>"] <- NA
collated_grievrance_data$Rectification[collated_grievrance_data$Rectification == "Replace G.O"] <- "Replace G.O."
collated_grievrance_data$Rectification[collated_grievrance_data$Rectification == "D.O Fuse replace"] <- "D.O. Fuse replace"
collated_grievrance_data$Rectification <- toupper(collated_grievrance_data$Rectification)
collated_grievrance_data$Rectification <- str_squish(collated_grievrance_data$Rectification)   

# Add priority column

collated_grievrance_data <- collated_grievrance_data |> mutate(Priority = 
    if_else(ComplaintType == "ALL AREA NO POWER", "High",
    if_else(ComplaintType == "BILL RELATED", "Low",
    if_else(ComplaintType == "COMMERCIAL REQUEST", "Medium",
    if_else(ComplaintType == "ENERGY THEFT", "Low",
    if_else(ComplaintType == "METER RELATED", "Low",
    if_else(ComplaintType == "NO CURRENT COMPLAINT", "Medium",
    if_else(ComplaintType == "OTHER COMPLAINT", "High",
    if_else(ComplaintType == "SAFETY RELATED", "High",
    if_else(ComplaintType == "SERVICE LINE RELATED", "Medium",
    if_else(ComplaintType == "THEFT INFORMATION", "Low",
    if_else(ComplaintType == "TRANSFORMER FAILURE", "High", "No Priority"
))))))))))))

##################### Save as csv file #####################
write.csv(collated_grievrance_data, "data-preped/merged_tsecl.csv")

