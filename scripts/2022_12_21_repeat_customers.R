####################  Load libraries ####################
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(wesanderson)
library(waffle)
library(patchwork)

#################### Import merged TSECL, FEDCO data ####################
merged_tsecl <- read_csv(here("data-preped", "merged_tsecl.csv"))
merged_fedco <- read_csv(here("data-preped", "merged_fedco.csv"))

#################### Create Custom Theme ####################

theme_tsecl <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      # (panel.grid.major = element_blank(),)   #strip major gridlines 
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 28,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,                #center align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 24),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 17,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 24),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 22),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.title = element_text(size = 20),     # Legend
      legend.text = element_text(size = 16, hjust = 0)
      
    )
}


####################  TSECL Repeat Consumers ####################

# Clean Mobile Numbers
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 0] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 1] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 9999999999] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 1234567890] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 91381231] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 91381232] <- NA
merged_tsecl$MOBILENO[merged_tsecl$MOBILENO == 91381251] <- NA

# Create df for analysis
df_tsecl <- merged_tsecl[,c("SubdivisionName_clean", "Division","Circle", "MOBILENO",
                      "ComplaintType", "SubComplaintType","OutageType", "Rectification", 
                      "Cause", "ComplaintDate_clean", "ComplaintCloseDate_clean")] 


# Viz 0: TSECL unique repeaters (32,068) 

df_tsecl |> 
  group_by(MOBILENO) |> 
  mutate(n = n()) |>  
  filter(n > 1) |>
  filter(!is.na(MOBILENO)) |> 
  count()  

df_tsecl |>              # Non-repeaters 51,451
  group_by(MOBILENO) |> 
  mutate(n = n()) |>  
  filter(n == 1) |>
  filter(!is.na(MOBILENO)) |> 
  count()  

viz0 <- c(Repeaters = 32068, 'Non-Repeaters' = 51451)

waffle(viz0/1000, rows = 6, size = 0.2, colors = c("#e09500", "grey"), 
       xlab = "1 square is equal to 1000 Complainants")
  
# TSECL total complaints by repeaters (1,17,593 answer)
df_tsecl |> 
  group_by(MOBILENO) |> 
  mutate(n = n()) |>  
  filter(n == 3) |>            # change n to get nn below 
  filter(!is.na(MOBILENO)) |> 
  select(MOBILENO,n) |> 
  distinct() |> 
  pull(n) |> 
  sum()

# Complaints when n=1 nn=51451, n=2 nn=31572, n=3 nn=20232, n=4 nn=13868, n=5+ nn=51921,
viz00 <- c( 
           '1st Time Callers (N=1)' = 51451,
           'Repeat Callers (N=2)' = 31572,
           'Repeat Callers (N=3)' = 20232,
           'Repeat Callers (N=3+)' = 65789
           )

waffle_tsecl <- waffle(viz00/1000, rows = 6, size = 0.2, 
       colors = c("grey", "#ad7300", "#e09500", "#ffb014"), 
       xlab = "TSECL Data from Aug 2020 to Oct 2022; 1 square is equal to 1000 Complaints") +
  theme(legend.position = "bottom")

# Viz 1: TSECL unique repeaters by year (2020:5828, 2021:12322, 2022:12521)
repeat_year_tsecl <-  df_tsecl |> 
  group_by(MOBILENO,Year = year(ComplaintDate_clean)) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Year = year(ComplaintDate_clean)) |> 
  distinct(MOBILENO) |> 
  count()

non_repeat_year_tsecl <-  df_tsecl |> 
  group_by(MOBILENO,Year = year(ComplaintDate_clean)) |> 
  mutate(n = n()) |> 
  filter(n <= 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Year = year(ComplaintDate_clean)) |> 
  distinct(MOBILENO) |> 
  count()


viz1 <- cbind(non_repeat_year_tsecl, repeat_year_tsecl) |> 
  select(c(1,2,4)) |> 
  mutate(
    Year = Year...1,
    Repeaters = n...4,
    "Non-Repeaters" = n...2) |> 
  select(Year, Repeaters, "Non-Repeaters") |> 
  pivot_longer(!Year, names_to = "Type", values_to = "Count") |> 
  group_by(Year) |> 
  mutate(percent = percent(Count / sum(Count), accuracy = 1, trim = FALSE))
  

p1 <- ggplot(viz1, aes(x = Year, y = Count, fill = Type)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "", y = "Count", title = "Proportion of Repeat Callers", 
       subtitle = "TSECL", fill = "") +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = c("grey", "#e09500"),
                    labels = c("First-time Callers", "Repeat Callers")) +  
  geom_text(aes(label = percent), 
            hjust = -0.6, color = "#404040", size = 6,
            position = position_stack(vjust = 0.5)) +
  geom_text(
    aes(label = scales::comma(Count)),
    hjust = 1,
    color = "#7c1419",
    size = 6, position = position_stack(vjust = 0.5)) +
  theme_tsecl() +
  theme(legend.position="bottom") 
p1

  

# Viz2: TSECL total complaints by repeaters grouped by year (2020:16374, 2021:36541, 2022:41922)
repeat_tot_comp_year <-  df_tsecl |> 
  group_by(MOBILENO,Year = year(ComplaintDate_clean)) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, Year, n) |> 
  distinct() |> 
  group_by(Year) |> 
  summarise(repeat_tot_comp =sum(n))

tot_comp_year <-  df_tsecl |> 
  group_by(Year = year(ComplaintDate_clean)) |> 
  summarise(tot_comp = n())

viz2 <- cbind(repeat_tot_comp_year, tot_comp_year) |> 
  select(c(1,2,4)) |> 
  mutate(non_repeat_tot_comp = tot_comp - repeat_tot_comp) |> 
  select(Year, repeat_tot_comp, non_repeat_tot_comp) |> 
  pivot_longer(!Year, names_to = "Type", values_to = "Count") |> 
  group_by(Year) |> 
  mutate(percent = percent(Count / sum(Count), accuracy = 1, trim = FALSE))
  
p2 <- ggplot(viz2, aes(x = Year, y = Count, fill = Type)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "", y = "Count", subtitle = "TSECL", fill = "", 
       title = "Proportion of Complaints Registered by Repeat Callers") +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = c("grey", "#e09500"), 
                    labels = c("No. of Complaints Registered by First-time Callers", 
                              "No. of Complaints Registered by Repeat Callers")) +  
  geom_text(aes(label = percent), 
            hjust = -0.6, color = "#404040", size = 6, 
            position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = scales::comma(Count)),
    hjust = 1, position = position_stack(vjust = 0.5),
    color = "#7c1419", size = 6) +
  theme_tsecl() +
  theme(legend.position="bottom")
p2

# TSECL total unique repeaters by Circle (total 34582)
df_tsecl |> 
  group_by(MOBILENO, Circle) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Circle) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Circle (1 lakh in circle-1)
df_tsecl |> 
  group_by(MOBILENO, Circle) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, Circle, n) |> 
  distinct() |> 
  group_by(Circle) |> 
  summarise(nn =sum(n))

# TSECL total unique repeaters grouped by Division
df_tsecl |> 
  group_by(MOBILENO, Division) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Division) |> 
  distinct(MOBILENO) |> 
  count(sort=T)


# TSECL total complaints by repeaters grouped by Division 
df_tsecl |> 
  group_by(MOBILENO, Division) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, Division, n) |> 
  distinct() |> 
  group_by(Division) |> 
  summarise(nn =sum(n))
  

# TSECL total unique repeaters grouped by Subdivision
df_tsecl |> 
  group_by(MOBILENO, SubdivisionName_clean) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(SubdivisionName_clean) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Subdivision 
df_tsecl |> 
  group_by(MOBILENO, SubdivisionName_clean) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, SubdivisionName_clean, n) |> 
  distinct() |> 
  group_by(SubdivisionName_clean) |> 
  summarise(nn =sum(n))


# TSECL total unique repeaters grouped by Complaint Type
df_tsecl |> 
  group_by(MOBILENO, ComplaintType) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(ComplaintType) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Complaint Type 
df_tsecl |> 
  group_by(MOBILENO, ComplaintType) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, ComplaintType, n) |> 
  distinct() |> 
  group_by(ComplaintType) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

# TSECL total unique repeaters grouped by Outage Type
df_tsecl |> 
  group_by(MOBILENO, OutageType) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(OutageType) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Outage Type
df_tsecl |> 
  group_by(MOBILENO, OutageType) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, OutageType, n) |> 
  distinct() |> 
  group_by(OutageType) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

# TSECL total unique repeaters grouped by Cause Type
df_tsecl |> 
  group_by(MOBILENO, Cause) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Cause) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Cause Type
df_tsecl |> 
  group_by(MOBILENO, Cause) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, Cause, n) |> 
  distinct() |> 
  group_by(Cause) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

# TSECL total unique repeaters grouped by Rectification Type
df_tsecl |> 
  group_by(MOBILENO, Rectification) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  ungroup() |> 
  group_by(Rectification) |> 
  distinct(MOBILENO) |> 
  count(sort=T)

# TSECL total complaints by repeaters grouped by Rectification Type
df_tsecl |> 
  group_by(MOBILENO, Rectification) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(MOBILENO)) |>
  select(MOBILENO, Rectification, n) |> 
  distinct() |> 
  group_by(Rectification) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

######################### FEDCO Repeat Consumers ######################### 

# Create df for analysis
df_fedco <- merged_fedco[, c("Caller_number", "Category_name", "Subcategory_A",
                             "Subcategory_B", "Subdivision", "Division", 
                             "Complaintlogtime_clean")]
  
df_fedco <- df_fedco |>    # Add circle column
  mutate(
    Circle =
      ifelse(Division == "Ambassa", "DHALAI",
        ifelse(Division == "Manu", "DHALAI",
          ifelse(Division == "Mohanpur", "CIRCLE 2 (W. TRIPURA)",
            ifelse(Division == "Sabroom", "BELONIA", NA)))))

# Viz0: FEDCO unique repeaters (4081)
df_fedco |> 
  group_by(Caller_number) |> 
  mutate(n = n()) |>  
  filter(n > 1) |>
  filter(!is.na(Caller_number)) |> 
  count()     

# Fedco total complaints by repeaters (32944)
df_fedco |> 
  group_by(Caller_number) |> 
  mutate(n = n()) |>  
  filter(n== 3) |>             # change n to get nn below
  filter(!is.na(Caller_number)) |> 
  select(Caller_number,n) |> 
  distinct() |> 
  pull(n) |> 
  sum()

# Complaints when n=1 nn=7344, n=2 nn=3638, n=3 nn=2304, n=4+ nn=27002
viz00_fedco <- c( 
  '1st time Callers (N=1)' = 7344,
  'Repeat Callers (N=2)' = 3638,
  'Repeat Callers (N=3)' = 2304,
  'Repeat Callers (N=3+)' = 27002
)

waffle_fedco <- waffle(viz00_fedco/1000, rows = 6, size = 0.2, 
                       colors = c("grey", "#ad7300", "#e09500", "#ffb014"), 
                       xlab = "FEDCO Data from Sep 2020 to Oct 2022; 1 square is equal to 1000 Complaints")+
                theme(legend.position = "bottom")



# Viz1: FEDCO unique repeaters by year (2020:261, 2021:1172, 2022:2798)
repeat_year_fedco <- df_fedco |> 
  group_by(Caller_number,Year = year(Complaintlogtime_clean)) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Year = year(Complaintlogtime_clean)) |> 
  distinct(Caller_number) |> 
  count()

non_repeat_year_fedco <-  df_fedco |> 
  group_by(Caller_number,Year = year(Complaintlogtime_clean)) |> 
  mutate(n = n()) |> 
  filter(n <= 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Year = year(Complaintlogtime_clean)) |> 
  distinct(Caller_number) |> 
  count()

viz_1 <- cbind(non_repeat_year_fedco, repeat_year_fedco) |> 
  select(c(1,2,4)) |> 
  mutate(
    Year = Year...1,
    Repeaters = n...4,
    "Non-Repeaters" = n...2) |> 
  select(Year, Repeaters, "Non-Repeaters") |> 
  pivot_longer(!Year, names_to = "Type", values_to = "Count") |> 
  group_by(Year) |> 
  mutate(percent = percent(Count / sum(Count), accuracy = 1, trim = FALSE))

p3 <- ggplot(viz_1, aes(x = Year, y = Count, fill = Type)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "", y = "Count", subtitle = "FEDCO", fill = "", 
       title = "Proportion of Repeat Callers") +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = c("grey", "#e09500"),
                    labels = c("First-time Callers", "Repeat Callers")) +  
  geom_text(aes(label = percent), 
            hjust = -0.6, color = "#404040", size = 6,
            position = position_stack(vjust = 0.5)) +
  geom_text(
    aes(label = scales::comma(Count)),
    hjust = 1,
    color = "#7c1419",
    size = 6, position = position_stack(vjust = 0.5)) +
  theme_tsecl() +
  theme(legend.position="bottom")
p3

# Fedco total complaints by repeaters grouped by year (2020:1349, 2021:5645, 2022:24643)
repeat_tot_comp_year_fedco <- df_fedco |> 
  group_by(Caller_number,Year = year(Complaintlogtime_clean)) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Year, n) |> 
  distinct() |> 
  group_by(Year) |> 
  summarise(repeat_tot_comp =sum(n))

tot_comp_year_fedco <-  df_fedco |> 
  group_by(Year = year(Complaintlogtime_clean)) |> 
  summarise(tot_comp = n())

viz_2 <- cbind(repeat_tot_comp_year_fedco, tot_comp_year_fedco) |> 
  select(c(1,2,4)) |> 
  mutate(non_repeat_tot_comp = tot_comp - repeat_tot_comp) |> 
  select(Year, repeat_tot_comp, non_repeat_tot_comp) |> 
  pivot_longer(!Year, names_to = "Type", values_to = "Count") |> 
  group_by(Year) |> 
  mutate(percent = percent(Count / sum(Count), accuracy = 1, trim = FALSE))

p4 <- ggplot(viz_2, aes(x = Year, y = Count, fill = Type)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "", y = "Count", subtitle = "FEDCO", fill = "",
       title = "Proportion of Complaints Registered by Repeat Callers") +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = c("grey", "#e09500"), 
                    labels = c("No. of Complaints Registered by First-time Callers", 
                               "No. of Complaints Registered by Repeat Callers")) +  
  geom_text(aes(label = percent), 
            hjust = -0.6, color = "#404040", size = 6, 
            position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = scales::comma(Count)),
            hjust = 1, position = position_stack(vjust = 0.5),
            color = "#7c1419", size = 6) +
  theme_tsecl() +
  theme(legend.position="bottom")
p4

# Fedco total unique repeaters by Circle
df_fedco |> 
  group_by(Caller_number, Circle) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Circle) |> 
  distinct(Caller_number) |> 
  filter(!is.na(Circle)) |>
  count(sort=T)

# Fedco total complaints by repeaters grouped by Circle 
df_fedco |> 
  group_by(Caller_number, Circle) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Circle, n) |> 
  distinct() |> 
  group_by(Circle) |> 
  summarise(nn =sum(n)) |> 
  filter(!is.na(Circle))

# Fedco total unique repeaters by Division
df_fedco |> 
  group_by(Caller_number, Division) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Division) |> 
  distinct(Caller_number) |> 
  count(sort=T)

# Fedco total complaints by repeaters grouped by Division 
df_fedco |> 
  group_by(Caller_number, Division) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Division, n) |> 
  distinct() |> 
  group_by(Division) |> 
  summarise(nn =sum(n)) |> 
  filter(!is.na(Division))|> 
  arrange(desc(nn))

# Fedco total unique repeaters by Subdivision
df_fedco |> 
  group_by(Caller_number, Subdivision) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Subdivision) |> 
  distinct(Caller_number) |> 
  count(sort=T)

# Fedco total complaints by repeaters grouped by Subdivision 
df_fedco |> 
  group_by(Caller_number, Subdivision) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Subdivision, n) |> 
  distinct() |> 
  group_by(Subdivision) |> 
  summarise(nn =sum(n)) |> 
  filter(!is.na(Subdivision))|> 
  arrange(desc(nn))

# Fedco total unique repeaters by Complaint Type
df_fedco |> 
  group_by(Caller_number, Category_name) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Category_name) |> 
  distinct(Caller_number) |> 
  count(sort=T)

# Fedco total complaints by repeaters grouped by Complaint Type
df_fedco |> 
  group_by(Caller_number, Category_name) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Category_name, n) |> 
  distinct() |> 
  group_by(Category_name) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

# Fedco total unique repeaters by Subcomplaint Type A
df_fedco |> 
  group_by(Caller_number, Subcategory_A) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Subcategory_A) |> 
  distinct(Caller_number) |> 
  count(sort=T)

# Fedco total complaints by repeaters grouped by Subcomplaint Type A
df_fedco |> 
  group_by(Caller_number, Subcategory_A) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Subcategory_A, n) |> 
  distinct() |> 
  group_by(Subcategory_A) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))

# Fedco total unique repeaters by Subcomplaint Type B
df_fedco |> 
  group_by(Caller_number, Subcategory_B) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  ungroup() |> 
  group_by(Subcategory_B) |> 
  distinct(Caller_number) |> 
  count(sort=T)

# Fedco total complaints by repeaters grouped by Subcomplaint Type B
df_fedco |> 
  group_by(Caller_number, Subcategory_B) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  filter(!is.na(Caller_number)) |>
  select(Caller_number, Subcategory_B, n) |> 
  distinct() |> 
  group_by(Subcategory_B) |> 
  summarise(nn =sum(n)) |> 
  arrange(desc(nn))


######################## Time Difference between calls  #######################
# Filter for repeat callers in TSECL
repeaters_tsecl <- df_tsecl |> 
  filter(!is.na(MOBILENO)) |> 
  filter(ComplaintCloseDate_clean - ComplaintDate_clean > 0) |> 
  group_by(MOBILENO) |> 
  mutate(n = n()) |>  
  filter(n > 1) |>
  ungroup() 

# Create lag columns in TSECL
lag_df_tsecl <- repeaters_tsecl |>  
  select(MOBILENO, ComplaintDate_clean) |> 
  arrange(MOBILENO, ComplaintDate_clean) |>
  group_by(MOBILENO) |> 
  mutate(lag_complaint_date = lag(ComplaintDate_clean)) |>
  mutate(diff_lag = ComplaintDate_clean - lag_complaint_date) |> 
  ungroup()

# calculate summary stats in TSECL
call_diff_time_tsecl <- lag_df_tsecl |> 
  group_by(MOBILENO) |> 
  summarise(total_calls = n(),
            ave = mean(diff_lag, na.rm = T),
            med = median(diff_lag, na.rm = T),
            average_days = round(ave/86400, 2),
            median_days = round(med/86400, 2))

call_diff_time_tsecl$median_days_cat[call_diff_time_tsecl$med < 3600] <- "< 1hr"
call_diff_time_tsecl$median_days_cat[call_diff_time_tsecl$med >= 3600 & call_diff_time_tsecl$med < 18000] <- "1-5hr"
call_diff_time_tsecl$median_days_cat[call_diff_time_tsecl$med >= 18000 & call_diff_time_tsecl$med < 86400] <- "5hr-1day"
call_diff_time_tsecl$median_days_cat[call_diff_time_tsecl$med >= 86400 & call_diff_time_tsecl$med <= 604800] <- "1day-1week"
call_diff_time_tsecl$median_days_cat[call_diff_time_tsecl$med > 604800] <- ">1week"

call_diff_time_tsecl |> 
  count(median_days_cat) |> 
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(
    x = factor(median_days_cat, levels = c('< 1hr','1-5hr','5hr-1day','1day-1week','>1week')), 
             y = n)) +
  labs(
    title = "Time Lag Between Consecutive Complaints Registered by Repeat Caller (TSECL)",
    x = "Median Time Between Consecutive Complaints",
    y = "Number of Unique Repeat Callers",
    subtitle = "",
    caption = ""
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 0.5,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 0.5,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 


call_diff_time_tsecl |> 
  group_by(median_days_cat) |> 
  summarise(tot_call = sum(total_calls)) |> 
  mutate(percent = percent(tot_call / sum(tot_call), accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(
    x = factor(median_days_cat, levels = c('< 1hr','1-5hr','5hr-1day','1day-1week','>1week')), 
    y = tot_call)) +
  labs(
    title = "Time Lag Between Consecutive Complaints 
    Registered by Repeat Caller (TSECL)",
    x = "Median Time Between Consecutive Complaints",
    y = "Number of Complaints",
    subtitle = "N = 1,12,631",
    caption = "Time Period: August 2020 to October 2022."
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), breaks = c(30000, 60000, 90000)) +
  geom_text(
    aes(label = percent),
    hjust = 0.5,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(tot_call)),
    hjust = 0.5,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 


# Filter for repeat callers in FEDCO
repeaters_fedco <- df_fedco |> 
  filter(!is.na(Caller_number)) |> 
  group_by(Caller_number) |> 
  mutate(n = n()) |>  
  filter(n > 1) |>
  ungroup()

# Create lag columns in FEDCO
lag_df_fedco <- repeaters_fedco |>  
  select(Caller_number, Complaintlogtime_clean) |> 
  arrange(Caller_number, Complaintlogtime_clean) |>
  group_by(Caller_number) |> 
  mutate(lag_complaint_date = lag(Complaintlogtime_clean)) |>
  mutate(diff_lag = Complaintlogtime_clean - lag_complaint_date) |> 
  ungroup()

# calculate summary stats in FEDCO
call_diff_time_fedco <- lag_df_fedco |> 
  group_by(Caller_number) |> 
  summarise(total_calls = n(),
            ave = mean(diff_lag, na.rm = T),
            med = median(diff_lag, na.rm = T),
            average_days = round(ave/86400, 2),
            median_days = round(med/86400, 2))

call_diff_time_fedco$median_days_cat[call_diff_time_fedco$med < 3600] <- "< 1hr"
call_diff_time_fedco$median_days_cat[call_diff_time_fedco$med >= 3600 & call_diff_time_fedco$med < 18000] <- "1-5hr"
call_diff_time_fedco$median_days_cat[call_diff_time_fedco$med >= 18000 & call_diff_time_fedco$med < 86400] <- "5hr-1day"
call_diff_time_fedco$median_days_cat[call_diff_time_fedco$med >= 86400 & call_diff_time_fedco$med <= 604800] <- "1day-1week"
call_diff_time_fedco$median_days_cat[call_diff_time_fedco$med > 604800] <- ">1week"

call_diff_time_fedco |> 
  count(median_days_cat) |> 
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(
    x = factor(median_days_cat, levels = c('< 1hr','1-5hr','5hr-1day','1day-1week','>1week')), 
    y = n)) +
  labs(
    title = "Time Duration of Repeat Calls (FEDCO)",
    x = "Median Time between Repeat Calls",
    y = "Number of Unique Repeat Callers",
    subtitle = "",
    caption = ""
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 0.5,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 0.5,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 


call_diff_time_fedco |> 
  group_by(median_days_cat) |> 
  summarise(tot_call = sum(total_calls)) |> 
  mutate(percent = percent(tot_call / sum(tot_call), accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(
    x = factor(median_days_cat, levels = c('< 1hr','1-5hr','5hr-1day','1day-1week','>1week')), 
    y = tot_call)) +
  labs(
    title = "Time Lag Between Consecutive Complaints 
    Registered by Repeat Caller (FedCo)",
    x = "Median Time Between Consecutive Complaints",
    y = "Number of Complaints",
    subtitle = "N = 32,944",
    caption = "Time Period: September 2020 to October 2022"
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), breaks = c(30000, 60000, 90000)) +
  geom_text(
    aes(label = percent),
    hjust = 0.5,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(tot_call)),
    hjust = 0.5,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 

######################## END #######################
# SAI total repeat (No way to figure out)

# Get 2 tsecl & 2 fedco plots together
(p1 + p2) / (p3 + p4)

# Get tsecl & fedco waffle plots together
waffle_tsecl / waffle_fedco +  plot_layout(widths = c(1, 1))



