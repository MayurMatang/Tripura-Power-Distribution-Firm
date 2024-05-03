####################  Load libraries & Data ####################
library(tidyverse)
library(here)
library(lubridate)
library(scales)

merged_tsecl <- read_csv(here("data-preped", "merged_tsecl.csv"))


#################### EDA Outage Type ####################

# Outage Type
merged_tsecl |> group_by(OutageType) |> count(sort=T) |> view()

# Location
table(merged_tsecl$Circle, merged_tsecl$OutageType) |>  view()
table(merged_tsecl$Division, merged_tsecl$OutageType) |>  view()
table(merged_tsecl$SubdivisionName_clean, merged_tsecl$OutageType) |>  view()

merged_tsecl |> group_by(Circle, Division, SubdivisionName_clean, OutageType) |> count() |> view()

# Year/Month
merged_tsecl <- merged_tsecl |> mutate(Year=year(ComplaintDate_clean))
merged_tsecl <- merged_tsecl |> mutate(Month=month(ComplaintDate_clean))

table(merged_tsecl$Year, merged_tsecl$OutageType) |>  view()
table(merged_tsecl$Month, merged_tsecl$OutageType) |>  view()


# Feeder Name 
table(merged_tsecl$FEEDERNAME, merged_tsecl$OutageType) |>  view()

# Complaint Type
table(merged_tsecl$ComplaintType, merged_tsecl$OutageType) |>  view()
table(merged_tsecl$Priority, merged_tsecl$OutageType) |>  view()
table(merged_tsecl$SubComplaintType, merged_tsecl$OutageType) |>  view()

# Consumer Type
table(merged_tsecl$ConsumerType, merged_tsecl$OutageType) |>  view()

# Rectification
table(merged_tsecl$Rectification, merged_tsecl$OutageType) |> view()

# Cause
table(merged_tsecl$Cause, merged_tsecl$OutageType) |> view()

# Rectification & Cause
merged_tsecl |> group_by(OutageType, Cause, Rectification) |> count(sort=T) |>  view()


#################### EDA Cause ####################

# Cause
merged_tsecl |> group_by(Cause) |> count(sort=T) |> view()

# Location
table(merged_tsecl$Circle, merged_tsecl$Cause) |>  view()
table(merged_tsecl$Division, merged_tsecl$Cause) |>  view()
table(merged_tsecl$SubdivisionName_clean, merged_tsecl$Cause) |>  view()

merged_tsecl |> group_by(Circle, Division, SubdivisionName_clean, OutageType) |> count() |> view()

# Year/Month
table(merged_tsecl$Year, merged_tsecl$Cause) |>  view()
table(merged_tsecl$Month, merged_tsecl$Cause) |>  view()

# Feeder Name
table(merged_tsecl$FEEDERNAME, merged_tsecl$Cause) |>  view()

# Complaint Type
table(merged_tsecl$ComplaintType, merged_tsecl$Cause) |>  view()
table(merged_tsecl$Priority, merged_tsecl$Cause) |>  view()
table(merged_tsecl$SubComplaintType, merged_tsecl$Cause) |>  view()

# Consumer Type
table(merged_tsecl$ConsumerType, merged_tsecl$Cause) |>  view()

# Rectification
table(merged_tsecl$Rectification, merged_tsecl$Cause) |> view()


#################### EDA Rectification ####################

# Rectification
merged_tsecl |> group_by(Rectification) |> count(sort=T) |> view()

# Location
table(merged_tsecl$Circle, merged_tsecl$Rectification) |>  view()
table(merged_tsecl$Division, merged_tsecl$Rectification) |>  view()
table(merged_tsecl$SubdivisionName_clean, merged_tsecl$Rectification) |>  view()

merged_tsecl |> group_by(Circle, Division, SubdivisionName_clean, Rectification) |> count() |> view()

# Year/Month
table(merged_tsecl$Year, merged_tsecl$Rectification) |>  view()
table(merged_tsecl$Month, merged_tsecl$Rectification) |>  view()

# Feeder Name
table(merged_tsecl$FEEDERNAME, merged_tsecl$Rectification) |>  view()

# Complaint Type
table(merged_tsecl$ComplaintType, merged_tsecl$Rectification) |>  view()
table(merged_tsecl$Priority, merged_tsecl$Rectification) |>  view()
table(merged_tsecl$SubComplaintType, merged_tsecl$Rectification) |>  view()

# Consumer Type
table(merged_tsecl$ConsumerType, merged_tsecl$Rectification) |>  view()

# Time resolution (under 5 hrs) against top 5 Rectification
boxplot <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs <= 5,
         month = month(ComplaintDate_clean),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5)
  

ggplot(boxplot, aes(x = Rectification, y = res_time_hrs)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(25))

# Time resolution (under 5 hrs) against top 5 Rectification facet by year
ggplot(boxplot, aes(x = Rectification, y = res_time_hrs)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(25)) +
  coord_flip() +
  facet_grid(~year)

# Time resolution (under 5 hrs) against top 5 Rectification facet by circle
ggplot(boxplot |> filter(!is.na(Circle)), aes(x = Rectification, y = res_time_hrs)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(17)) +
  facet_wrap(vars(Circle), nrow = 2)+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


# Time resolution (under 5 hrs) against top 5 Rectification facet by division
ggplot(boxplot |> filter(!is.na(Circle)), aes(x = Rectification, y = res_time_hrs)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(20)) +
  facet_wrap(vars(Division), nrow = 5, ncol = 5) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


# Time resolution (over 5 hrs) against top 5 Rectification
boxplot1 <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs <= 5,
         month = month(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 5) |> 
  select(Rectification, res_time_hrs)


ggplot(boxplot1, aes(x = Rectification, y = res_time_hrs)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_format(25))


# Time resolution against top 5 Rectification facet by Subdivision
viz <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time = (ComplaintCloseDate_clean - ComplaintDate_clean)) |> 
  filter(res_time >= 0)
  
viz |> filter(Rectification == "R,Y OR B FUSE REPLACE") |> 
  select(Rectification, res_time, SubdivisionName_clean) |> 
ggplot(aes(x = Rectification, y = res_time)) +
  geom_boxplot() +
  labs(y = "res_time (sec)") + 
  facet_wrap(~SubdivisionName_clean)

viz |> filter(Rectification == "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT") |> 
  select(Rectification, res_time, SubdivisionName_clean) |> 
  ggplot(aes(x = Rectification, y = res_time)) +
  geom_boxplot() +
  labs(y = "res_time (sec)") + 
  facet_wrap(~SubdivisionName_clean)

viz |> filter(Rectification == "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED") |> 
  select(Rectification, res_time, SubdivisionName_clean) |> 
  ggplot(aes(x = Rectification, y = res_time)) +
  geom_boxplot() +
  labs(y = "res_time (sec)") + 
  facet_wrap(~SubdivisionName_clean)

# Time resolution against top 5 Rectification facet by Year
viz <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time = (ComplaintCloseDate_clean - ComplaintDate_clean)) |> 
  filter(res_time >= 0)

viz |> 
  filter(Rectification == "R,Y OR B FUSE REPLACE") |> 
  select(Rectification, res_time, ComplaintDate_clean) |> 
  ggplot(aes(x = Rectification, y = res_time)) +
  geom_boxplot() +
  labs(y = "res_time (sec)") + 
  facet_wrap(~year(ComplaintDate_clean))

# Line chart
viz_line <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time <= 5,
         month = month(ComplaintDate_clean)) |> 
  filter(res_time >= 0) |>
  group_by(Rectification, res_time_under5) |> 
  count(month)

ggplot(viz_line, aes(x = month, y = n, color = Rectification)) +
  geom_line(alpha = 0.7) +
  facet_grid(~res_time_under5) +
  scale_x_continuous(n.breaks = 12)


# Table: Subdivision, No. of Complaints with res time under 5, above 5
table1 <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time >= 0) |> 
  mutate(res_time_cat = 
           if_else(res_time < 5, "under_5",
           if_else(res_time >= 5 & res_time <= 250, "5_to_250",
           if_else(res_time > 250 & res_time <= 3000, "250_to_3000",
           if_else(res_time > 3000, "above_3000", "none"))))) |> 
  select(SubdivisionName_clean, res_time, res_time_cat) |> 
  group_by(SubdivisionName_clean) |> 
  mutate(n_under_5 = sum(res_time_cat == "under_5"),
         n_5_to_250 = sum(res_time_cat == "5_to_250"),                
         n_250_to_3000 = sum(res_time_cat == "250_to_3000"),
         n_above_3000 = sum(res_time_cat == "above_3000")) |> 
  select(SubdivisionName_clean, n_under_5, n_5_to_250, n_250_to_3000, n_above_3000) |> 
  unique() |> 
  group_by(SubdivisionName_clean) |> 
  mutate(sd_under_5_pc = n_under_5/(n_under_5+n_5_to_250+n_250_to_3000+n_above_3000),
         sd_5_to_250_pc = n_5_to_250/(n_under_5+n_5_to_250+n_250_to_3000+n_above_3000),
         sd_250_to_3000_pc = n_250_to_3000/(n_under_5+n_5_to_250+n_250_to_3000+n_above_3000),
         sd_above_3000_pc = n_above_3000/(n_under_5+n_5_to_250+n_250_to_3000+n_above_3000)) |> 
  ungroup() |> 
  mutate(tot_under_5_pc = n_under_5/sum(n_under_5),
         tot_5_to_250_pc = n_5_to_250/sum(n_5_to_250),
         tot_250_to_3000_pc = n_250_to_3000/sum(n_250_to_3000),
         tot_above_3000_pc = n_above_3000/sum(n_above_3000))
 
                             
  
  
