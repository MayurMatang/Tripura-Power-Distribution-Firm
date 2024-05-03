####################  Load libraries ####################
library(tidyverse)
library(here)
library(kableExtra)
library(lubridate)
library(treemapify)
library(ggthemes)
library(wesanderson)
library(scales)
library(ggsankey)
library(viridisLite)
library(RColorBrewer)
library(gt)
library(ggalt)
library(janitor)
library(ggrepel)
library(ggfittext)

#################### Load merged TSECL data & Colors ####################

merged_tsecl <- read_csv(here("data-preped", "merged_tsecl.csv"))
colors <- read_csv(here("OtherFiles", "export.csv"))


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
        size = 24,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,                #center align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 20),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 13,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 20),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 18),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      legend.title = element_text(size = 16),     # Legend
      legend.text = element_text(size = 12, hjust = 0)
      
    )
}

############### Number of complaints across years and months ###############

table <- merged_tsecl |>          # Create a table
  mutate(
    Years = year(ComplaintDate_clean),
    Months = month(ComplaintDate_clean, label = T, abbr = T)
  ) |>
  select(Years, Months) |>
  table() |>
  as.tibble() |>
  pivot_wider(names_from = Months, values_from = n)

table <-
  mutate(table, Total = rowSums(table[2:13]))    # Add row & column totals

colsum <- summarise_all(table[2:14], sum) |>
  mutate(Years = "Total")

colsum <- colsum[, c(14, 1:13)]

table <- rbind(table, colsum)


#Add commas alternative 1
#table$Jan <- prettyNum(table$Jan,big.mark = ',')
#table$Feb <- prettyNum(table$Feb,big.mark = ',')
#table$Mar <- prettyNum(table$Mar,big.mark = ',')
#table$Apr <- prettyNum(table$Apr,big.mark = ',')
#table$May <- prettyNum(table$May,big.mark = ',')
#table$Jun <- prettyNum(table$Jun,big.mark = ',')
#table$Jul <- prettyNum(table$Jul,big.mark = ',')
#table$Aug <- prettyNum(table$Aug,big.mark = ',')
#table$Sep <- prettyNum(table$Sep,big.mark = ',')
#table$Oct <- prettyNum(table$Oct,big.mark = ',')
#table$Nov <- prettyNum(table$Nov,big.mark = ',')
#table$Dec <- prettyNum(table$Dec,big.mark = ',')
#table$Total <- prettyNum(table$Total,big.mark = ',')

#Add Commas alternative 2
# table <- apply(table, 2, function(x) prettyNum(x, big.mark = ","))

# Viz-1: Table of Absolute Complaints Across Years & Months
kable_table <- table |>                        # Customize table
  kbl(caption = "TSECL Complaints Across Years & Months", align = "c")  |>
  column_spec(6, background = ifelse(table[, 6] > 8000 &
                                       table[, 6] < 10000, "#e09500", "white")) |>
  column_spec(7, background = ifelse(table[, 7] > 7000 &
                                       table[, 7] < 12000, "#e09500", "white")) |>
  column_spec(8, background = ifelse(table[, 8] > 7000 &
                                       table[, 8] < 8000, "#e09500", "white")) |>
  column_spec(9, background = ifelse(table[, 9] > 7000 &
                                       table[, 9] < 20000, "#e09500", "white")) |>
  column_spec(10, background = ifelse(table[, 10] > 9000 &
                                        table[, 10] < 20000, "#e09500", "white")) |>
  column_spec(11, background = ifelse(table[, 11] > 6000 &
                                        table[, 11] < 7000, "#e09500", "white")) |>
  kable_classic(full_width = F, html_font = "Georgia") |>
  #add_header_above(c(" " = 1, "Months" = 12, " "=1), bold = TRUE) |>
  kable_styling(bootstrap_options = c("striped")) |>
  row_spec(c(4, 0), background = "white", bold = TRUE) |>
  column_spec(c(1, 14), bold = TRUE)


# Viz-2: Bar Graph of Absolute Complaints Across Years

merged_tsecl |> group_by(year = year(ComplaintDate_clean)) |>
  summarise(n = n()) |>
  ggplot(aes(x = factor(year), y = n)) +
  labs(
    x = "",
    y = "Number of Complaints",
    title = "Total Number of Complaints",
    caption = "Data shown is from Aug 2020 to Oct 2022"
  ) +
  geom_col(fill = "#e09500", width = 0.8) +  theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 7) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 0.5,
    vjust = 2,
    color = "white",
    size = 6
  )

# Viz-3: Line graph of Absolute Complaints Across Years & Months

merged_tsecl |>
  group_by(
    year = year(ComplaintDate_clean),
    month = month(ComplaintDate_clean, label = T, abbr = T)
  ) |>
  summarise(n = n()) |>
  ggplot(aes(x = month, y = n, color = factor(year))) +
  geom_line(linewidth = 1, aes(group = year)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 8) +
  labs(
    title = "Total Complaints across Months",
    x = "",
    y = "Total Number of Complaints",
    color = "Year",
    subtitle = "Complaints are higher between May & Aug"
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1")) +
  theme_tsecl()


#################### Medium of complaints ####################

# Viz-4: Bar Graph of Mediums of Complaints (Absolute)

merged_tsecl |> group_by(ComplaintSource) |>
  summarise(n = n()) |>
  filter(!is.na(ComplaintSource)) |>
  ggplot(aes(x = reorder(ComplaintSource, n), y = n)) +
  labs(
    title = "Mediums of Complaint",
    x = "Medium",
    y = "Total Complaints",
    subtitle = "Majority of the complaints are received by call center"
  ) +
  geom_col() + theme_tsecl() + scale_y_continuous(n.breaks = 9) + coord_flip()

# Viz-5: Bar Graph of Mediums of Complaints (Absolute) facet by year

merged_tsecl |> group_by(year(ComplaintDate_clean), ComplaintSource) |>
  summarise(n = n()) |>
  filter(!is.na(ComplaintSource)) |>   # Remove 2 NAs
  ggplot(aes(x = reorder(ComplaintSource, n), y = n)) +
  labs(
    title = "Mediums of Complaint",
    x = "Medium",
    y = "Total Complaints",
    subtitle = "Majority of the complaints are received by call center"
  ) +
  geom_col() + theme_tsecl()  +
  coord_flip() + facet_wrap( ~ `year(ComplaintDate_clean)`)

# Viz-6: Bar Graph of Mediums of Complaints (Absolute & Percent) facet by year
merged_tsecl |>
  filter(!is.na(ComplaintSource)) |>
  group_by(year = year(ComplaintDate_clean), ComplaintSource) |>   # proportion complaints
  summarise(n = n()) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 2000) |>
  ggplot(aes(x = reorder(ComplaintSource, n), y = n)) +
  labs(
    title = "Mediums of Registering Complaint",
    x = "Medium",
    y = "Total Number of Complaints",
    subtitle = "Majority of the complaints are received by call center",
    caption = "Bijli Mitra Web, WhatsApp, Sampark Portal, Twitter,
       Feedback, SMS and WebChat are not shown as they
       make less than 5% of the complaint."
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() + coord_flip() +
  theme(strip.text.x = element_text(size = 15)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) +
  facet_grid( ~ year)


########################## Complaint Types ##########################


# Viz-7: Bar Graph of Complaint Types (Absolute & Percent)

merged_tsecl |> group_by(ComplaintType) |>  # Complaint Types in absolute
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> arrange(desc(n)) |>
  filter(n > 1000) |>
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col(fill = "#e09500") +
  coord_flip() + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = -1,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  labs(
    title = "Complaint Types",
    caption = "Written Complaint, On Door Connection Request,
       Theft Information, Energy Theft, Harassment by
       DISCOM Official & Transformer Failure are not
       shown as they make less than 1% of the complaint.
       *Percents are rounded to nearest integer.",
    x = "Complaint Type",
    y = "Total Number of Complaints"
  )


# Viz-8: Tree Map of Complaint Types (Percent)

merged_tsecl |> group_by(ComplaintType) |>  # Complaint Types in proportion
  summarise(n = n()) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ggplot(aes(
    fill = ComplaintType,
    area = percent,
    label = paste0(ComplaintType, "\n", percent, "%")
  )) +
  geom_treemap() + geom_treemap_text(colour = "white",
                                     place = "centre",
                                     family = "Georgia") +
  scale_fill_manual(values = sample(colors$`RGB Hex`, size = 15, replace =
                                      F)) +
  labs(
    title = "Proportion of Complaint Types",
    caption = "Written Complaint, On Door Connection Request,
       Theft Information, Energy Theft, Harassment by
       DISCOM Official & Transformer Failure are not
       shown as they make less than 1% of the complaint.
       *Percents are rounded to nearest integer."
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(family = "Georgia"),
    plot.title = element_text(
      family = "Georgia",
      size = 20,
      face = 'bold',
      hjust = 0.5,
      vjust = 2
    )
  )


# Viz-9: Tree Map of Complaint Types (Percent) facet by year

merged_tsecl |>
  group_by(year = year(ComplaintDate_clean), ComplaintType) |>  # Complaint Types in proportion
  summarise(n = n()) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |> #arrange(desc(n))
  ggplot(aes(
    fill = ComplaintType,
    area = percent,
    label = paste0(ComplaintType, "\n", percent, "%")
  )) +
  geom_treemap() + geom_treemap_text(colour = "white",
                                     place = "centre",
                                     family = "Georgia") +
  scale_fill_manual(values = sample(colors$`RGB Hex`, size = 15, replace =
                                      F)) +
  labs(
    title = "Proportion of Complaint Types",
    caption = "Written Complaint, On Door Connection Request,
       Theft Information, Energy Theft, Harassment by
       DISCOM Official & Transformer Failure are not
       shown as they make less than 1% of the complaint.
       *Percents are rounded to nearest integer."
  ) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    text = element_text(family = "Georgia"),
    strip.text.x = element_text(size = 18),
    plot.title = element_text(
      family = "Georgia",
      size = 20,
      face = 'bold',
      hjust = 0.5,
      vjust = 2
    )
  ) +
  facet_grid( ~ year)


########################## Subcomplaint Types ##########################

# Viz-10: Bar Graph of Subcomplaint Types under 'OTHER COMPLAINT' (Absolute)

merged_tsecl |> filter(ComplaintType == "OTHER COMPLAINT") |>
  mutate(SubComplaintType = replace(SubComplaintType, is.na(SubComplaintType), "Missing Value")) |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col() + coord_flip() +
  theme_tsecl() +
  labs(x = "Subcomplaints", y = "Number of Complaints",
       title = "Total Complaints under 'Other Complaint'")


# Viz-10: Bar Graph of Subcomplaint Types under 'OTHER COMPLAINT' (Absolute & %)

merged_tsecl |> filter(ComplaintType == "OTHER COMPLAINT") |>
  mutate(SubComplaintType = replace(SubComplaintType, is.na(SubComplaintType), "Missing Value")) |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 1000) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) +
  geom_col(fill = c("darkgrey", "#e09500"), width = 0.7) +
  coord_flip() + theme_tsecl() +
  labs(
    title = "Subcomplaint Types under 'Other Complaint'",
    y = "Total Number of Complaints",
    x = "Subcomplaint Type",
    caption = "HT Complaint, Planned Shut Down by ESD,
       Written Complaint, New Connection, Reconnection,
       Disconnection & Delay in Connection are not
       shown as they make less than 5% of complaints."
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.5,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.8,
    color = "#7c1419",
    size = 6
  )


# Viz-11: Bar Graph of Subcomplaint Types under 'No Current Complaint' (Absolute & %)

merged_tsecl |>
  filter(ComplaintType == "NO CURRENT COMPLAINT") |>
  mutate(SubComplaintType = replace(SubComplaintType, is.na(SubComplaintType), "Missing Value")) |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 900) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col(fill =
                                                                    c("darkgrey", "#e09500"), width = 0.7) +
  coord_flip() + theme_tsecl() +
  labs(
    title = "Subcomplaint Types under 'No Current Complaint'",
    y = "Total Number of Complaints",
    x = "Subcomplaint Type",
    caption = "Other Technical, Broken/Damanged Wire, Loose Wire,
       Voltage Fluctuation, Broken, Bent/Broken Pole,
       Leakage of Current in Pole, Tored, Single Phase,
       Hanged & Agriculture are not shown as they make
       less than 1% of complaints."
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.5,
    color = "white",
    size = 4.5
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.8,
    color = "#7c1419",
    size = 4.5
  )


# Viz-12: Bar Graph of Subcomplaint Types under 'All Area No Power' (Absolute & %)

merged_tsecl |>
  filter(ComplaintType == "ALL AREA NO POWER") |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = NULL, trim = FALSE)) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col(width =
                                                                    0.7, fill = "#e09500") +
  coord_flip() + theme_tsecl() +
  labs(title = "Subcomplaint Types Under 'All Area No Power'", x = "Subcomplaint Type",
       y = "Total Number of Complaints") +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.5,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.8,
    color = "#7c1419",
    size = 6
  )

# Viz-13: Bar Graph of Subcomplaint Types under 'Service Line Related' (Absolute & %)

merged_tsecl |>
  filter(ComplaintType == "SERVICE LINE RELATED") |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col(width =
                                                                    0.7, fill = "#e09500") +
  coord_flip() + theme_tsecl() +
  labs(
    title = "Subcomplaint Types Under 'Service Line Related'",
    y = "Total Number of Complaints",
    x = "Subcomplaint Type",
    caption = ""
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.5,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.8,
    color = "#7c1419",
    size = 6
  )


# Viz-14: Bar Graph of Subcomplaint Types under 'Other Technical' (Absolute & %)

merged_tsecl |>
  filter(ComplaintType == "OTHER TECHNICAL") |>
  group_by(SubComplaintType) |>
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  ggplot(aes(x = reorder(SubComplaintType, n), y = n)) + geom_col(width =
                                                                    0.7, fill = "#e09500") +
  coord_flip() + theme_tsecl() +
  labs(
    title = "Subcomplaint Types Under 'Other Technical'",
    y = "Total Number of Complaints",
    x = "Subcomplaint Type",
    caption = ""
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.5,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.8,
    color = "#7c1419",
    size = 6
  )


############### Complaints under Circles Divisions & Subdivision ###############

# Create a vector of color for Circles
circle_color <-
  c(
    "#c14c54",
    "#04aa6d",
    "#74aadb",
    "#335964",
    "#e1af00",
    "#f18b00",
    "#8c8466",
    "#898999",
    "#181f86"
  )

# Viz-15: Bar Graph of Complaints Across Circles (Absolute)

merged_tsecl |> group_by(Circle) |>   # Circle complaints in absolute
  filter(!is.na(Circle)) |>
  summarise(n = n()) |> arrange(n) |>
  ggplot(aes(x = reorder(Circle, n), y = n)) + geom_col() + coord_flip() + theme_tsecl()

# Viz-16: Bar Graph of Complaints Across Top 3 Circles (Absolute & %)

merged_tsecl |> group_by(Circle) |>
  filter(!is.na(Circle)) |> # Remove 5 NAs
  summarise(n = n()) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  arrange(desc(n)) |>
  filter(n > 9000) |>
  ggplot(aes(x = reorder(Circle, n), y = n, fill=Circle)) + geom_col(width = 0.7) +
  scale_fill_manual(values=circle_color) +
  coord_flip() + theme_tsecl() +
  theme(legend.position = "none") +
  labs(
    title = "Complaints Across Circles",
    x = "Circle",
    y = "Total Number of Complaints",
    subtitle = "2 out of 9 circles received 97% of total complaints",
    caption = 
      "0.9% (48) of the complaints under Durgachowmohani, 
    Kamalpur Division, Dhalai Circle have been misclassified. 
    They are counted under Durgachowmohani_R, 
    Agartala 1 Division, Circle 1 (W. Tripura)."
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "#7c1419",
    size = 6
  )


# Viz-17: Bar Graph of Complaints Across Top 3 Circles (Absolute & %) facet by year

merged_tsecl |>
  group_by(year = year(ComplaintDate_clean), Circle) |>
  filter(!is.na(Division)) |> # Remove 5 NAs
  summarise(n = n()) |> #arrange(desc(n)) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 1000) |> arrange(year) |> print(n = Inf) |>
  ggplot(aes(
    x = reorder(Circle, n),
    y = n,
    fill = Circle
  )) + geom_col() +
  coord_flip() + theme_tsecl() + scale_fill_manual(values = circle_color) +
  theme(strip.text.x = element_text(size = 15)) +
  labs(
    title = "Complaints Across Top Circles",
    x = "Circle",
    y = "Total Number of Complaints",
    subtitle = ""
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 3) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.3,
    color = "white",
    size = 3.5
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 0.6,
    vjust = -1.2,
    color = "black",
    size = 3.5
  ) +
  facet_grid( ~ year)


# Viz-18: Bar Graph of Complaints Across Divisions (Absolute)

merged_tsecl |> group_by(Division) |>
  filter(!is.na(Division)) |>
  summarise(n = n()) |> arrange(n) |>
  ggplot(aes(x = reorder(Division, n), y = n)) + geom_col() + coord_flip() + theme_tsecl()


# Viz-19: Bar Graph of Complaints Across Top 5 Divisions (Absolute & %)

merged_tsecl |> group_by(Division, Circle) |>
  summarise(n = n()) |> arrange(desc(n)) |>
  ungroup() |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE))  |>
  filter(n > 8000) |>
  group_by(Circle) |>
  ggplot(aes(
    x = reorder(Division, n),
    y = n,
    fill = Circle
  )) +
  geom_col() +
  coord_flip() + theme_tsecl() + scale_fill_manual(values = circle_color) +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "white",
    size = 6
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "white",
    size = 6
  ) +
  labs(
    title = "Complaints Across Divisions",
    x = "Division",
    y = "Total Number of Complaints",
    subtitle = "4 out of 23 divisions received 97% of total complaints",
    caption = "0.9% (48) of the complaints under Durgachowmohani, 
    Kamalpur Division, Dhalai Circle have been misclassified. 
    They are counted under Durgachowmohani_R, 
    Agartala 1 Division, Circle 1 (W. Tripura)."
  )


# Viz-20: Bar Graph of Complaints Across Bottom 18 Divisions (Absolute & %)

merged_tsecl |> group_by(Division, Circle) |>
  filter(!is.na(Division)) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = percent(n / sum(n), accuracy = 0.01, trim = FALSE))  |>
  filter(n <= 1000) |>
  group_by(Circle) |>
  ggplot(aes(x = reorder(Division,-n), y = n)) + geom_col(fill = "#e09500") +
  coord_flip() + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = circle_color) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 0.8,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.6,
    vjust = 0.8,
    color = "#7c1419",
    size = 4.5
  ) +
  labs(
    title = "Complaints Across Divisions",
    x = "Division",
    y = "Total Number of Complaints",
    subtitle = "18 out of 23 divisions received just 3.5% of total complaints"
  )


# Viz-21: Bar Graph of Complaints Across Bottom 18 Divisions (Absolute & %)

merged_tsecl |> group_by(Division, Circle) |>
  filter(!is.na(Division)) |>         # Remove 5 NAs
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = percent(n / sum(n), accuracy = 0.01, trim = FALSE))  |>
  filter(n <= 1000) |>
  group_by(Circle) |>
  ggplot(aes(
    x = reorder(Division,-n),
    y = n,
    fill = Circle
  )) + geom_col() + coord_flip() + theme_tsecl() +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_fill_manual(values = circle_color) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 0.8,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.5,
    vjust = 0.8,
    color = "#7c1419",
    size = 4.5
  ) +
  labs(
    title = "Complaints Across Divisions",
    x = "Division",
    y = "Total Number of Complaints",
    subtitle = "19 out of 23 divisions received just 3.6% of total complaints",
    caption = 
      "0.9% (48) of the complaints under Durgachowmohani, 
    Kamalpur Division, Dhalai Circle have been misclassified. 
    They are counted under Durgachowmohani_R, 
    Agartala 1 Division, Circle 1 (W. Tripura)."
  )


# Viz-22: Bar Graph of Complaints Across Top 3 Divisions (Absolute & %) facet by year

merged_tsecl |>
  group_by(Division, year = year(ComplaintDate_clean), Circle) |>
  filter(!is.na(Division)) |> # Remove 5 NAs
  summarise(n = n()) |> #arrange(desc(n)) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 500) |> arrange(year) |> print(n = Inf) |>
  ggplot(aes(
    x = reorder(Division, n),
    y = n,
    fill = Circle
  )) + geom_col() +
  coord_flip() + theme_tsecl() + scale_fill_manual(values = circle_color) +
  theme(strip.text.x = element_text(size = 15)) +
  labs(
    title = "Complaints Across Top Divisions",
    x = "Division",
    y = "Total Number of Complaints",
    subtitle = ""
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 3) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.3,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1.2,
    color = "black",
    size = 4
  ) +
  facet_grid( ~ year)


# Viz-23: Bar Graph of Complaints Across Subdivisions > 1000 Complaints (Absolute)

merged_tsecl |> group_by(SubdivisionName_clean) |>
  summarise(n = n()) |> arrange(desc(n)) |>
  filter(n > 1000) |>
  ggplot(aes(x = reorder(SubdivisionName_clean, n), y = n)) + geom_col() + coord_flip() + theme_tsecl()


# Viz-24: Bar Graph of Complaints Across Subdivisions < 1000 Complaints (Absolute)

merged_tsecl |> group_by(SubdivisionName_clean) |>
  filter(!is.na(SubdivisionName_clean)) |>
  summarise(n = n()) |> arrange(n) |>
  filter(n <= 1000) |>
  ggplot(aes(x = reorder(SubdivisionName_clean,-n), y = n)) + geom_col() + coord_flip() + theme_tsecl()


# Viz-25: Bar Graph of Complaints of Top 10  Subdivisions (Absolute & %)

merged_tsecl |> group_by(SubdivisionName_clean, Circle) |>
  filter(!is.na(SubdivisionName_clean)) |> # Remove 5 NAs
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  filter(n > 9000) |>
  group_by(Circle) |>
  ggplot(aes(
    x = reorder(SubdivisionName_clean, n),
    y = n,
    fill = Circle
  )) + geom_col() +
  coord_flip() + theme_tsecl() +
  scale_fill_manual(values = circle_color) +
  labs(
    title = "Complaints Across Subdivisions",
    x = "Subdivision",
    y = "Total Number of Complaints",
    subtitle = "10 out of 78 subdivisions received 81% of total complaints",
    caption = 
      "0.9% (48) of the complaints under Durgachowmohani, 
    Kamalpur Division, Dhalai Circle have been misclassified. 
    They are counted under Durgachowmohani_R, 
    Agartala 1 Division, Circle 1 (W. Tripura)."
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 5) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1.2,
    color = "white",
    size = 5
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -0.7,
    color = "white",
    size = 5
  )


# Viz-26: Bar Graph of Complaints of Top  Subdivisions (Absolute & %) facet by Year

merged_tsecl |>
  group_by(SubdivisionName_clean, year = year(ComplaintDate_clean), Circle) |>
  filter(!is.na(SubdivisionName_clean)) |> # Remove 5 NAs
  summarise(n = n()) |> #arrange(desc(n)) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = percent(n / sum(n), accuracy = 0.1, trim = FALSE)) |>
  filter(n > 1000) |> arrange(year) |> print(n = Inf) |>
  ggplot(aes(
    x = reorder(SubdivisionName_clean, n),
    y = n,
    fill = Circle
  )) + geom_col() +
  coord_flip() + theme_tsecl() + scale_fill_manual(values = circle_color) +
  theme(strip.text.x = element_text(size = 15)) +
  labs(
    title = "Complaints Across Top Subdivisions",
    x = "Subdivision",
    y = "Total Number of Complaints",
    subtitle = ""
  ) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 1,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.1,
    vjust = 1,
    color = "#7c1419",
    size = 3.5
  ) +
  facet_grid( ~ year)


########################## Complaint Types Across Locations ##########################

# Viz-27: Bar Graph of Complaints Types (Absolute) facet by Circle

merged_tsecl |> group_by(Circle, ComplaintType) |>
  filter(!is.na(Circle)) |>
  summarise(n = n()) |> # arrange(n) |> print(n=Inf) |>
  ungroup() |>
  group_by(Circle) |>
  mutate(percent = percent(n / sum(n), accuracy = 0.01, trim = FALSE))  |>
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col(fill = "#e09500") +
  coord_flip() + theme_tsecl() + facet_wrap( ~ Circle) +
  theme(strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  labs(title = "Complaint Types Across Circles", x = "Complaint Type", y =
         "Total Number of Complaints") +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) #+
#geom_text(aes(label=percent), hjust = 1.1, vjust = -1, color = "white", size=4) +
#geom_text(aes(label=scales::comma(n)), hjust = 1.1, vjust = 2, color = "#7c1419", size=4)


# Viz-28: Bar Graph of Complaints Types (Absolute) facet by Division

merged_tsecl |> group_by(Division, ComplaintType) |>
  filter(!is.na(Division),!is.na(ComplaintType)) |>
  summarise(n = n()) |> arrange(n) |> print(n = Inf) |>
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col(fill = "#e09500") + coord_flip() + theme_tsecl() +
  facet_wrap( ~ Division) + theme(strip.text.x = element_text(size = 0.1),
                                  axis.text = element_text(size = 12)) +
  labs(title = "Complaint Types Across Divisions", x = "Complaint Type", y =
         "Total Number of Complaints")

# Viz-29: Bar Graph of Complaints Types (Absolute) facet by Subdivision

merged_tsecl |> group_by(SubdivisionName_clean, ComplaintType) |>
  filter(!is.na(SubdivisionName_clean),!is.na(ComplaintType)) |>
  summarise(n = n()) |> arrange(n) |> print(n = Inf) |>
  ggplot(aes(x = reorder(ComplaintType, n), y = n)) + geom_col() + coord_flip() + theme_tsecl() +
  facet_wrap( ~ SubdivisionName_clean)


######################## Complaints w.r.t time ########################

# Viz-30: Line Graph of Complaints (Absolute) Across Time (hours)

merged_tsecl |> group_by(hour(ComplaintDate_clean)) |>    # Time of complaints filed
  summarise(n = n()) |>
  ggplot(aes(x = `hour(ComplaintDate_clean)`, y = n)) +
  geom_line(size = 1, color = "#e09500") + theme_tsecl() +
  labs(
    title = "Time of Complaints",
    x = "Time (hour)",
    y = "Total Number of Complaints",
    subtitle = "Majority complaints are received between 8am to 6pm"
  ) +
  theme_tsecl() + scale_x_continuous(n.breaks = 14) +
  scale_y_continuous(labels = comma_format(), n.breaks = 6)


# Viz-31: Bar Graph of Total Days Taken to Resolve a Complaint Type

merged_tsecl |> filter(!is.na(ComplaintCloseDate_clean)) |>
  mutate(Timediff = ComplaintCloseDate_clean - ComplaintDate_clean) |>
  group_by(ComplaintType) |>
  summarise(ResDays = sum(Timediff) / 86400) |>
  ggplot(aes(x = reorder(ComplaintType, ResDays), y = ResDays)) +  geom_col() +
  coord_flip()
  
  # Viz-32: Bar Graph of Median Hours Taken to Resolve a Complaint Type
  
  merged_tsecl |> filter(!is.na(ComplaintCloseDate_clean)) |>
  mutate(Timediff = ComplaintCloseDate_clean - ComplaintDate_clean) |>
  group_by(ComplaintType) |>
  summarise(AvgResTime = median(Timediff) / 3600) |>
  filter(!is.na(ComplaintType)) |>
  ggplot(aes(x = reorder(ComplaintType, AvgResTime), y = AvgResTime)) +  
  geom_col(fill = "#e09500") +
  coord_flip() +
  labs(title = "Time to Resolve Complaint Types", x = "Complaint Type", y =
         "Median Number of Hours") +
  theme_tsecl() +
  geom_text(
    aes(label = round(AvgResTime, 2)),
    hjust = 1.2,
    vjust = 0.7,
    color = "white",
    size = 6
  )


# Viz-33: Bar Graph of Median Hours Taken to Resolve a Complaint Type (Index)

merged_tsecl |>
  filter(!is.na(ComplaintCloseDate_clean)) |>
  mutate(Timediff = ComplaintCloseDate_clean - ComplaintDate_clean) |>
  group_by(ComplaintType) |>
  summarise(n = n(), AvgResTime = median(Timediff)) |>
  ungroup() |>
  group_by(ComplaintType) |>
  mutate(pct = n / sum(n), Index = AvgResTime / pct)
ggplot(aes(x = ComplaintType, y = AvgResTime)) +
  geom_col(aes(fill = Index)) +
  coord_flip() +
  labs(title = "Median Time / % of Complaint Type", x = "Complaint Type") +
  theme_tsecl()


# Viz-34: Bar Graph of Total Days Taken to Resolve a Subcomplaint Type

merged_tsecl |> filter(!is.na(ComplaintCloseDate_clean)) |>
  mutate(Timediff = ComplaintCloseDate_clean - ComplaintDate_clean) |>
  group_by(SubComplaintType) |>
  summarise(ResDays = sum(Timediff) / 86400) |>
  filter(!is.na(SubComplaintType)) |>
  ggplot(aes(x = reorder(SubComplaintType, ResDays), y = ResDays)) +  geom_col() +
  coord_flip() + theme_tsecl()


# Viz-32: Bar Graph of Median Hours Taken to Resolve a Subcomplaint Type

merged_tsecl |> filter(!is.na(ComplaintCloseDate_clean)) |>
  mutate(Timediff = ComplaintCloseDate_clean - ComplaintDate_clean) |>
  group_by(SubComplaintType) |>
  summarise(AvgResTime = median(Timediff) / 3600) |>
  filter(!is.na(SubComplaintType)) |>
  filter(AvgResTime > 0) |>
  ggplot(aes(x = reorder(SubComplaintType, AvgResTime), y = AvgResTime)) +  geom_col(fill =
                                                                                       "#e09500") +
  coord_flip() + labs(
    y = "Median Number of Hours",
    x = "Subcomplaint Type",
    title = "Time to Resolve Subcomplaint Types",
    caption = "Subcomplaint Type Disconnection is not
  shown as it gives negative median hours."
  ) +
  theme_tsecl() + theme(axis.text = element_text(size = 10)) +
  geom_text(
    aes(label = round(AvgResTime, 2)),
    hjust = 1.2,
    vjust = 0.5,
    color = "white",
    size = 4.5
  )


######################## Harassment by officials ########################

# Total complaints across 3 years is 15 
merged_tsecl |> filter(ComplaintType == "HARASSMENT BY DISCOM OFFICIAL") |> 
  group_by(Year = year(ComplaintDate_clean), Month = month(ComplaintDate_clean)) |> 
  count() |> 
  arrange(desc(n))

# Location
merged_tsecl |> filter(ComplaintType == "HARASSMENT BY DISCOM OFFICIAL") |> 
  group_by(Circle, Division, SubdivisionName_clean) |> 
  count()|> 
  arrange(desc(n))

# Source
merged_tsecl |> filter(ComplaintType == "HARASSMENT BY DISCOM OFFICIAL") |> 
  group_by(ComplaintSource) |> 
  count()|> 
  arrange(desc(n))

# Outage type, rectification and cause
merged_tsecl |> filter(ComplaintType == "HARASSMENT BY DISCOM OFFICIAL") |> 
  group_by(OutageType, Rectification,	Cause) |> 
  count() |> 
  arrange(desc(n))


########################## Outage Type ##########################

# Viz 1: Outage Type (absolute & proportion)
merged_tsecl |> 
  filter(!is.na(OutageType)) |> 
  group_by(OutageType) |> 
  summarise(n = n()) |> 
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(x = reorder(OutageType, n), y = n)) +
  labs(
    title = "Complaints Across Outage Types",
    x = "",
    y = "Total Number of Complaints",
    subtitle = "",
    caption = ""
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() + coord_flip() +
  theme(strip.text.x = element_text(size = 15)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_x_discrete(labels = wrap_format(15)) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 

# Viz 2: Outage Type (absolute & proportion) facet by year
merged_tsecl |>
  filter(!is.na(OutageType)) |>
  group_by(year = year(ComplaintDate_clean), OutageType) |>   # proportion complaints
  summarise(n = n()) |>
  ungroup() |>
  group_by(year) |>
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |>
  ggplot(aes(x = reorder(OutageType, n), y = n)) +
  labs(
    title = "Complaints Across Outage Types",
    x = "Outage Type",
    y = "Total Number of Complaints",
    subtitle = "",
    caption = ""
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() + coord_flip() +
  theme(strip.text.x = element_text(size = 15)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 3) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "#7c1419",
    size = 4,
  ) +
  facet_grid( ~ year)

# Viz 3: Outage Type (top 3) across months
merged_tsecl |>
  group_by(
    OutageType,
    month = month(ComplaintDate_clean, label = T, abbr = T)
  ) |>
  summarise(n = n()) |> 
  filter(OutageType %in% c("FUSE RELATED", "SERVICE LINE","CABLE")) |> 
  ggplot(aes(x = month, y = n, color = factor(OutageType))) +
  geom_line(linewidth = 1, aes(group = OutageType)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 8) +
  labs(
    title = "Top 3 Outage Type",
    x = "",
    y = "Total Number of Complaints",
    color = "Outage Type",
    subtitle = ""
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1")) +
  theme_tsecl()


########################## Cause ##########################

# Viz 1: Cause (absolute & proportion)
merged_tsecl |> 
  filter(!is.na(Cause)) |> 
  mutate(Cause = fct_lump_n(Cause, n=9, other_level = "OTHERS*")) |> 
  group_by(Cause) |> 
  summarise(n = n()) |> 
  mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> 
  #select(Cause, n, percent)
  ggplot(aes(x = fct_reorder(Cause,n), y = n)) +
  labs(
    title = "Complaints Across Cause Types",
    x = "",
    y = "Total Number of Complaints",
    subtitle = "",
    caption = ""
  ) +
  geom_col(fill = "#e09500") + theme_tsecl() + coord_flip() +
  theme(strip.text.x = element_text(size = 15)) +
  scale_y_continuous(labels = comma_format(), n.breaks = 4) +
  scale_x_discrete(labels = wrap_format(17)) +
  geom_text(
    aes(label = percent),
    hjust = 1.1,
    vjust = 2,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = 1.1,
    vjust = -1,
    color = "#7c1419",
    size = 4
  ) 


########################## Rectification ##########################
# Viz 1: Rectification (absolute & proportion)
merged_tsecl |> 
    filter(!is.na(Rectification)) |> 
    mutate(Rectification = fct_lump_n(Rectification, n=7, other_level = "OTHERS*")) |>
    group_by(Rectification) |> 
    summarise(n = n()) |>
    mutate(percent = percent(n / sum(n), accuracy = 1, trim = FALSE)) |> 
    ggplot(aes(x = reorder(Rectification, n), y = n)) +
    labs(
      title = "Complaints Across Rectification Types",
      x = "",
      y = "Total Number of Complaints",
      subtitle = "",
      caption = ""
    ) +
    geom_col(fill = "#e09500") + theme_tsecl() + coord_flip() +
    theme(strip.text.x = element_text(size = 15)) +
    scale_y_continuous(labels = comma_format(), n.breaks = 4) +
    scale_x_discrete(labels = wrap_format(25)) +
    geom_text(
      aes(label = percent),
      hjust = 1.1,
      vjust = 2,
      color = "#7c1419",
      size = 4
    ) +
    geom_text(
      aes(label = scales::comma(n)),
      hjust = 1.1,
      vjust = -1,
      color = "#7c1419",
      size = 4
    ) 

  
############# Sankey: Cause,Outage Type, Rectification ##################
  
df_sankey <- merged_tsecl |> 
    filter(!is.na(OutageType)) |> 
    select(Cause, OutageType,Rectification)  
    #group_by(OutageType, Cause, Rectification) |> 
    #count(sort=T) |>  view()
  
df_sankey <- df_sankey |> 
  make_long(Cause, OutageType,Rectification)
  

p1 <- ggplot(df_sankey, aes(x = x,
                            next_x = next_x,
                            node = node,
                            next_node = next_node,
                            fill=factor(node),
                            label = node)) +
  geom_sankey(flow.alpha=0.5, 
              node.color="white",
              show.legend = F) +
  geom_sankey_label(size=2, 
                    color="black", 
                    fill="white", 
                    hjust = "outward",
                    label.size = 0.05
                    ) +
  theme_tsecl() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_blank()) + 
  labs(title = "", subtitle="", x = "", y="") +
  scale_fill_viridis_d(option = "C")
p1


#################### Rectification and Resolution Time ##################

# Viz 1: Under 5 Res time boxplots for top 5 Rectification
merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
ggplot(aes(x = fct_reorder(Rectification, -res_time_hrs), y = res_time_hrs)) +
  geom_boxplot(fill = c("#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")) +
  scale_x_discrete(labels = wrap_format(25)) +
  labs(x = "Rectification Types", y = "Resolution Time (hrs)") +
  theme_tsecl() +
    theme(axis.text.x = element_text(size=12)) 

# Viz 2: Under 5 Res time boxplots for top 5 Rectification facet by year
merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = fct_reorder(Rectification, -res_time_hrs), y = res_time_hrs)) +
  geom_boxplot(fill = c("#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")) +
  scale_x_discrete(labels = wrap_format(18)) +
  labs(x = "Rectification Types", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(axis.text.x = element_text(size=12), strip.text = element_text(size = 14)) +
  coord_flip() +
  facet_grid(~year)

# Viz 3: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot() +
  labs(caption = "Data includes all Rectification Types", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 
  #scale_fill_brewer(palette = "Set3")
  #scale_fill_viridis(discrete = TRUE)
  #scale_fill_viridis_d(option = "plasma") +


# Viz 3.1: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  filter(Rectification == "R,Y OR B FUSE REPLACE") |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot(fill = "#F98400") +
  labs(caption = "Data includes only 'R,Y OR B FUSE REPLACE' Rectification Type", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 
  #scale_fill_brewer(palette = "Set3")

# Viz 3.2: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  filter(Rectification == "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT") |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot(fill = "#ECCBAE") +
  labs(caption = "Data includes only 'IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT' Rectification Type", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 
  #scale_fill_brewer(palette = "Set3")

# Viz 3.3: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  filter(Rectification == "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED") |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot(fill = "#00A08A") +
  labs(caption = "Data includes only 'NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED' Rectification Type", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 

# Viz 3.4: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  filter(Rectification == "PATROLLING OF FEEDER TO RESOLVE THE FAULT") |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot(fill = "#5BBCD6") +
  labs(caption = "Data includes only 'PATROLLING OF FEEDER TO RESOLVE THE FAULT' Rectification Type", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 

# Viz 3.5: Under 5 Res time boxplots for top 5 Rectification across months
merged_tsecl |> 
  filter(Rectification == "IN CASE OF CARBON RECONNECT THE CABLE") |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean, label = T, abbr = T),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  ggplot(aes(x = month, y = res_time_hrs, group = month)) +
  geom_boxplot(fill = "#F2AD00") +
  labs(caption = "Data includes only 'IN CASE OF CARBON RECONNECT THE CABLE' Rectification Type", 
       x = "", y = "Resolution Time (hrs)") +
  theme_tsecl() +
  theme(legend.position = "none") 

# Viz 4: Density Plot
merged_tsecl |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0) |> #view()
  mutate(ln_res_time = log(res_time_hrs)) |> 
ggplot(aes(x = ln_res_time)) +
  geom_density(fill = "#e09500", color = "#e09500", alpha = 0.6) +
  geom_vline(xintercept = log(1.5), color = "red", linetype=2) +
  #scale_x_continuous(breaks = c(0, 50, 100, 150)) +
  annotate("text", x=0.27, y=0.27, label="Median = 1.5 hours", 
          angle=90, size=4.5, color="grey37") +
  geom_vline(xintercept = log(5), color = "red", linetype=2) +
  annotate("text", x=1.5, y=0.27, label="Top 25 Percentile", 
           angle=90, size=4.5, color="grey37") +
  geom_vline(xintercept = log(350), color = "red", linetype=2) +
  geom_vline(xintercept = log(3000), color = "red", linetype=2) +
  labs(x = "Resolution Time in hours, natural log (base e)", y = "Density") +
  theme_tsecl() +
  theme(legend.position = "none", plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))


# Viz 5.1: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions
merged_tsecl |> 
  filter(Rectification == "R,Y OR B FUSE REPLACE") |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  ggplot(aes(x = fct_reorder(SubdivisionName_clean, -res_time_hrs), 
             y = res_time_hrs)) +
  geom_boxplot() +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "Top 10 Subdivisions", y = "Resolution Time (hrs)", 
       caption = "Data includes only 'R,Y OR B FUSE REPLACE' Rectification Type")

# Viz 5.2: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions
merged_tsecl |> 
  filter(Rectification == "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT") |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  ggplot(aes(x = fct_reorder(SubdivisionName_clean, -res_time_hrs), 
             y = res_time_hrs)) +
  geom_boxplot() +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "Top 10 Subdivisions", y = "Resolution Time (hrs)", 
       caption = "Data includes only 'IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT' Rectification Type")

# Viz 5.3: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions
merged_tsecl |> 
  filter(Rectification == "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED") |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  ggplot(aes(x = fct_reorder(SubdivisionName_clean, -res_time_hrs), 
             y = res_time_hrs)) +
  geom_boxplot() +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "Top 10 Subdivisions", y = "Resolution Time (hrs)", 
       caption = "Data includes only 'NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED' Rectification Type")


# Viz 5.4: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions
merged_tsecl |> 
  filter(Rectification == "PATROLLING OF FEEDER TO RESOLVE THE FAULT") |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  ggplot(aes(x = fct_reorder(SubdivisionName_clean, -res_time_hrs), 
             y = res_time_hrs)) +
  geom_boxplot() +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "Top 10 Subdivisions", y = "Resolution Time (hrs)", 
       caption = "Data includes only 'PATROLLING OF FEEDER TO RESOLVE THE FAULT' Rectification Type")


# Viz 5.5: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions
merged_tsecl |> 
  filter(Rectification == "IN CASE OF CARBON RECONNECT THE CABLE") |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  ggplot(aes(x = fct_reorder(SubdivisionName_clean, -res_time_hrs), 
             y = res_time_hrs)) +
  geom_boxplot() +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "Top 10 Subdivisions", y = "Resolution Time (hrs)", 
       caption = "Data includes only 'IN CASE OF CARBON RECONNECT THE CABLE' Rectification Type")


# Viz 5: Under 5 Res time boxplots for top 5 Rectification across top 10 subdivisions

boxplot_sd <- merged_tsecl |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |>
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         res_time_under5 = res_time_hrs < 5,
         month = month(ComplaintDate_clean),
         year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0 & res_time_hrs < 5) |> 
  filter(SubdivisionName_clean %in% c("DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR",
                                      "AMTALI", "BANAMALIPUR 1", "GB", "IGM",
                                      "DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN"))

boxplot_sd$SubdivisionName_clean[boxplot_sd$SubdivisionName_clean == 
                                   "DURGACHOWMOHANI"] <- "DURGAC'MOHANI"


ggplot(boxplot_sd, aes(x = fct_reorder(Rectification, -res_time_hrs), y = res_time_hrs)) +
  geom_boxplot(fill = c("#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6",
                        "#ECCBAE", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")) +
  scale_x_discrete(labels = wrap_format(18)) +
  labs(x = "Rectification Types", y = "Resolution Time (hrs)") +
  coord_flip() +
  theme_tsecl() +
  theme(axis.text.x = element_text(size=12)) +
  facet_grid(col = vars(SubdivisionName_clean), 
             labeller = labeller(SubdivisionName_clean = label_wrap_gen(15)))


# Viz 6: Volume of complaints across months for res time <5, 5-250, >3000
merged_tsecl |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         month = month(ComplaintDate_clean, label = T, abbr = T)) |> 
  filter(res_time_hrs >= 0) |> 
  mutate(res_time_cat = 
           if_else(res_time_hrs < 5, "< 5 hrs",
                   if_else(res_time_hrs >= 5 & res_time_hrs <= 350, "5-350 hrs",
                           if_else(res_time_hrs > 3000, "> 3000 hrs", "none")))) |>
  group_by(month, res_time_cat) |> 
  summarise(N = n()) |>
  ggplot(aes(x = month, y = N, group = res_time_cat, color = res_time_cat)) +
  geom_line(linewidth = 1) +
  theme_tsecl() +
  #scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "bottom") +
  #theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "Number of Complaints", color = "",
       caption = "October has 21 complaints with resolution time > 3000 hours.") +
  scale_color_manual(values = wes_palette("FantasticFox1"))
  

# Viz 7: Volume of complaints across subdivisions for res time <5, 5-350, >3000
table_gt <- merged_tsecl |> 
  mutate(res_time = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time >= 0) |> 
  mutate(res_time_cat = 
           if_else(res_time < 5, "under_5",
                   if_else(res_time >= 5 & res_time <= 350, "5_to_350",
                           if_else(res_time > 350 & res_time <= 3000, "351_to_3000",
                                   if_else(res_time > 3000, "above_3000", "none"))))) |> 
  group_by(SubdivisionName_clean) |> 
  mutate(n_under_5 = sum(res_time_cat == "under_5"),
         n_5_to_350 = sum(res_time_cat == "5_to_350"),                
         n_351_to_3000 = sum(res_time_cat == "351_to_3000"),
         n_above_3000 = sum(res_time_cat == "above_3000")) |> 
  select(SubdivisionName_clean, n_under_5, n_5_to_350, n_351_to_3000, n_above_3000) |> 
  unique() |> 
  group_by(SubdivisionName_clean) |> 
  mutate(sd_under_5_pc = n_under_5/(n_under_5+n_5_to_350+n_351_to_3000+n_above_3000),
         sd_5_to_350_pc = n_5_to_350/(n_under_5+n_5_to_350+n_351_to_3000+n_above_3000),
         sd_351_to_3000_pc = n_351_to_3000/(n_under_5+n_5_to_350+n_351_to_3000+n_above_3000),
         sd_above_3000_pc = n_above_3000/(n_under_5+n_5_to_350+n_351_to_3000+n_above_3000)) |> 
  filter(!is.na(SubdivisionName_clean)) |> 
  mutate(
    sd_under_5_pc = round(sd_under_5_pc, 2), 
    sd_5_to_350_pc = round(sd_5_to_350_pc, 2), 
    sd_351_to_3000_pc = round(sd_351_to_3000_pc, 2), 
    sd_above_3000_pc = round(sd_above_3000_pc, 2)
  ) |> 
  as.tibble() |> 
  gt()

table_gt |> 
  tab_spanner(
    label = "No. of Complaints",
    columns = c(n_under_5, n_5_to_350, n_351_to_3000, n_above_3000)) |> 
  tab_spanner(
    label = "% of Complaints within the Subdivision",
    columns = c(sd_under_5_pc, sd_5_to_350_pc, sd_351_to_3000_pc, sd_above_3000_pc)) |> 
  gtsave("test_plt.html")


# Viz 8: Dumbbell chart
dumbell <- merged_tsecl |> 
  mutate(res_time = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |>
  filter(res_time >= 0) |> 
  mutate(res_time_cat = 
           if_else(res_time < 5, "under_5",
                   if_else(res_time >= 5 & res_time <= 350, "5_to_350",
                           if_else(res_time > 350 & res_time <= 3000, "351_to_3000",
                                   if_else(res_time > 3000, "above_3000", "none"))))) |> 
  select(SubdivisionName_clean, res_time_cat) |> 
  group_by(SubdivisionName_clean, res_time_cat) |> 
  mutate(n = n()) |> 
  distinct() |> 
  ungroup() |>
  filter(!is.na(SubdivisionName_clean)) |> 
  group_by(SubdivisionName_clean) |> 
  mutate(pc = round(n/sum(n), 2), 
         sd_tot_n = sum(n)) |> 
  filter(SubdivisionName_clean %in% c(
           "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
           "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |>
  filter(res_time_cat %in% c("under_5", "5_to_350"))


dumbell_plot <- ggplot(dumbell, aes(x = pc, y = reorder(SubdivisionName_clean, n), fill = res_time_cat)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_tsecl() +
  labs(x = "Proportion of Complaints", y = "", fill = "Resolution Time", 
       caption = 
         "Under 5 hrs category: Average = 1.76 hrs, Median = 1.57 hrs
       5-350 hrs category: Average = 16.2 hrs, Median = 10.4 hrs") +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("#E2D200", "#DD8D29"),
                    labels = c("5-350 hours per complaint", "Under 5 hours  per complaint")) +
  geom_text(aes(label = comma(n)), hjust = 1, vjust = -1,
            color = "#7c1419",size = 4, position="stack") +
  geom_text(aes(label = percent(pc)), hjust = 1, vjust = 1.7,
            color = "white",size = 4, position="stack") + 
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(text = element_text(family = "Georgia"), legend.position = "bottom") +
  theme(plot.caption = element_text(size = 12, face = "italic")) 

  #scale_x_discrete(limits=c("2", "0.5", "1")) +
  #annotate("text", x=1.04, y=10, label="(18,347)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=9, label="(16,092)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=8, label="(15,950)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=7, label="(15,782)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=6, label="(13,684)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=5, label="(13,085)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=4, label="(12,852)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=3, label="(11,691)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=2, label="(10,526)", size=4, color="#7c1419") +
  #annotate("text", x=1.04, y=1, label="(9,202)", size=4, color="#7c1419") +
  
  
-------------------
ggplot(dumbell, aes(x = pc_5_to_350, xend = pc_under_5, 
                    y = SubdivisionName_clean, group = SubdivisionName_clean)) +
  geom_dumbbell(
    color = "white",
    color_xend = "blue",
    size = 4,
    dot_guide = T,
    dot_guide_size = 0.15,
    dot_guide_colour = "red"
  )
--------------------
dumbell_under5 <- dumbell |> filter(res_time_cat == "under_5")
dumbell_5to350 <- dumbell |> filter(res_time_cat == "5_to_350")  
  
ggplot(dumbell) +
  geom_segment(data = dumbell_under5, 
               aes(x = pc, y = SubdivisionName_clean,
                   yend = dumbell_5to350$SubdivisionName_clean, 
                   xend = dumbell_5to350$pc),
               color = "#aeb6bf",size = 4.5,alpha = .5) +
  geom_point(aes(x = pc, y = SubdivisionName_clean, color = res_time_cat), 
             size = 4, show.legend = TRUE) +
  scale_x_continuous(label = percent) +
  theme_tsecl()

  
  group_by(SubdivisionName_clean) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(pc = percent(n/sum(n), accuracy = 0.01)) |> 
  arrange(desc(n))
          

  filter(SubdivisionName_clean %in% c(
    "DURJOYNAGAR", "BORDOWALI RURAL", "JOGENDRANAGAR","AMTALI", "BANAMALIPUR 1", 
    "GB", "IGM","DURGACHOWMOHANI", "BANAMALIPUR 2", "BORDOWALI URBAN")) |> 
  mutate(res_time = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time >= 0) |>  
  mutate(res_time_cat = 
           if_else(res_time < 5, "under_5",
                   if_else(res_time >= 5 & res_time <= 350, "5_to_350",
                           if_else(res_time > 350 & res_time <= 3000, "351_to_3000",
                                   if_else(res_time > 3000, "above_3000", "none"))))) |> 
    

# Viz9:Area chart
merged_tsecl |> 
    mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
           month = month(ComplaintDate_clean, label = T, abbr = T),
           year = year(ComplaintDate_clean)) |> 
    filter(res_time_hrs >= 0) |> 
    mutate(res_time_cat = 
             if_else(res_time_hrs < 5, "< 5 hrs",
                     if_else(res_time_hrs >= 5 & res_time_hrs <= 350, "5-350 hrs",
                             if_else(res_time_hrs > 3000, "> 3000 hrs", "none")))) |>
    group_by(year, month, res_time_cat) |> 
    summarise(N = n()) |>
    ggplot(aes(x = month, y = N, group = res_time_cat, fill = res_time_cat)) +
    geom_area(linewidth = 1) +
    labs(x = "", y = "Number of Complaints", color = "", fill = "Resolution Time",
         caption = "October 2020 has 21 complaints with resolution time > 3000 
       hours belonging to 'R, Y, or B Fuse Replace' Rectification Type.") +
    scale_fill_manual(values = wes_palette("FantasticFox1")) +
    facet_grid(rows = vars(year)) +
    theme_tsecl() +
    theme(strip.text = element_text(size = 13)) +
    theme(legend.position = c(0.2, 0.85)) +
    theme(text = element_text(family = "Georgia"))
  
# Viz 10.1: Table
table_10 <- merged_tsecl |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600,
         Month = month(ComplaintDate_clean, label = T, abbr = T),
         Year = year(ComplaintDate_clean)) |> 
  filter(res_time_hrs >= 0) |> 
  mutate(res_time_cat = 
           if_else(res_time_hrs < 5, "Under 5 hrs",
                   if_else(res_time_hrs >= 5 & res_time_hrs <= 350, "5-350 hrs",
                           if_else(res_time_hrs > 3000, "3000+ hrs", "none")))) |> 
  select(Year, Month, res_time_hrs, res_time_cat, Rectification)

table_10_1 <- table_10 |> 
  select(Year, res_time_cat) |> 
  group_by(Year, res_time_cat) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(Year) |> 
  mutate(pc = percent(round(n/sum(n), 4), 0.01)) |> 
  select(Year, res_time_cat, pc) |> 
  pivot_wider(names_from = res_time_cat, values_from = pc) |> 
  #mutate(`3000+ hrs` = ifelse(is.na(.), 0, .)) |> 
  relocate('3000+ hrs', .after = '5-350 hrs') |> 
  relocate('Under 5 hrs', .before = '5-350 hrs') |> 
  as.tibble() |> 
  gt()
  
table_10_2 <- table_10 |>  
  filter(Month %in% c("Apr", "May", "Jun")) |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |> 
  select(Year, Month, res_time_cat, Rectification) |> 
  group_by(Year, Month, res_time_cat, Rectification) |> 
  summarise(n = n()) |> 
  ungroup() |>
  pivot_wider(names_from = c(res_time_cat), values_from = n) |> 
  relocate('Under 5 hrs', .before = '5-350 hrs') |>
  pivot_wider(names_from = c(Year, Month), values_from = c('Under 5 hrs', '5-350 hrs'))

write.csv(table_10_2, "charts_2/table_10_2.csv")
  
table_10_3 <- table_10 |>  
  filter(Month %in% c("Aug", "Sep", "Oct")) |> 
  filter(Rectification %in% c("R,Y OR B FUSE REPLACE", 
                              "IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT",
                              "NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED",
                              "PATROLLING OF FEEDER TO RESOLVE THE FAULT",
                              "IN CASE OF CARBON RECONNECT THE CABLE")) |> 
  select(Year, Month, res_time_cat, Rectification) |> 
  group_by(Year, Month, res_time_cat, Rectification) |> 
  summarise(n = n()) |> 
  ungroup() |>
  pivot_wider(names_from = c(Year, Month, res_time_cat), values_from = n)
  
write.csv(table_10_3, "charts_2/table_10_3.csv")

# June 2023
##################### Analysis Resolution Time #####################

restime_0_350 <- merged_tsecl |> 
  mutate(res_time_hrs = as.numeric((ComplaintCloseDate_clean - ComplaintDate_clean))/3600) |> 
  filter(res_time_hrs >= 0 & res_time_hrs <= 350) |> 
  mutate(Z = (res_time_hrs - mean(res_time_hrs, na.rm = T))/sd(res_time_hrs, na.rm=T)) |> 
  mutate(res_time_cat = 
           if_else(res_time_hrs >= 0 & res_time_hrs < 2, "0-2 hrs",
                   if_else(res_time_hrs >= 2 & res_time_hrs < 8, "2-8 hrs",
                           if_else(res_time_hrs >= 8 & res_time_hrs < 24, "8-24 hrs",
                                   if_else(res_time_hrs >= 24, "24+ hrs", "none"))))) |> 
  mutate(year = year(ComplaintCloseDate_clean), month = month(ComplaintDate_clean, label = T, abbr = T)) |> 
  mutate(res_time_cat = factor(res_time_cat, levels=c('24+ hrs','8-24 hrs','2-8 hrs','0-2 hrs'))) |> 
  mutate(complaint_cat =
    if_else(ComplaintType == "ALL AREA NO POWER", "Technical",
    if_else(ComplaintType == "NO CURRENT COMPLAINT", "Technical",
    if_else(ComplaintType == "OTHER COMPLAINT", "Technical",
    if_else(ComplaintType == "OTHER TECHNICAL", "Technical",
    if_else(ComplaintType == "SAFETY RELATED", "Technical",
    if_else(ComplaintType == "SERVICE LINE RELATED", "Technical",
    if_else(ComplaintType == "TRANSFORMER FAILURE", "Technical", "Commercial"))))))))


# Density Plot
restime_0_350 |> 
  ggplot(aes(x = log(Z))) +
  geom_density(fill = "#e09500", color = "#e09500", alpha = 0.6, ) +
  geom_vline(xintercept = 2.564427, color = "red", linetype=2) +
  #annotate("text", x=2.4, y=0.4, label="Resolution Time = 24 hours", 
   #        angle=90, size=4, color="grey37") +
  geom_vline(xintercept = -0.2089951, color = "red", linetype=2) +
  #annotate("text", x= -0.21, y=0.4, label="2 hours", 
  #         angle=90, size=4, color="grey37")+
  geom_vline(xintercept = 0.1691988, color = "red", linetype=2) +
  #annotate("text", x=0.14, y=0.4, label="5 hours", 
    #       angle=90, size=4, color="grey37")+
  scale_x_log10() +
  labs(x = "Resolution Time", y = "", fill = "", 
       caption = "0-2hrs 95742 57%, 2-5hrs 50736 30%, 5-24hrs 19007 11%, 24+hrs 3855 2%") +
  theme_tsecl()

# mean & sd of res_time_hrs is 3.657841 & 7.932439

tabyl(restime_0_350, res_time_cat) # count & pc

# Commercial & Technical
# Complaint type % commercial/technical
restime_0_350 |> 
  select(ComplaintType,complaint_cat) |>
  group_by(ComplaintType,complaint_cat) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> 
  ungroup() |> 
  mutate(pc = n/sum(n)) |> 
  filter(pc >= 0.006) |> 
  ggplot(aes(x = reorder(ComplaintType, pc), y = pc, fill = complaint_cat)) + 
  geom_col() +
  coord_flip()+
  labs(x = 'Complaint Type', y = '% of complaints', fill = '', caption = 
'Less than 1% Complaint types (Written Complaint, 
Theft Information, On Door Connection Request, 
Transformer Failure, Harassment By Discom Official, 
Energy Theft) are excluded.') +
  theme_tsecl() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = percent(pc, accuracy = 1)), hjust = 0, vjust = -0.15, color = "grey10", size = 5.5, position = position_stack(vjust = 1)) +
  geom_text(aes(label = scales::comma(n)),hjust = 0,color = "grey10",size = 4.5, vjust = 1.5,
           position = position_stack(vjust = 1))+
  scale_fill_manual(values = c('#f77f00','#003049')) +
  #scale_x_discrete(labels = wrap_format(15)) +
  theme(plot.margin = margin(1,0,1,1, "cm"), legend.position = 'top')+
  theme(plot.caption = element_text(hjust = 0))
  

# Complaint Types across Commercial / Technical & year
comm_tech_year <- restime_0_350 |> 
  group_by(year, complaint_cat, res_time_cat) |> 
  count() |> 
  group_by(year, complaint_cat) |> 
  mutate(pc = n/sum(n)) |> view()

ggplot(comm_tech_year, aes(x = complaint_cat,  fill = fct_rev(res_time_cat), y = pc)) +
  geom_col() +
  facet_grid(~year) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = subset(comm_tech_year, pc > 0.019) ,aes(label = percent(pc, accuracy = 1)),color = "grey10", size = 6.5, vjust = -0.7, position = position_stack(vjust = 0.5)) +
  geom_text(data = comm_tech_year[!comm_tech_year$n %in% c(12,46,141,578,16,924,70,2692),],aes(label = scales::comma(n)),vjust = 0.8,color = "grey10",size = 5.5,position = position_stack(vjust = 0.4))+
  labs(x = "Complaint Category", y = "% of Complaints", fill = "Resolution Time", 
       caption = "Technical Complaints: ALL AREA NO POWER, NO CURRENT COMPLAINT, OTHER COMPLAINT, OTHER TECHNICAL, SAFETY RELATED, SERVICE LINE RELATED, TRANSFORMER FAILURE.
Commercial Complaints: BILL RELATED, COMMERCIAL REQUEST, ENERGY THEFT,HARASSMENT BY DISCOM OFFICIAL,METER RELATED, ON DOOR CONNECTION REQUEST, THEFT INFORMATION, WRITTEN COMPLAINT.") +
  theme_tsecl() +
  theme(plot.caption = element_text(size = 8, face = "italic", hjust = 0, vjust = -2)) + 
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  theme(axis.title = element_text(family = "Georgia",size = 28), 
        axis.text = element_text(family = "Georgia", size = 26),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20, hjust = 0),
        strip.text = element_text(size = 20))

# Stacked bar chart: Technical 
restime_0_350$res_time_cat <- factor(restime_0_350$res_time_cat, levels=c('24+ hrs', '8-24 hrs', '2-8 hrs','0-2 hrs'))

stack_tech <- restime_0_350 |> 
  filter(complaint_cat == "Technical") |> 
  select(res_time_cat, ComplaintType) |> 
  group_by(ComplaintType, res_time_cat) |> 
  count() |> 
  ungroup() |> 
  group_by(ComplaintType) |> 
  mutate(pc = n/sum(n)) |> 
  group_by(ComplaintType) |> 
  mutate(N = sum(n))
  
ggplot(stack_tech, aes(x = reorder(ComplaintType, N),  fill = res_time_cat, y = pc)) +
  geom_col(position = 'stack')+
  labs(title = "Complaints under Technical Category", x = "Complaint Type", y = "% of Complaints", fill = "Resolution Time", 
       caption = "") +
  coord_flip()+
  #geom_text(data = subset(stack_tech, pc >0.01), 
  #aes(label = percent(pc, accuracy = 1)), 
  #hjust = 0,vjust = -0.5, color = "grey10", size = 6.5, 
  #position = position_stack(vjust = 0.5)) +
  #geom_text(data = stack_tech[!stack_tech$n %in% c(1526,92,3),], aes(label = scales::comma(n)),color = "grey10",size = 5.5,vjust = 1.5,hjust = 0,check_overlap = T,position = position_stack(vjust = 0.5))+
  scale_y_continuous(labels = scales::percent) +
  theme_tsecl() +
  theme(legend.direction = "horizontal", legend.position = "top") +
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  guides(fill = guide_legend(reverse=TRUE))

# Stacked bar chart: Commercial 
stack_commer <- restime_0_350 |> 
  filter(complaint_cat == "Commercial") |> 
  select(res_time_cat, ComplaintType)

stack_commer$ComplaintType[stack_commer$ComplaintType == 'ENERGY THEFT'] <- 'THEFT'
stack_commer$ComplaintType[stack_commer$ComplaintType == 'THEFT INFORMATION'] <- 'THEFT'

stack_commer <- stack_commer |> 
  group_by(ComplaintType, res_time_cat) |> 
  count() |> 
  ungroup() |> 
  group_by(ComplaintType) |> 
  mutate(pc = n/sum(n)) |> 
  filter(ComplaintType != 'HARASSMENT BY DISCOM OFFICIAL') |> 
  group_by(ComplaintType) |> 
  mutate(N = sum(n))


ggplot(stack_commer, aes(x = reorder(ComplaintType, N),  fill = res_time_cat, y = pc)) +
  geom_col() +
  labs(title = "Complaints under Commercial Category", x = "Complaint Type", y = "% of Complaints", fill = "Resolution Time", 
       caption = "N = 7337.
       BILL RELATED N = 4048, COMMERCIAL REQUEST N = 1696, METER RELATED N = 1323, 
       ON DOOR CONNECTION REQUEST N = 16, THEFT N = 109, WRITTEN COMPLAINT N = 145") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  #geom_text(data = subset(stack_commer, pc >=0.01),aes(label = percent(pc, accuracy = 1)), vjust = 0, color = "grey15", size = 6.5, position = position_stack(vjust = 0.5)) +
  #geom_text(data = stack_commer[!stack_commer$n %in% c(7),],
   # aes(label = scales::comma(n)),color = "grey15",size = 5.5, vjust=2,check_overlap = T,position = position_stack(vjust = 0.5))+
  theme_tsecl() +
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  theme(legend.direction = "horizontal", legend.position = "top") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_x_discrete(labels = wrap_format(20))


# Res cat across years
janitor::tabyl(restime_0_350,res_time_cat, year) |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting()

restime_0_350$res_time_cat <- factor(restime_0_350$res_time_cat, levels=c('0-2 hrs','2-8 hrs','8-24 hrs','24+ hrs'))

res_cat_years <- tabyl(restime_0_350,res_time_cat, year) |> 
  pivot_longer(!res_time_cat, names_to = "year", values_to = "n") |> 
  group_by(year) |> 
  mutate(pc = n/sum(n)) |> 
  ungroup() |> 
  group_by(res_time_cat) |> 
  mutate(avg = mean(n), avg_pc = mean(pc)) |> 
  ungroup() |>  view()

ggplot(res_cat_years, aes(x = res_time_cat, y = pc, color = year)) +
  geom_point(position = position_dodge2(w = 0.65), size = 9) +
  geom_segment(aes(x=0.65,xend=1.4,y=0.557,yend=0.557), color = "darkgrey", size = 1.5) +
  geom_segment(aes(x=1.65,xend=2.4,y=0.3528,yend=0.3528), color = "darkgrey", size = 1.5) +
  geom_segment(aes(x=2.65,xend=3.4,y=0.066,yend=0.066), color = "darkgrey", size = 1.5) +
  geom_segment(aes(x=3.65,xend=4.4,y=0.02339,yend=0.02339), color = "darkgrey", size = 1.5) +
  labs(y = "Proportion of complaints", color = "Year", x = "Resolution Time") +
  geom_segment(aes(x=0.785,xend=0.785,y=0.5153,yend=0.557),color = "#00008B",size = 1) +
  geom_segment(aes(x=1,xend=1,y=0.634,yend=0.557),color = "#00FFFF",size = 1) +
  geom_segment(aes(x=1.215,xend=1.215,y=0.5236,yend=0.557),color = "#89CFF0",size = 1) +
  geom_segment(aes(x=1.785,xend=1.785,y=0.3601,yend=0.3528),color = "#00008B",size = 1) +
  #geom_segment(aes(x=2,xend=2,y=0.3409,yend=0.295),color = "#00A08A",size = 1) +
  geom_segment(aes(x=2.215,xend=2.215,y=0.3441,yend=0.3528),color = "#89CFF0",size = 1) +
  geom_segment(aes(x=2.785,xend=2.785,y=0.0965,yend=0.066),color = "#00008B",size = 1) +
  geom_segment(aes(x=3,xend=3,y=0.0093,yend=0.066),color = "#00FFFF",size = 1) +
  geom_segment(aes(x=3.215,xend=3.215,y=0.0922,yend=0.066),color = "#89CFF0",size = 1) +
  #geom_segment(aes(x=3.785,xend=3.785,y=0.0279,yend=0.02339),color = "#FF0000",size = 1) +
  geom_segment(aes(x=4,xend=4,y=0.00229,yend=0.02339),color = "#00FFFF",size = 1) +
  geom_segment(aes(x=4.215,xend=4.215,y=0.0399,yend=0.02339),color = "#89CFF0",size = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c('#00008B','#00FFFF','#89CFF0')) +
  # annotate("text", x=0.785, y=0.49, label="52%", size=4.5, color="#FF0000") +
  # annotate("text", x=0.785, y=0.47, label="(n=17,349)", size=4.5, color="#FF0000") +
  # annotate("text", x=1, y=0.654, label="63%", size=4.5, color="#00A08A") +
  # annotate("text", x=1, y=0.674, label="(n=42,194)", size=4.5, color="#00A08A") +
  # annotate("text", x=1.215, y=0.5036, label="52%", size=4.5, color="#F2AD00") +
  # annotate("text", x=1.215, y=0.48, label="(n=36,199)", size=4.5, color="#F2AD00") +
  # annotate("text", x=1.765, y=0.38, label="36%", size=4.5, color="#FF0000") +
  # annotate("text", x=1.765, y=0.4, label="(n=12,125)", size=4.5, color="#FF0000") +
  # annotate("text", x=2.115, y=0.372, label="35%", size=4.5, color="#00A08A") +
  # annotate("text", x=2.115, y=0.392, label="(n=23,576)", size=4.5, color="#00A08A") +
  # annotate("text", x=2.215, y=0.3241, label="34%", size=4.5, color="#F2AD00") +
  # annotate("text", x=2.215, y=0.3041, label="(n=23,792)", size=4.5, color="#F2AD00") +
  # annotate("text", x=2.785, y=0.119, label="10%", size=4.5, color="#FF0000") +
  # annotate("text", x=2.785, y=0.139, label="(n=3,250)", size=4.5, color="#FF0000") +
  # annotate("text", x=2.82, y=0.0283, label="1%", size=4.5, color="#00A08A") +
  # annotate("text", x=2.82, y=0.0083, label="(n=624)", size=4.5, color="#00A08A") +
  # annotate("text", x=3.215, y=0.11, label="9%", size=4.5, color="#F2AD00") +
  # annotate("text", x=3.215, y=0.13, label="(n=6,376)", size=4.5, color="#F2AD00") +
  # annotate("text", x=3.785, y=0.056, label="3%", size=4.5, color="#FF0000") +
  # annotate("text", x=3.785, y=0.076, label="(n=940)", size=4.5, color="#FF0000") +
  # annotate("text", x=4.18, y=0.005, label="0%", size=4.5, color="#00A08A") +
  # annotate("text", x=4.18, y=-0.019, label="(n=153)", size=4.5, color="#00A08A") +
  # annotate("text", x=4.215, y=0.0599, label="4%", size=4.5, color="#F2AD00") +
  # annotate("text", x=4.215, y=0.0799, label="(n=2,762)", size=4.5, color="#F2AD00") +
  theme_tsecl()+
  theme(axis.title = element_text(family = "Georgia",size = 28), 
        axis.text = element_text(family = "Georgia", size = 26),
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20, hjust = 0)
        )
  

# Dodge bar chart - subcomplaint category (technical)
restime_0_350 |> 
  filter(complaint_cat == "Technical") |> 
  group_by(SubComplaintType, res_time_cat) |> 
  summarise(n = n()) |> 
  filter(SubComplaintType %in% c('ALL AREA NO POWER-LT ISSUE','ALL AREA NO POWER-HT ISSUE',
                                 ''))
  #arrange(desc(N)) |> 
  filter(n > 50) |> 
  ungroup() |> 
  group_by(ComplaintType) |> 
  mutate(N = sum(n)) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(ComplaintType, -N), y=n,fill = factor(SubComplaintType))) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Major subcomplaints under 8-24 hrs resolution time",x = "techinal complaints")
  
# Subcomplaint
subcomplaint_restime <- restime_0_350 |> 
  filter(ComplaintType %in% c('ALL AREA NO POWER')) |> # 'SERVICE LINE RELATED','OTHER COMPLAINT'
  group_by(SubComplaintType, res_time_cat) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |>
  filter(n > 20) |> 
  ungroup() |> 
  group_by(SubComplaintType) |> 
  mutate(pc = n/sum(n)) 

subcomplaint_restime$SubComplaintType[subcomplaint_restime$SubComplaintType == 'ALL AREA NO POWER-LT ISSUE'] <- 'LT ISSUE'
subcomplaint_restime$SubComplaintType[subcomplaint_restime$SubComplaintType == 'ALL AREA NO POWER-HT ISSUE'] <- 'HT ISSUE'

ggplot(subcomplaint_restime, 
       aes(x = reorder(SubComplaintType, -n), y=pc,fill = res_time_cat)) +
  geom_col() +
  labs(y= "% of Complaints", x = "ALL AREA NO POWER", fill='Resolution Time', 
       caption = "LT issue, N = 17247 & HT issue, N = 6693.") +
  theme_tsecl()+
  scale_y_continuous(labels = scales::percent) +
  #geom_text(aes(label = percent(pc, accuracy = 1)), color = "grey15", size = 6.5, position = position_stack(vjust = 0.5), vjust = -0.5) +
  #geom_text(data = subset(subcomplaint_restime, n > 512), aes(label = scales::comma(n)),color = "grey15",size = 5.5, vjust=1.1, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(legend.direction = "horizontal", legend.position = "top")
  

subcomplaint_restime <- restime_0_350 |> 
  filter(ComplaintType %in% c('SERVICE LINE RELATED')) |> 
  group_by(SubComplaintType, res_time_cat) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |>
  filter(n > 20) |> 
  ungroup() |> 
  group_by(SubComplaintType) |> 
  mutate(pc = n/sum(n)) 

ggplot(subcomplaint_restime, 
       aes(x = reorder(SubComplaintType, -n), y=pc,fill = res_time_cat)) +
  geom_col() +
  labs(y= "% of Complaints", x = "SERVICE LINE RELATED", fill='Resolution Time',
       caption = "Broken N = 9158, Hanged, N = 8328, Tored N = 685, Service Line Reconnection N = 1173") +
  theme_tsecl()+
  scale_y_continuous(labels = scales::percent) +
  #geom_text(aes(label = percent(pc, accuracy = 1)), color = "grey15", size = 6.5, position = position_stack(vjust = 0.5), vjust = -0.5) +
  #geom_text(data = subcomplaint_restime[!subcomplaint_restime$n %in% c(269,29,31),], aes(label = scales::comma(n)),color = "grey15",size = 5.5, vjust=1.1, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(legend.direction = "horizontal", legend.position = "top") +
  theme(axis.text = element_text(size = 19), axis.title = element_text(size = 21))

subcomplaint_restime <- restime_0_350 |> 
  filter(ComplaintType %in% c('OTHER COMPLAINT')) |> 
  group_by(SubComplaintType, res_time_cat) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |>
  filter(n > 20) |> 
  ungroup() |> 
  group_by(SubComplaintType) |> 
  mutate(pc = n/sum(n)) 

ggplot(subcomplaint_restime, 
       aes(x = reorder(SubComplaintType, -n), y=pc,fill = res_time_cat)) +
  geom_col() +
  labs(y= "% of Complaints", x = "OTHER COMPLAINT", fill='Resolution Time') +
  theme_tsecl()+
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = percent(pc, accuracy = 1)), color = "grey15", size = 6.5, position = position_stack(vjust = 0.5), vjust = -0.5) +
  geom_text(data = subcomplaint_restime[!subcomplaint_restime$n %in% c(44),], aes(label = scales::comma(n)),color = "grey15",size = 5.5, vjust=1.1, position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme(legend.direction = "horizontal", legend.position = "top")

  

# Dodge bar chart - subcomplaint category (commercial)
restime_0_350 |> 
  filter(complaint_cat == "Commercial", res_time_cat %in% c("8-24 hrs")) |> 
  select(ComplaintType, SubComplaintType) |> 
  group_by(ComplaintType, SubComplaintType) |> 
  summarise(n = n()) |> 
  arrange(desc(n)) |> 
  filter(n > 5) |> 
  ungroup() |> 
  group_by(ComplaintType) |> 
  mutate(N = sum(n)) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(ComplaintType, -N), y=n,fill = factor(SubComplaintType))) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Major subcomplaints in 8-24 hrs resolution time", x = "commercial complaints")

restime_0_350 |> 
  filter(complaint_cat == "Commercial", res_time_cat %in% c("24+ hrs")) |> 
  select(ComplaintType, SubComplaintType) |> 
  group_by(ComplaintType, SubComplaintType) |> 
  summarise(n = n()) |> 
  #arrange(desc(N)) |> 
  #filter(n > 50) |> 
  ungroup() |> 
  group_by(ComplaintType) |> 
  mutate(N = sum(n)) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(ComplaintType, -N), y=n,fill = factor(SubComplaintType))) +
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Major subcomplaints in 24+ hrs resolution time", x = "commercial complaints")

# Area chart - Month, Year, Res Cat
area_chart <- restime_0_350 |> 
  group_by(year, month, res_time_cat) |> 
  summarise(N = n()) |>
  group_by(year, month) |> 
  mutate(pc = N/sum(N)) |> #view()
  filter(N >1) |> 
  mutate(res_time_cat = fct_rev(res_time_cat)) 

new_rows <- data.frame(
  year = c(2020, 2020),
  month = c('Nov', 'Nov'),
  res_time_cat = c("2-8 hrs", "8-24 hrs"),
  N = c(0,0),
  pc = c(0,0)
)

area_chart <- rbind(area_chart, new_rows)
area_chart$month <- factor(area_chart$month, levels = c('Jan','Feb', 'Mar','Apr','May','Jun', 'Jul','Aug',
                                                        'Sep','Oct','Nov','Dec'))
area_chart$res_time_cat = factor(area_chart$res_time_cat, levels = c('0-2 hrs','2-8 hrs','8-24 hrs','24+ hrs'))

ggplot(area_chart,aes(x = month, y = pc, group = fct_rev(res_time_cat), fill = fct_rev(res_time_cat))) +
  geom_area(linewidth = 1) +
  labs(x = "", y = "% of Complaints", fill = "Resolution Time") +
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  facet_grid(rows = vars(year)) +
  scale_y_continuous(labels = scales::percent, n.breaks = 3)+
  #geom_text(aes(label = comma(N)),color = "grey15",size = 4.5, hjust = -0.1,vjust=0,check_overlap = T,position = position_stack(vjust = 0))+  
  theme_tsecl() +
  theme(strip.text = element_text(size = 13)) +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(reverse = TRUE))
  


############ Sankey: Complaint Type, Cause,Outage Type, Rectification ##################

df_sankey <- restime_0_350 |> 
  filter(!is.na(OutageType)) |> 
  select(ComplaintType, Cause, OutageType, Rectification)
#group_by(OutageType, Cause, Rectification) |> 
#count(sort=T) |>  view()

df_sankey$ComplaintType[df_sankey$ComplaintType == 'BILL RELATED'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'COMMERCIAL REQUEST'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'ENERGY THEFT'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'HARASSMENT BY DISCOM OFFICIAL'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'METER RELATED'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'ON DOOR CONNECTION REQUEST'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'THEFT INFORMATION'] <- 'COMMERCIAL COMPLAINT'
df_sankey$ComplaintType[df_sankey$ComplaintType == 'WRITTEN COMPLAINT'] <- 'COMMERCIAL COMPLAINT'

df_sankey$Rectification[df_sankey$Rectification == 'BUSHING REPLACE OR BYPASS'] <- 'BUSHING REPLACE/BYPASS'
df_sankey$Rectification[df_sankey$Rectification == 'FUSE BASE REPLACE OR BYPASS'] <- 'FUSE BASE REPLACE/BYPASS'
df_sankey$Rectification[df_sankey$Rectification == 'IF SERVICE CABLE IS LOOSE THEN TIGHTEN IT'] <- 'SERVICE CABLE LOOSE TIGHTEN IT'
df_sankey$Rectification[df_sankey$Rectification == 'IN CASE OF CARBON RECONNECT THE CABLE'] <- 'RECONNECT CABLE IF CARBON'
df_sankey$Rectification[df_sankey$Rectification == 'NEUTRAL REPLACE OR BYPASS'] <- 'NEUTRAL REPLACE/BYPASS'
df_sankey$Rectification[df_sankey$Rectification == 'NEW CABLE REPLACE OR JOINT CABLE IF REQUIRED'] <- 'NEW CABLE REPLACE/JOINT CABLE'
df_sankey$Rectification[df_sankey$Rectification == 'PATROLLING OF FEEDER TO RESOLVE THE FAULT'] <- 'FEEDER PATROLLING'
df_sankey$Rectification[df_sankey$Rectification == 'R,Y OR B FUSE REPLACE'] <- 'R,Y,B FUSE REPLACE'
df_sankey$Rectification[df_sankey$Rectification == 'REPLACE KNIFE SWITCH OR BYPASS'] <- 'REPLACE KNIFE SWITCH/BYPASS'

df_sankey$ComplaintType <- factor(df_sankey$ComplaintType, levels = c("OTHER TECHNICAL", "NO CURRENT COMPLAINT", "ALL AREA NO POWER","SERVICE LINE RELATED", "OTHER COMPLAINT","SAFETY RELATED","TRANSFORMER FAILURE","COMMERCIAL COMPLAINT"))


df_sankey <- df_sankey |> 
  make_long(ComplaintType, Cause, OutageType,Rectification) 

comp_type_sankey <- restime_0_350 |> 
  group_by(ComplaintType) |> 
  summarise(n = n()) |> 
  mutate(pc = n/sum(n))

p1 <- ggplot(df_sankey, 
             aes(x = x,next_x = next_x,node = node,next_node = next_node,fill=factor(node),label = node)) +
  geom_sankey(flow.alpha=0.5, node.color="white",show.legend = F) +
  #geom_sankey_label(size=2,color="black", fill="white", hjust = "outward",label.size = 0.05) +
  theme_tsecl() +
  theme(axis.title = element_blank(),panel.grid = element_blank(),axis.ticks = element_blank(),
    axis.text.y = element_blank(),legend.position = "none",panel.background = element_blank(),
    axis.line = element_blank()) + labs(title = "", subtitle="", x = "", y="") +
  scale_fill_viridis_d(option = "C")
p1

tabyl(df_sankey, ComplaintType)

# Geography (Division)
wtripura <- restime_0_350 |> 
  filter(Circle %in% c("CIRCLE 1 (W. TRIPURA)","CIRCLE 2 (W. TRIPURA)")) |>
  group_by(Division, res_time_cat) |> 
  summarise(n = n()) |>
  group_by(Division) |> 
  mutate(pc = n/sum(n), N = sum(n)) 
  #mutate(res_time_cat = fct_rev(res_time_cat)) |> 

ggplot(wtripura, aes(x = reorder(Division, N),  fill = res_time_cat, y = pc)) +
  geom_col() +
  coord_flip() +
  labs(title = "", x = "West Tripura Divisions", y = "% of Complaints", fill = "Resolution Time", 
       caption = "N = 155,921.
       AGARTALA 1 N = 48746, AGARTALA 2 N = 68992, CAPITAL COMPLEX N = 36797, JIRANIA N = 821, MOHANPUR N = 565") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  #geom_text(aes(label = percent(pc, accuracy = 1)), vjust = -0.5, color = "grey15", size = 6.5, position = position_stack(vjust = 0.5)) +
  #geom_text(data = wtripura[!wtripura$n %in% c(614),], aes(label = comma(n)),color = "grey15",size = 5.5, vjust= 1.2, position = position_stack(vjust = 0.5))+
  theme_tsecl() +
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  theme(legend.direction = "horizontal", legend.position = "top") +
  guides(fill = guide_legend(reverse=T))

# Geography (Subdivision)
wtripura_sd <- restime_0_350 |> 
  filter(Circle %in% c("CIRCLE 1 (W. TRIPURA)","CIRCLE 2 (W. TRIPURA)")) |>
  group_by(SubdivisionName_clean, res_time_cat) |> 
  summarise(n = n()) |>
  group_by(SubdivisionName_clean) |> 
  mutate(pc = n/sum(n), N = sum(n)) |> 
  filter(N > 5000)

ggplot(wtripura_sd, aes(x = reorder(SubdivisionName_clean, N),  fill = res_time_cat, y = pc)) +
  geom_col() +
  coord_flip() +
  labs(title = "", x = "West Tripura Subdivisions", y = "% of Complaints", fill = "Resolution Time", 
       caption = "N = 154,535. AMTALI N = 15780, BANAMALIPUR 1 N = 13684, BANAMALIPUR 2 N = 10523, 
       BORDOWALI RURAL N = 16091, BORDOWALI URBAN N = 9201, CAPITAL COMPLEX N = 5371, 
       DURGACHOWMOHANI_R N = 11690, DURJOYNAGAR N = 18342, GB N = 13084, IGM N = 12849, 
       JOGENDRANAGAR N = 15949, PRATAPGARH N = 6950, SEKERKOTE N = 5021") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()+
  #geom_text(data = subset(wtripura_sd, pc >0.019), aes(label = percent(pc, accuracy = 1)), vjust = -0.3, color = "grey15", size = 6.5, position = position_stack(vjust = 0.5)) +
  #geom_text(data = subset(wtripura_sd, n > 201),aes(label = comma(n)),color = "grey15",size = 5.5, vjust= 1.2, position = position_stack(vjust = 0.5))+
  theme_tsecl() +
  scale_fill_manual(values = c('#dc143c','#ffc40c','#90ee90','#3cb371')) +
  theme(legend.direction = "horizontal", legend.position = "top") +
  guides(fill = guide_legend(reverse=T))

##################################### END #####################################