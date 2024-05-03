####################  Load libraries ####################
library(sf)
library(geojsonsf)
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)

####################  Load data ####################
ind_shp <- geojson_sf("https://raw.githubusercontent.com/AyushBipinPatel/CSD-Workshop-for-Programming-in-R/main/india-districts-2019-734.geojson")

merged_tsecl <- read_csv("data-preped/merged_tsecl.csv")
merged_fedco <- read_csv("data-preped/merged_fedco.csv")
merged_sai <- read_csv("data-preped/merged_sai.csv")

####################  Clean data ####################

# Filter Tripura state
tripura_shp <- ind_shp |> 
  filter(st_nm == "Tripura") |> 
  mutate(district = toupper(district))

# Combine tsecl fedco sai
merged_tsecl_circle <- merged_tsecl[,c("Circle", "ComplaintDate_clean")]

merged_fedco <- merged_fedco |> 
  mutate(Circle = 
           ifelse(Division == "Ambassa", "DHALAI",
           ifelse(Division == "Manu", "DHALAI",
           ifelse(Division == "Mohanpur", "CIRCLE 2 (W. TRIPURA)",
           ifelse(Division == "Sabroom", "BELONIA", NA)))))

merged_fedco_circle <- merged_fedco[, c(39, 37)] 

merged_sai_circle <- merged_sai[, c(11, 14)]

colnames(merged_tsecl_circle)<- c("Circle","date_time")
colnames(merged_fedco_circle)<- c("Circle","date_time")
colnames(merged_sai_circle)<- c("Circle","date_time")
merged_circle_all <- rbind(merged_tsecl_circle, merged_fedco_circle, merged_sai_circle) 

# Add district column
merged_circle_all <- merged_circle_all |> 
  mutate(
    District = ifelse(Circle == "GOMATI", "GOMATI",
               ifelse(Circle == "DHALAI", "DHALAI",
               ifelse(Circle == "CIRCLE 1 (W. TRIPURA)", "WEST TRIPURA",
               ifelse(Circle == "BELONIA", "SOUTH TRIPURA",
               ifelse(Circle == "CIRCLE 2 (W. TRIPURA)", "WEST TRIPURA",
               ifelse(Circle == "SEPAHIJALA (BISHRAMGANJ)", "SIPAHIJALA",
               ifelse(Circle == "DHARMANAGAR", "NORTH TRIPURA",
               ifelse(Circle == "UNOKOTI", "UNOKOTI",
               ifelse(Circle == "KHOWAI", "KHOWAI", NA
))))))))))


# Merge data with shape file
df <- left_join(tripura_shp, merged_circle_all,
          by = c("district" = "District"))

################### Create Theme ##################

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#666666"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F1EAEA",
                                     color = NA),
      panel.background = element_rect(fill = "#F1EAEA",
                                      color = NA),
      legend.background = element_rect(fill = "#F1EAEA",
                                       color = NA),
      # borders and margins (I have commented these as these generate an error with the plotly, else it works perfect)
      # plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      # panel.border = element_blank(),
      # panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "#666666"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#666666"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#666666",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

#################### Viz 1: Total Complaints across districts ####################  

# Create data frame for viz
df1_all <- df |>                
  group_by(district) |> 
  summarise(n = n()) |>
  select(district, n, geometry) 

# Plot (all years)
ggplot(df1_all)+
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = district), size = 3, nudge_x = -0.014) +
  geom_sf_label(aes(label = comma(n)), size = 3, nudge_y = -0.053) +
  scale_fill_gradient(low = "grey" , high = "#e37400", labels = comma) +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 6, face= "italic")
  ) +
  labs(caption = "Data shown contains TSECL, FEDCO & SAI complaints during 2020-22") +
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Total Complaints",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


# Plot (2020)
df1_2020 <- df |>                
  group_by(Year = year(date_time), district) |> 
  summarise(n = n()) |>
  select(district, n, Year, geometry) |> 
  filter(Year == 2020) 

ggplot(df1_2020)+
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = district), size = 3.8, nudge_x = -0.03) +
  geom_sf_label(aes(label = comma(n)), size = 3.8, nudge_x = -0.03, 
                nudge_y = -0.053) +
  scale_fill_gradient(low = "grey" , high = "#e37400", labels = comma) +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face= "italic", size = 7)
  ) +
  labs(caption = 
         "Data shown contains complaints in 2020. TSECL data is from August to December,
       FEDCO data is from September to December & SAI data is from September to December.
       0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division, Dhalai Circle 
       have been misclassified. They are counted under Durgachowmohani_R, Agartala 1 Division,
       Circle 1 (W. Tripura).") +  
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Total Complaints",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


# Plot (2021)
df1_2021 <- df |>                
  group_by(Year = year(date_time), district) |> 
  summarise(n = n()) |>
  select(district, n, Year, geometry) |> 
  filter(Year == 2021) 

ggplot(df1_2021)+
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = district), size = 3.8, nudge_x = -0.03) +
  geom_sf_label(aes(label = comma(n)), size = 3.8, nudge_x = -0.03, 
                nudge_y = -0.053) +
  scale_fill_gradient(low = "grey" , high = "#e37400", labels = comma) +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust=0.5, face= "italic", size = 7)
  ) +
  labs(caption = 
         "Data shown contains TSECL, FEDCO & SAI complaints from January 2021 to December 2021.
       0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division, Dhalai Circle 
       have been misclassified. They are counted under Durgachowmohani_R, Agartala 1 Division,
       Circle 1 (W. Tripura).") +  
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Total Complaints",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


# Plot (2022)
df1_2022 <- df |>                
  group_by(Year = year(date_time), district) |> 
  summarise(n = n()) |>
  select(district, n, Year, geometry) |> 
  filter(Year == 2022) 

ggplot(df1_2022)+
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = district), size = 3.8, nudge_x = -0.03) +
  geom_sf_label(aes(label = comma(n)), size = 3.8, nudge_x = -0.03, 
                nudge_y = -0.053) +
  scale_fill_gradient(low = "grey" , high = "#e37400", labels = comma) +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust=0.5, face= "italic", size = 7)
  ) +
  labs(caption = 
         "Data shown contains TSECL, FEDCO & SAI complaints from January 2022 to October 2022.
       0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division, Dhalai Circle 
       have been misclassified. They are counted under Durgachowmohani_R, Agartala 1 Division,
       Circle 1 (W. Tripura).") +  
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Total Complaints",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


----------------------
  # Create quantiles for total complaints
  quantiles <- df1$n %>% 
  quantile(probs = seq (0,1, length.out = 4)) %>% 
  as.vector()

# Create labels 
labels <- imap_chr(quantiles, function(., idx){
  return(if(quantiles[idx] < 1000){
    paste0(quantiles[idx],
           "-", 
           quantiles[idx+1])
  } else{
    paste0(round(quantiles[idx]/1000,0),
           "K",
           "-", 
           round(quantiles[idx+1]/1000,0), "K")
  })
})

labels <- labels[c(1,2,3)]  # remove irrelevant labels


# Add quantiles in data frame
df1 <- df1 %>%  mutate(mean_quantiles = 
                         cut(n, 
                             breaks = quantiles,
                             labels = labels,
                             include.lowest = T))


# Plot
ggplot(df1)+
  geom_sf(aes(fill = mean_quantiles), colour = "white", size = 0.1)+
  scale_fill_viridis(
    option = "magma",
    "Complaints across districts",
    alpha = 0.8,
    begin = 0.1,
    end = 0.9,
    discrete = T,
    direction = 1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T
    )
  )+
  geom_sf_label(aes(label = district),size = 3, label.size = 0.1) +
  labs(
    x = NULL,
    y = NULL,
    title = "TSECL: Total Complaints",
    caption = "caption to be here"
  )+
  theme_map()

  
-----------------------
ggplot(df1)+
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = district), clip = "off", size = 3) +
  scale_fill_gradient(low = "#9ba746" ,
                      high = "#e37400") +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  ) +
  
  guides(fill = guide_colorbar(
    direction = "horizontal",
    #barwidth=10,label.position="bottom"
    
  ))
  
  
  guides(fill = guide_legend(
    title.position = 'top', 
    #barwidth = 2, 
    #barheight = 2,
    title = "Total Number of Complaints",
    label.position = "bottom", 
    #label.hjust = 2
    
    ))

################## Viz 2: Complaints density across districts ##################

# Total consumers
load("data-preped/merged_consumption.Rda")
#consumption$Account_No <- as.character(consumption$Account_No)

total_consumer_tsecl <- merged_consumption |>     #  tsecl consumers per subdivision
  group_by(Subdivision) |>
  distinct(Account_No) |> 
  count()

total_consumer_fedco <- merged_fedco |>   # fedco consumers per subdivision
  group_by(Subdivision) |>
  filter(!is.na(Subdivision)) |>     # Remove 74 NAs
  distinct(Caller_number) |> 
  count()
  
total_consumer_sai <- tibble(       # imputing circle average consumer for sai subdivisions
  Subdivision = c("KAILASHAHAR 1", "KAILASHAHAR 2"), n = 13895) 

total_consumer_all <- rbind(total_consumer_tsecl,total_consumer_fedco,total_consumer_sai)
total_consumer_all$Subdivision <- toupper(total_consumer_all$Subdivision)

# Add district column to total consumer 
subdivision_mastersheet <- read_csv("OtherFiles/subdivison_mastersheet.csv")

total_consumer_new <- left_join(total_consumer_all, subdivision_mastersheet,
          by = c("Subdivision" = "Subdivision"))

total_consumer_new <- total_consumer_new |> 
  mutate(District = 
      ifelse(Circle == "GOMATI", "GOMATI",
      ifelse(Circle == "DHALAI", "DHALAI",
      ifelse(Circle == "CIRCLE 1 (W. TRIPURA)", "WEST TRIPURA",
      ifelse(Circle == "BELONIA", "SOUTH TRIPURA",
      ifelse(Circle == "CIRCLE 2 (W. TRIPURA)", "WEST TRIPURA",
      ifelse(Circle == "SEPAHIJALA (BISHRAMGANJ)", "SIPAHIJALA",
      ifelse(Circle == "DHARMANAGAR", "NORTH TRIPURA",
      ifelse(Circle == "UNOKOTI", "UNOKOTI",
      ifelse(Circle == "KHOWAI", "KHOWAI", NA
))))))))))


total_consumer_district <- total_consumer_new |>  # Group total consumers by district
  group_by(District) |> 
  summarise(Total_Consumers = sum(n)) |>
  select(District, Total_Consumers)

# Create data frame for viz
df2 <- df |>                                # Total complaints per district
  group_by(district, year = year(date_time)) |> 
  summarise(Total_Complaints = n()) |>
  select(year, district, Total_Complaints, geometry)

# Merge df2 & total consumer district
df2 <- left_join(df2, total_consumer_district,     
                 by = c("district" = "District"))
  
# Add density column
df2 <- df2 |>                                
  mutate(Density = round(1000*Total_Complaints/Total_Consumers, 0))

# Viz density (combined) (wrong)
ggplot(df2)+
  geom_sf(aes(fill = Density)) +
  geom_sf_label(aes(label = district), nudge_x = -0.015, size = 3) +
  geom_sf_label(aes(label = Density), clip = "off", size = 3, nudge_y = -0.05) +
  scale_fill_gradient(low = "grey" ,   #9ba746
                      high = "#e37400") +
  labs(caption = "Data shown contains TSECL, FEDCO & SAI complaints from Aug 2020 to Nov 2022.") +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 3.5, face= "italic")
  ) +
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Complaints per 1000 Consumers",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


# Viz density (2020)
ggplot(df2 |> filter(year == 2020))+
  geom_sf(aes(fill = Density)) +
  geom_sf_label(aes(label = district), nudge_x = -0.015, size = 3) +
  geom_sf_label(aes(label = Density), size = 3, nudge_y = -0.05) +
  scale_fill_gradient(low = "grey" , high = "#e37400") +
  labs(caption = "Data shown contains complaints in 2020. TSECL data is from August to December, 
       FEDCO data is from September to December & SAI data is from September to December.
       0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division, Dhalai Circle
       have been misclassified. They are counted under Durgachowmohani_R, Agartala 1 Division,
       Circle 1 (W. Tripura).") +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face= "italic")
  ) +
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Complaints per 1000 Consumers",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))


# Viz density (2021)
ggplot(df2 |> filter(year == 2021))+
  geom_sf(aes(fill = Density)) +
  geom_sf_label(aes(label = district), nudge_x = -0.015, size = 3) +
  geom_sf_label(aes(label = Density), size = 3, nudge_y = -0.05) +
  scale_fill_gradient(low = "grey" , high = "#e37400") +
  labs(caption = "Data shown contains TSECL, FEDCO & SAI complaints from January 2021 to
       December 2021. 0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division,
       Dhalai Circle have been misclassified. They are counted under Durgachowmohani_R, 
       Agartala 1 Division, Circle 1 (W. Tripura).") +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face= "italic")
  ) +
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Complaints per 1000 Consumers",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))

# Viz density (2022)
ggplot(df2 |> filter(year == 2022))+
  geom_sf(aes(fill = Density)) +
  geom_sf_label(aes(label = district), nudge_x = -0.015, size = 3) +
  geom_sf_label(aes(label = Density), size = 3, nudge_y = -0.05) +
  scale_fill_gradient(low = "grey" , high = "#e37400") +
  labs(caption = "Data shown contains TSECL, FEDCO & SAI complaints from January 2022 
       to October 2022. 0.9% (48) of the complaints under Durgachowmohani, Kamalpur Division, 
       Dhalai Circle have been misclassified. They are counted under Durgachowmohani_R, 
       Agartala 1 Division, Circle 1 (W. Tripura).") +
  theme(
    panel.background = element_rect(
      fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face= "italic")
  ) +
  guides(fill = guide_colorbar(
    title.position = 'top', 
    title.hjust = 0.5, 
    title = "Complaints per 1000 Consumers",
    direction = "horizontal",
    barwidth=20, barheight=1, label.position="bottom"))

####################  End ####################

merged_tsecl |> filter(is.na(SubdivisionName_clean)) |>  which()
which(is.na(merged_tsecl$SubdivisionName_clean))
blanks <- merged_tsecl[c(174439, 174441, 174442, 174445, 174446),]

merged_sai |> group_by(Year = year(date_time), month(date_time)) |> count() |> view()
