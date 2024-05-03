####################  Load libraries ####################
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(wesanderson)

####################  Load data ####################

merged_tsecl <- read_csv("data-preped/merged_tsecl.csv")
merged_fedco <- read_csv("data-preped/merged_fedco.csv")
merged_sai <- read_csv("data-preped/merged_sai.csv")

####################  Clean data ####################

# Combine tsecl fedco sai
merged_tsecl_circle <- merged_tsecl[,c("SubdivisionName_clean", "Division", "Circle")]

merged_fedco <- merged_fedco |> 
  mutate(Circle = 
           ifelse(Division == "Ambassa", "DHALAI",
                  ifelse(Division == "Manu", "DHALAI",
                         ifelse(Division == "Mohanpur", "CIRCLE 2 (W. TRIPURA)",
                                ifelse(Division == "Sabroom", "BELONIA", NA)))))

merged_fedco_circle <- merged_fedco[, c(25, 24,39)] 

merged_sai_circle <- merged_sai[, c(11, 12)] |> 
  mutate(Subdivision = "KAILASHAHAR 1 & 2") |> 
  select(Subdivision, Division, Circle)

colnames(merged_tsecl_circle)<- c("Subdivision","Division", "Circle")
colnames(merged_fedco_circle)<- c("Subdivision","Division", "Circle")

# remove NAs
merged_fedco_circle  <- merged_fedco_circle |> 
  filter(!is.na(Circle))

merged_tsecl_circle <- merged_tsecl_circle |> 
  filter(!is.na(Circle))

# Combine tsecl, fedco, sai
merged_circle_all <- rbind(merged_tsecl_circle, merged_fedco_circle, merged_sai_circle) 

# Combine Kailashar 1 & 2 subdivisions 
merged_circle_all$Subdivision[merged_circle_all$Subdivision == "KAILASHAHAR 1"] <- "KAILASHAHAR 1 & 2"
merged_circle_all$Subdivision[merged_circle_all$Subdivision == "KAILASHAHAR 2"] <- "KAILASHAHAR 1 & 2"

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

# All caps
merged_circle_all$Subdivision <- toupper(merged_circle_all$Subdivision)
merged_circle_all$Division <- toupper(merged_circle_all$Division)
merged_circle_all$Circle <- toupper(merged_circle_all$Circle)
merged_circle_all$District <- toupper(merged_circle_all$District)

################### Create Theme ##################

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


# Filter Top 3 Districts
df_3 <- merged_circle_all |> 
  filter(District %in% c("WEST TRIPURA", "DHALAI", "UNOKOTI"))

########## Division graphics ##########

# West Tripura
wt_div <- df_3 |> 
  filter(District == "WEST TRIPURA") |> #count() 
  group_by(Division) |> 
  summarise(N = n()) |> 
  mutate(prop = N/183175,
         prop = round(prop, 2))

ggplot(wt_div, aes(y = reorder(Division,N), x = prop)) +
  geom_col(fill = "#e09500", width = 0.8) +
  labs(title = "Distribution of Complaints: West Tripura", 
       subtitle = "N = 183,175", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Division") +
  geom_text(
    aes(label = percent(prop)),
    hjust = 1.1,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent()) +
  theme_tsecl() +
  coord_cartesian(clip = 'off')

# Dhalai
dh_div <- df_3 |> 
  filter(District == "DHALAI") |> #count() 
  group_by(Division) |> 
  summarise(N = n()) |> 
  mutate(prop = N/11908,
         prop = round(prop, 2))

ggplot(dh_div, aes(y = reorder(Division,N), x = prop)) +
  geom_col(fill = "#e09500", width = 0.8) +
  labs(title = "Distribution of Complaints: Dhalai", 
       subtitle = "N = 11,908", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Division") +
  geom_text(
    aes(label = percent(prop)),
    hjust = 1.1,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent()) +
  theme_tsecl() +
  coord_cartesian(clip = 'off')


# Unokoti
un_div <- df_3 |> 
  filter(District == "UNOKOTI") |> #count() 
  group_by(Division) |> 
  summarise(N = n()) |> 
  mutate(prop = N/23155,
         prop = round(prop, 2))

ggplot(un_div, aes(y = reorder(Division,N), x = prop)) +
  geom_col(fill = "#e09500", width = 0.8) +
  labs(title = "Distribution of Complaints: Unokoti", 
       subtitle = "N = 23,155", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Division") +
  geom_text(
    aes(label = percent(prop)),
    hjust = 1.1,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent()) +
  theme_tsecl() +
  coord_cartesian(clip = 'off')


########## Subdivision graphics ##########

# West Tripura
wt_sd <- df_3 |> 
  filter(District == "WEST TRIPURA") |> #count() 
  group_by(Subdivision, Division) |> 
  summarise(N = n()) |> #view()
  mutate(prop = N/183175,
         prop = round(prop, 2))

ggplot(wt_sd, aes(y = reorder(Subdivision,N), x = prop, fill = Division)) +
  geom_col(width = 0.8) +
  labs(title = "Distribution of Complaints: West Tripura", 
       subtitle = "N = 183,175", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Subdivision") +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    hjust = 1.1,
    vjust = 0.5,
    color = "white",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  theme_tsecl() +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values = wes_palette("Cavalcanti1"))

# Dhalai
dh_sd <- df_3 |> 
  filter(District == "DHALAI") |> #count() 
  group_by(Subdivision, Division) |> 
  summarise(N = n()) |> 
  mutate(prop = N/11908,
         prop = round(prop, 2))

ggplot(dh_sd, aes(y = reorder(Subdivision,N), x = prop, fill = Division)) +
  geom_col(width = 0.8) +
  labs(title = "Distribution of Complaints: Dhalai", 
       subtitle = "N = 11,908", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Subdivision") +
  geom_text(
    aes(label = percent(prop)),
    hjust = 1.1,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent()) +
  theme_tsecl() +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values = wes_palette("Chevalier1"))

# Unokoti
un_sd <- df_3 |> 
  filter(District == "UNOKOTI") |> #count() 
  group_by(Subdivision, Division) |> 
  summarise(N = n()) |> 
  mutate(prop = N/23155,
         prop = round(prop, 2))

ggplot(un_sd, aes(y = reorder(Subdivision,N), x = prop, fill = Division)) +
  geom_col(width = 0.8) +
  labs(title = "Distribution of Complaints: Unokoti", 
       subtitle = "N = 23,155", 
       caption = "Percents are rounded to nearest integer.", 
       x = "% of Complaints", y = "Subdivision") +
  geom_text(
    aes(label = percent(prop)),
    hjust = 1.1,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  geom_text(
    aes(label = scales::comma(N)),
    hjust = -0.15,
    vjust = 0.5,
    color = "#7c1419",
    size = 4
  ) +
  scale_x_continuous(labels = label_percent()) +
  theme_tsecl() +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values = wes_palette("Royal1"))

# Number of observation in Tsecl, Fedco and Sai by year
merged_sai |> group_by(year = year(date_time)) |> summarise(N=n())
merged_fedco |> group_by(year = year(Complaintlogtime_clean)) |> summarise(N=n())
merged_tsecl |> group_by(year = year(ComplaintDate_clean)) |> summarise(N=n())
