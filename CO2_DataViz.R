######################################################

# Author: Susan Paykin
# Date created: 26 April 2020
# Date last modified: 26 April 2020

# Step 1: Test code on Uganda sample data
# Step 2: Load, aggregate, and prepare all Afrobarometer country data
# Step 3: Load, aggregate, and prepare global trends data
# Step 4: Plot data

######################################################

# Load libraries
library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)
library(lubridate)

setwd("~/Desktop/Spring 2020/Policy Lab")

##################################
########## TEST: UGANDA ##########
##################################

#Load data
uganda_co2 <- read.csv("Data/Data Viz Assignment/5ea35212c15e005bdf603974/5ea35212c15e005bdf603974_results.csv")

# Test - Aggregate Uganda data
UGA_national <-
  uganda_co2 %>%
  filter(iso == "UGA") %>%
  summarize(country = "UGA",
            "2015" = mean(oco2_xco2_yearly.2015.mean),
            "2016" = mean(oco2_xco2_yearly.2016.mean),
            "2017" = mean(oco2_xco2_yearly.2017.mean),
            "2018" = mean(oco2_xco2_yearly.2018.mean)) %>%
  gather("year", "mean", -country)

# Plot Uganda data
ggplot(UGA_national, aes(x = year, y = mean)) +
  geom_point() 

####################################################
########## AFROBAROMETER COUNTRY CO2 DATA ##########
####################################################

# Read in all Afrobarometer country data
files <- list.files(path = "Data/Data Viz Assignment/subnational_data/", pattern = "*.csv", full.names = T)
co2 <- sapply(files, read_csv, simplify = FALSE) %>% bind_rows(.id = NULL)

# Aggregate mean CO2 concentrations by year, country
national <-
  co2 %>%
  group_by(iso) %>%
  summarize("2015" = mean(oco2_xco2_yearly.2015.mean),
            "2016" = mean(oco2_xco2_yearly.2016.mean),
            "2017" = mean(oco2_xco2_yearly.2017.mean),
            "2018" = mean(oco2_xco2_yearly.2018.mean)) %>%
  gather("year", "mean", -iso)

#########################################
######### GLOBAL CO2 DATA ###############
#########################################

# Load global CO2 data
library("jsonlite")

json_file <- 'https://datahub.io/core/co2-ppm-daily/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

path_to_file = json_data$resources$path[i]
data <- read.csv(url(path_to_file))

# Aggregate mean CO2 concentratinos by year
global <- 
  data %>% 
  mutate(year = year(date)) %>%
  group_by(year) %>%
  filter(year == c("2015", "2016", "2017", "2018")) %>%
  summarise(mean = mean(value))

write.csv(global, "global.csv")

###############################
########## Plot Data ##########
###############################

# Set colors for all countries
colourCount = length(unique(national$iso))
getPalette = colorRampPalette(brewer.pal(9, "BrBG"))

# Set position
pd <- position_dodge(0.75)

# Scatterplot - data as points
scatter <- ggplot(national, aes(x = year, y = mean, color = iso, group = iso)) +
  geom_point(aes(color = iso), position = pd) +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("CO2 (ppm(") +
  ggtitle("Average CO2 Concentration (ppm), Afrobarometer Countries") +
  labs(color = "Country") +
  theme_minimal()

scatter

# Line graph - including points
line_points <- 
  ggplot(national, aes(x = year, y = mean, color = iso, group = iso)) +
  geom_line(aes(color = iso), position = pd, linetype = "dashed", alpha = 0.5) +
  geom_point(aes(color = iso), position = pd) +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("CO2") +
  ggtitle("Average CO2 Concentration (PPM), Afrobarometer Countries") +
  labs(color = "Country") +
  theme_minimal()

line_points
  

# Line graph 2 - simple, no points
line_simple <- 
  ggplot() +
  geom_line(data = national, aes(x = year, y = mean, color = iso, group = iso),
            position = pd) +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.position = "bottom") +
  xlab("Year") + 
  ylab("CO2") +
  #ylim(380, 415) +
  ggtitle("Average CO2 Concentration (PPM), Afrobarometer Countries") +
  theme_minimal()
  
line_simple

# Line graph 3 with global trend line

# Change global year var to character
global$year <- as.character(global$year)

line_global <- 
  ggplot() +
  geom_line(data = national, aes(x = year, y = mean, color = iso, group = iso), size = 0.5, position = pd) +
  geom_line(data = global, aes(x = year, y = mean, linetype = "Global mean"), group = 1, size = 1.0, position = pd) +
  scale_fill_manual(values = getPalette(colourCount)) +
  xlab("Year") + 
  ylab("CO2 (ppm)") +
  ggtitle("Average CO2 Concentration (ppm), Afrobarometer Countries") +
  theme_minimal() +
  labs(color = "Country") +
  labs(linetype = "")

line_global

# # Line graph 4 starting at 350ppm
# line_350 <- 
#   ggplot() +
#   geom_line(data = national, aes(x = year, y = mean, color = iso, group = iso), size = 0.5, position = pd) +
#   geom_line(data = global, aes(x = year, y = mean, linetype = "Global Mean"), 
#             group = 1, size = 1.0, position = pd) +
#   scale_fill_manual(values = getPalette(colourCount)) +
#   theme(legend.position = "bottom") +
#   xlab("Year") + 
#   ylab("CO2 (ppm)") +
#   ggtitle("Average CO2 Concentration (ppm), Afrobarometer Countries") +
#   theme_minimal() +
#   ylim(350, 410) +
#   labs(color = "Country") +
#   labs(linetype = "")
# 
# line_350


  