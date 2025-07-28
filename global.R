library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(sf)
library(dplyr)
library(DT)
library(leaflet)
library(classInt)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(mapview)
library(webshot)
#library(shinyWidgets)




source("map_module.R")
source("line_module.R")
# source("ggMapModule.R")
#source("state_selector_module.R")

data <- read.xlsx("data/complete_database_edit.xlsx")
geom <- st_read("data/geom_simple_maps.geojson")

data_info <- read.xlsx("data/dictionary.xlsx") %>% 
  filter(category %in% c("Identification", "Electoral")) %>% 
  select(Category= category, Variable = variable, Description = description)
dict <- read.xlsx("data/dictionary.xlsx") %>% filter(scope == "subnational")
party_colors <- read.xlsx("data/party_colors.xlsx")

country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -59, lng2 = -56, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -44.5, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 14.5, lng2 = -86.7, lat2 = 32.7),
  `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)