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
library(mapview)
library(webshot)
library(jsonlite)
#library(shinyWidgets)




source("map_module.R")
source("line_module.R")
source("get_jstree_data.R")


NED <- read.xlsx("data/NED (v.0.1).xlsx") %>%
  mutate(ideo_party_nat_exe = as.double(ideo_party_nat_exe))
SEED <- read.xlsx("data/SEED (v.0.1).xlsx")
SED <- read.xlsx("data/SED (v.0.1).xlsx")
SLED <- read.xlsx("data/SLED (v.0.1).xlsx")

data <- left_join(NED,SED,c("country_name","country_code","year")) %>% 
  left_join(.,SEED,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))

geom <- st_read("data/geom_simple_maps.geojson")

data_info <- read.xlsx("data/dict_new.xlsx") %>% 
  select(Category= category, Variable = variable, Description = description)
dict <- read.xlsx("data/dict_new.xlsx") %>% filter(scope == "subnational")
party_colors <- read.xlsx("data/party_colors.xlsx")
jstree_json_data <- get_jstree_data(data)



country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -59, lng2 = -56, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -44.5, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 14.5, lng2 = -86.7, lat2 = 32.7),
  `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)