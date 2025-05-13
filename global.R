library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(sf)
library(dplyr)
library(DT)


source("map_module.R")
source("timeline_module.R")


data <- read.xlsx("data/complete_database_edit2.xlsx")
geom <- st_read("data/geom_paises_simplified.gpkg")

data_info <- read.xlsx("data/dictionary.xlsx") %>% 
  filter(category %in% c("Identification", "Electoral")) %>% 
  select(Category= category, Variable = variable, Description = description)
dict <- read.xlsx("data/dictionary.xlsx") %>% filter(viewable == 1, scope == "subnational")

country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -55.1, lng2 = -53.6, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -34.8, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 14.5, lng2 = -86.7, lat2 = 32.7),
  `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)