library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(sf)
library(dplyr)
library(tidyr)
library(DT)
#library(reactable)
library(leaflet)
library(classInt)
library(shinyWidgets)
library(shinyjs)
library(mapview)
library(webshot)
library(jsonlite)
library(tibble)
library(purrr)
library(echarts4r)
library(highcharter)
library(htmltools)
library(httr2)
library(promises)
library(future)
library(later)

source("map_module.R")
#source("line_module.R")
source("hc_line_module.R")
source("get_jstree_data.R")
source("get_jstree_data_vars.R")
#source("tableModule.R")
source("camera_module.R")
source("about_spp_module.R")
source("spp_list_module.R")
#source("dataBaseInfo.R")


data_info <- read.xlsx("data/dict_new.xlsx") %>% 
  select(Category= category, Variable = variable, Description = description_for_ui)
dict <- read.xlsx("data/dict_new.xlsx") %>% filter(scope == "subnational")

sled_names <- dict %>% 
  filter(dataset == "Legislative Elections", viewable_map == 1) %>% pull(variable)


NED <- read.xlsx("data/NED (v.0.1).xlsx") %>%
  mutate(ideo_party_nat_exe = as.double(ideo_party_nat_exe),
         start_date_head_nat_exe = as.Date(start_date_head_nat_exe - 2, origin = "1900-01-01"),
         end_date_head_nat_exe   = as.Date(end_date_head_nat_exe - 2, origin = "1900-01-01"))

SEED <- read.xlsx("data/SEED SHINY (v.0.1).xlsx")
SED <- read.xlsx("data/SED (v.0.1).xlsx")
SLED <- read.xlsx("data/SLED (v.0.1).xlsx")
CFTDFLD <- read.xlsx("data/CFTDFLD (v.0.1).xlsx")
SDI <- read.xlsx("data/SDI (v.1).xlsx") %>% 
  distinct()

cols_to_fill <- c("chamber_sub_leg",as.vector(outer(setdiff(sled_names,c("chamber_sub_leg","concurrent_election_with_nat_sub_leg")), c("_1","_2"), paste0)))

SLED_wide <- SLED %>%
  select(country_state_code, year, chamber_election_sub_leg, all_of(sled_names)) %>%
  distinct() %>%
  pivot_wider(
    id_cols     = c(country_state_code, year,chamber_sub_leg),
    names_from  = chamber_election_sub_leg,
    values_from = c(all_of(sled_names),-chamber_sub_leg),
    names_glue  = "{.value}_{chamber_election_sub_leg}",
    #values_fn   = ~ dplyr::last(., na_rm = TRUE),   # collapse duplicates
    values_fill = NA                                 # fill absent cells with NA
  ) %>%
  arrange(country_state_code, year)%>%
  # carry-forward within each chamber separately
  group_by(country_state_code) %>%
  complete(year = seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE), 1)) %>%
  arrange(country_state_code, year) %>%
  mutate(across(
    starts_with("concurrent_election_with_nat_sub_leg_"),
    ~ replace(., is.na(.), 0)
  )) %>%
  fill(all_of(cols_to_fill), .direction = "down") %>%
  ungroup()


data <- left_join(NED,SED,c("country_name","country_code","year")) %>% 
  left_join(.,SEED,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
  left_join(.,SLED_wide,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
  left_join(.,SDI,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))



geom <- st_read("data/geom_simple_maps.geojson")


party_colors <- read.xlsx("data/party_colors.xlsx")
jstree_json_data <- get_jstree_data(data)
jstree_json_vars <- get_jstree_data_vars(dict %>% filter(viewable_map == 1, variable != "chamber_sub_leg"))
jstree_json_vars_graph <- get_jstree_data_vars(dict %>% filter(viewable_graph == 1, variable != "chamber_sub_leg"), FALSE)




# country_bboxes <- list(
#   ARGENTINA = list(lng1 = -73.5, lat1 = -59, lng2 = -56, lat2 = -21.8),
#   BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -44.5, lat2 = 5.3),
#   MEXICO    = list(lng1 = -118.5, lat1 = 14.5, lng2 = -86.7, lat2 = 32.7)
# )



country_bboxes <- list(
  ARGENTINA = list(
    large  = list(
      lng1 = -73.5, lat1 = -60,
      lng2 = -56,   lat2 = -22.8
    ),
    medium = list(
      lng1 = -72,   lat1 = -57,
      lng2 = -57,   lat2 = -22
    ),
    small  = list(                      # doubled width
      lng1 = -77,   lat1 = -54.5,
      lng2 = -53,   lat2 = -25.5
    )
  ),
  
  BRAZIL = list(
    large  = list(
      lng1 = -73.9, lat1 = -34.7,
      lng2 = -44.5, lat2 = 4.3
    ),
    medium = list(
      lng1 = -72,   lat1 = -31,
      lng2 = -46,   lat2 = 5
    ),
    small  = list(                      # moved another ¼ height south
      lng1 = -68,   lat1 = -35.8,
      lng2 = -44,   lat2 = -4.6
    )
  ),
  
  MEXICO = list(
    large = list(
      lng1 = -118.5, lat1 = 12.5,
      lng2 = -86.7,  lat2 = 30.7
    ),
    medium = list(
      lng1 = -116,   lat1 = 16.5,
      lng2 = -89,    lat2 = 30.5
    ),
    small = list(
      lng1 = -114,   lat1 = 16.75,
      lng2 = -90.5,  lat2 = 27.75
    )
  )
)


