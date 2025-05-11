library(shiny)
library(shinydashboard)
library(plotly)
source("map_module.R")
source("timeline_module.R")

# Leer datos
library(openxlsx)
library(sf)
data <- read.xlsx("data/complete_database_edit.xlsx")
geom <- st_read("data/geom_paises.gpkg")
dict <- read.xlsx("data/dictionary.xlsx") %>% filter(viewable == 1, scope == "subnational")

country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -55.1, lng2 = -53.6, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -34.8, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 14.5, lng2 = -86.7, lat2 = 32.7),
  `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Elections"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Map", tabName = "map_tab", icon = icon("map")),
      menuItem("Timeline", tabName = "timeline", icon = icon("clock")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    selectInput("country_sel", "Country", choices = c(unique(data$country_name), "Select a country"), selected = "Select a country"),
    uiOutput("conditional_year_ui"),
    selectInput("var_sel", "Variable", choices = c(unique(dict$variable), "Select a variable"), selected = "Select a variable"),
    uiOutput("conditional_state_ui"),
    actionButton("apply_filters", "Apply Filters", icon = icon("arrows-rotate"))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Electoral Map", status = "primary", solidHeader = TRUE, width = 8,
            mapModuleUI("map1")
          ),
          column(width = 4,
                 box(
                   title = "National values", status = "primary", solidHeader = TRUE, width = 12,
                   DT::DTOutput('table')
                 ),
                 valueBoxOutput("last_elect_nat_box",width = 12)
                 )
        ),
        fluidRow(
          box(title = "Variable description", status = "info", solidHeader = TRUE, width = 8, textOutput("var_description"))
        )
      ),
      
      
      tabItem(
        tabName = "timeline",
        fluidRow(
          box(title = "Governor Timeline", status = "primary", solidHeader = TRUE, width = 8, vistime_module_ui("timeline1")),
          box(title = "Presidential Timeline", status = "primary", solidHeader = TRUE, width = 8, vistime_module_ui("timeline2")),
         )
      ),
      
      tabItem(tabName = "about", box(fluidRow(textOutput("ajajjajajaja"))))
    )
  )
)
