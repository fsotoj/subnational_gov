

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
    
    # Carga del CSS personalizado
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # ),
    
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
                   DT::DTOutput('table',width = "100%")
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
      tabItem(tabName = "data", DT::DTOutput("table_info", height = "100%")),
      
      tabItem(tabName = "about", box(fluidRow(textOutput("ajajjajajaja"))))
    )
  )
)
