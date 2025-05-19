

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Elections"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Map", tabName = "map_tab", icon = icon("map")),
      menuItem("Ideology", tabName = "timeline", icon = icon("clock")),
      menuItem("Votes", tabName = "votes_tab", icon = icon("clock")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    selectInput("country_sel", "Country", choices = c(unique(data$country_name), "Select a country"), selected = "Select a country"),
    uiOutput("conditional_year_ui"),
    uiOutput("conditional_select_var_ui"),
    uiOutput("conditional_state_ui"),
    uiOutput("conditional_apply_ui")
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Electoral Map", status = "primary", solidHeader = TRUE, width = 8,
            shinybusy::use_busy_spinner(spin = "fading-circle", color = "#112446"),
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
          box(title = "Governor Ideology Timeline", status = "primary", solidHeader = TRUE, 
              width = 8, vistime_module_ui("timeline1")),
          box(title = "Presidential Ideology Timeline", status = "primary", solidHeader = TRUE, 
              width = 8, vistime_module_ui("timeline2"))
         )
      ),
      
      tabItem(
        tabName = "votes_tab",
        fluidRow(
          box(title = "Governor Votes Timeline", status = "primary", solidHeader = TRUE, 
              width = 12, mod_vote_bubbles_ui("votes_module"))
          )
      ),
      
      tabItem(tabName = "data", DT::DTOutput("table_info", height = "100%")),
      
      tabItem(tabName = "about", box(fluidRow(textOutput("ajajjajajaja"))))
    )
  )
)
