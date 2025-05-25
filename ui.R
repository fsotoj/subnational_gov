

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Elections"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Map", tabName = "map_tab", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    selectInput("country_sel", "Country", choices = c("Select a country",unique(data$country_name)), selected = "ARGENTINA"),
    selectInput("year_sel", "Year", choices = c("Select a year",sort(unique(data$year),decreasing = T)), selected = "2024"),
    selectInput("var_sel", "Variable", choices = c("Select a variable",unique(dict$pretty_name)), selected = "Subnat. Leader Sex"),
    actionButton("apply_filters", "Apply Filters", icon = icon("arrows-rotate")),
    checkboxInput("dark_mode", "Dark", value = TRUE)
  ),
  dashboardBody(
    tags$head(
      tags$link(id = "theme-css", rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tags$script(HTML("
        Shiny.addCustomMessageHandler('update-theme', function(themeFile) {
          document.getElementById('theme-css').setAttribute('href', themeFile);
        });
      ")),
    tabItems(
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Electoral Map", solidHeader = TRUE, width = 8,
            shinybusy::use_busy_spinner(spin = "fading-circle", color = "#112446"),
            mapModuleUI("map1")
          ),
          column(width = 4,
                 box(
                   title = "National values",  solidHeader = TRUE, width = 12,
                   DT::DTOutput('table',width = "100%")
                 ),
                 valueBoxOutput("last_elect_nat_box",width = 12)
                 )
        ),
        fluidRow(
          box(title = "Variable description", solidHeader = TRUE, width = 8, textOutput("var_description"))
        )
      ),

      tabItem(tabName = "data", DT::DTOutput("table_info", height = "100%")),
      
      tabItem(tabName = "about", box(fluidRow(textOutput("ajajjajajaja"))))
    ),
    skin = "blue"
    
  )
)
