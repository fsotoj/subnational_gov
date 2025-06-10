

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Elections"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Map", tabName = "map_tab", icon = icon("map")),
      menuItem("Codebook", tabName = "codebook", icon = icon("book-open")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    selectInput("country_sel", "Country", choices = c("Select a country",unique(data$country_name)), selected = "ARGENTINA"),
    selectInput("year_sel", "Year", choices = c("Select a year",sort(unique(data$year),decreasing = T)), selected = "2024"),
    selectInput("var_sel", "Variable", choices = c("Select a variable",unique(dict$pretty_name)), selected = "Subnat. Leader Sex"),
    box(title = "Variable description", solidHeader = TRUE, width = 12, textOutput("var_description"), collapsible = T, collapsed = T),
    actionButton("apply_filters", "Apply Filters", icon = icon("arrows-rotate")),
    br(),
    downloadButton("download_data", "Download complete data"),
    downloadButton("download_geom", "Download geometries")
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
        shinybusy::use_busy_spinner(spin = "fading-circle", color = "#112446"),
        mapModuleUI("map1"),
        absolutePanel(
          top = 100, right = 12, width = 320,
          draggable = T,
          box(
            title = "National Leader Summary",
            #status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            
            uiOutput("leader_summary")
          )
        )
        
      ),

      tabItem(tabName = "codebook", 
              uiOutput("pdf_visor")),
      
      tabItem(
        tabName = "about",
        box(
          title = "About This Application",
          solidHeader = TRUE,
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          
          p("This interactive dashboard provides a comprehensive overview of subnational political leadership across Argentina, Brazil and Mexico. 
    It allows users to explore electoral outcomes, leadership profiles, ideological alignments, and regional trends using dynamic visualizations and maps."),
          
          p(HTML("The application integrates the Database of Subnational Federalism in Latin America <a href='https://www.agustinagiraudy.com' target='_blank'>(Giraudy, A. 2025)</a> with the geometries of the federal states and provinces from 
         <a href='https://www.simplemaps.com' target='_blank'>www.simplemaps.com</a>. 
         It presents them in an accessible format for researchers, journalists, policymakers, and the general public interested in political dynamics at the subnational level.")),
          
          p("Use the sidebar menu to navigate between countries, years and variables.")
        )
      )
    ),
    skin = "blue"
    
  )
)
