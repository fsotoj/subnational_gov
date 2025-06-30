

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Elections"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Mapping tool", tabName = "map_tab", icon = icon("map")),
      menuItem("Graphing tool", tabName = "graph_tab", icon = icon("chart-line")),
      menuItem("Codebook", tabName = "codebook", icon = icon("book-open")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    uiOutput("country_selector"),
    
    uiOutput("state_selector"),
    
    uiOutput("variable_selector"),
    
    
    
    br(),
    
    #uiOutput("year_selector"),
    
    
    
    
    #actionButton("apply_filters", "Apply Filters", icon = icon("arrows-rotate")),
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
        tabName = "map_tab", # QUE PASA ACAAAAAA
        tagList(
          mapModuleUI("map1"),
          
          absolutePanel(
            top = 100, right = 12, 
            width = 320,
            draggable = TRUE,
            box(
              title = "Variable description", 
              solidHeader = TRUE,
              collapsible = TRUE, 
              collapsed = FALSE,
              width = NULL,
              textOutput("var_description")
            ),
            box(
              title = "National Leader Summary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              uiOutput("leader_summary")
            )
          ),
          
          # Custom year selector placed at the bottom of the tab
          div(
            style = "position: absolute; bottom: 20px; left: 30%; right: 30%; z-index: 1000;",
            uiOutput("year_selector")
          )
          
        )
      ),
      
      tabItem(tabName = "graph_tab", 
              linePlotModuleUI("line_plot1")
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
