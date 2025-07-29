

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Politics Project",titleWidth = 280),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(id = "tabs",
                menuItem("Mapping tool", tabName = "map_tab", icon = icon("map")),
                menuItem("Graphing tool", tabName = "graph_tab", icon = icon("chart-line")),
                menuItem("Codebook", tabName = "codebook", icon = icon("book-open")),
                menuItem("Data", tabName = "data_tab", icon = icon("table")),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    uiOutput("country_selector"),  # default: visible
    hidden(selectInput("var_sel", "Variable", choices = NULL)),
    hidden(uiOutput("state_selector")),
    hidden(selectInput("var_sel2", "Variable", choices = NULL)),
    hidden(selectInput("country_sel2", "Select a country:", choices = c(unique(data$country_name)), selected = "ARGENTINA")),
    hidden(selectInput("state_sel2", "Select a state:", choices = NULL)),
    hidden(pickerInput(
      inputId = "columns_sel",
      label = "Select columns to show:",
      choices = colnames(data),
      selected = colnames(data),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        dropupAuto = FALSE,
        selectedTextFormat = "count > 3"
      )
    ))
    # ,
    # downloadButton("download_map", "Download Map PNG")
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
    tags$script(HTML("
    $(document).on('shiny:value', function(event) {
      const btn = $('.slider-animate-button');
      if (!btn.hasClass('customized')) {
        btn.addClass('customized');
        btn.append('<span class=\"btn-text\"> Play</span>');
  
        btn.on('click', function() {
          const textSpan = btn.find('.btn-text');
          const isPlaying = btn.hasClass('playing');
  
          if (isPlaying) {
            textSpan.text(' Play');
            btn.removeClass('playing');
          } else {
            textSpan.text(' Pause');
            btn.addClass('playing');
          }
        });
      }
    });
  ")),
    tabItems(
      tabItem(
        tabName = "map_tab", # QUE PASA ACAAAAAA
        tagList(
          mapModuleUI("map1"),
          
          absolutePanel(
            top = 90, right = 12, 
            width = 300,
            draggable = F,
            div(class = "small-text-box",
                box(
                  title = "Variable description", 
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = NULL,
                  textOutput("var_description_map")
                )
            ),
            div(class = "small-text-box",
                box(
                  title = "National Leader Summary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = NULL,
                  uiOutput("leader_summary")
                  )
          )),
          
          # Custom year selector placed at the bottom of the tab
          div(
            style = "position: absolute; bottom: 30px; left: 30%; right: 30%; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
            uiOutput("year_selector"),
            actionButton("captureMapBtn", "Download Map", class = "btn-primary")
          )
          
          
        )
      ),
      
      tabItem(
        tabName = "graph_tab",  # Este va directamente dentro de tabItem()
        fluidRow(
          column(9,
                 linePlotModuleUI("line_plot1")
          ),
          column(3,
                 box(
                   title = "Variable description", 
                   solidHeader = TRUE,
                   collapsible = TRUE, 
                   collapsed = FALSE,
                   width = NULL,
                   textOutput("var_description_graph")
                 )
          )
        )
      ),
      
        
        
      
      
      
      
      

      tabItem(tabName = "codebook", 
              uiOutput("pdf_visor")),
      
      tabItem(tabName = "data_tab",   
              fluidRow(DT::DTOutput("table_info")),
              br(),
              fluidRow(downloadButton("download_data", "Download complete data"),
                           downloadButton("download_geom", "Download complete geometries")
                
                
              )),
      
      
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
