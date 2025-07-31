

ui <- dashboardPage(
  dashboardHeader(title = "Subnational Politics Project",titleWidth = 280),
  dashboardSidebar(
    useShinyjs(),
    tagList(
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
      )),
      
      div(
        style = "
        position: absolute;
        bottom: 10px;
        left: 15px;
        right: 15px;
        font-size: 0.8em;
        color: #aaa;
      ",
        HTML("This tool was developed by <strong>Felipe Soto Jorquera</strong> as part of the <em>Subnational Politics Project (Agustina Giraudy et al., 2025)</em>.")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(id = "theme-css", rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
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
    # ui.R (Add this script, or combine it with your existing JS script)
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
            uiOutput("year_selector")
          ),
          div(
            style = "position: absolute; bottom: 30px; left: 30%; margin-left: 71px; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
            hidden(actionButton("captureMapBtn", "Download Map", class = "download_map_btn"))
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
                 ),
                 checkboxInput("force_y0", "Force Y-axis to start at 0", value = FALSE)
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
          title = "About the Subnational Politics Project (SPP)",
          solidHeader = TRUE,
          status = "info",
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          HTML("
        <p style='text-align:justify;'>
        The Subnational Politics Project (SPP) is part of a broader research project designed to compile, generate, and disseminate systematic, transparent, and publicly accessible data on subnational political institutions, subnational political processes, and subnational electoral outcomes in Latin America.
        </p>
        <p style='text-align:justify;'>
        The primary objective of the project is to create a centralized and standardized data infrastructure that facilitates both in-depth within-country analyses and cross-national comparative research on subnational political dynamics.
        </p>
        <p style='text-align:justify;'>
        By providing longitudinal and spatially disaggregated data, the SPP seeks to support empirical scholarship on a wide range of topics, including federalism, decentralization, party competition, electoral accountability, and territorial governance.
        </p>
      ")
        )
      )
    ),
    skin = "blue"
    
  )
)
