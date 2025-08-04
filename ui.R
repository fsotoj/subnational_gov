

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
      hidden(uiOutput("db_selector")),
      uiOutput("country_selector"),  # default: visible
      hidden(selectInput("var_sel", "Variable", choices = NULL)),
      hidden(uiOutput("state_selector")),
      hidden(selectInput("var_sel2", "Variable", choices = NULL)),
      hidden(selectInput("country_sel2", "Select a country:", choices = c(unique(data$country_name)), selected = "ARGENTINA")),
      hidden(selectInput("state_sel2", "Select a state:", choices = NULL)))
  ),
  dashboardBody(
    tags$footer(
      style = "
      position: fixed;
      bottom: 0;
      left: 0;
      padding: 5px 15px;
      font-size: 11px;
      text-align: left;
      width: max-content;
      background: linear-gradient(to right, rgba(18, 18, 18, 0.85), rgba(18, 18, 18, 0));
      color: #ccc;
      z-index: 1050;
    ",
      HTML("Tool developed by <strong>Felipe Soto Jorquera</strong> for the <em>Subnational Politics Project</em> (Giraudy et al., 2025).")
    ),
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
            style = "position: absolute; bottom: 50%; left: 40%; z-index: 1000;",
            hidden(textOutput("no_data_message"))
          ),
          div(
            style = "position: absolute; bottom: 30px; left: 30%; right: 30%; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
            uiOutput("year_selector")
          ),
          div(
            style = "position: absolute; bottom: 30px; left: 30%; margin-left: 71px; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
            hidden(
              actionButton(
                "captureMapBtn",
                label = tagList(icon("camera"), "Screenshot"),
                class = "download_map_btn"
              )
            )
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
                   collapsible = FALSE, 
                   collapsed = FALSE,
                   width = NULL,
                   textOutput("var_description_graph")
                 ),
                 box(width = NULL,
                     height = NULL,
                   checkboxInput("force_y0", "Click for Y-axis start at 0", value = FALSE, ))
                 )
        )
      ),
      
        
        
      
      
      
      
      

      tabItem(tabName = "codebook", 
              uiOutput("pdf_visor")),
      
      tabItem(tabName = "data_tab",   
              fluidRow(column(9,DTOutput("table_info")),
                       column(3,
                              
                              #fluidRow(downloadButton("download_geom", "Download complete geometries")),
                              box(
                                title = "Current database", 
                                solidHeader = TRUE,
                                collapsible = FALSE, 
                                collapsed = FALSE,
                                width = NULL,
                                uiOutput("texto_db")
                              )
                              ,
                              selectInput("file_format", "Select download format:", choices = c("CSV" = "csv", "Excel" = "xlsx")),
                              
                              
                              downloadButton("download_data", "Download this database"))
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
    <hr style='border-color:#17a2b8;'/>
    <p style='font-size: 0.9em; color: #bbb;'>
      <strong>Suggested citation for data retrieved from this app:</strong><br/>
      Giraudy, Agustina, <em>et al.</em> (2025). <em>Subnational Politics Project Databases</em> (v0.1). Data accessed via the Subnational Politics Project web app developed by Felipe Soto. [DOI to be assigned]
    </p>
  ")
        )
        
      )
    ),
    skin = "blue"
    
  )
)
