

ui <- dashboardPage(  
  dashboardHeader(title = tags$div(
    class = "app-header-logo",
    tags$img(src = "spp_logo_v5.svg", height = "50px")
    ),
     titleWidth = 310
    ),
  title = "SPP-Subnational Politics Project",   # <-- this sets <title>
  dashboardSidebar(
    width = 310,
    useShinyjs(),
    tagList(
      sidebarMenu(id = "tabs",
                  menuItem("Mapping tool", tabName = "map_tab", icon = icon("map")),
                  menuItem("Graphing tool", tabName = "graph_tab", icon = icon("chart-line")),
                  menuItem("Camera Viz tool", tabName = "camera", icon = icon("landmark")),
                  menuItem("Codebook", tabName = "codebook", icon = icon("book-open")),
                  menuItem("Data", tabName = "data_tab", icon = icon("table")),
                  menuItem("About", tabName = "about", icon = icon("info-circle"))
      ),
      hidden(uiOutput("db_selector")),
      hidden(uiOutput("country_selector")),  # default: visible
      #hidden(selectInput("var_sel", "Variable", choices = NULL)),
      shinyjs::hidden(
        div(
          id = "jstree_container", # Agregamos un ID para poder referenciarlo
          style = "padding: 15px;",
          tags$label("Select a state:", `for` = "jstree_demo"),
          div(id = "jstree_demo")
        )),
      shinyjs::hidden(
        div(
          id = "jstree_vars_container", # Agregamos un ID para poder referenciarlo
          style = "padding: 15px;",
          tags$label("Select a variable:", `for` = "jstree_vars_demo"),
          div(id = "jstree_vars_demo")
        )),
      
      shinyjs::hidden(
        div(
          id = "jstree_vars_container_graph", # Agregamos un ID para poder referenciarlo
          style = "padding: 15px;",
          tags$label("Select a variable:", `for` = "jstree_vars_demo_graph"),
          div(id = "jstree_vars_demo_graph")
        )),
      
      #hidden(uiOutput("state_selector")),
      #hidden(selectInput("var_sel2", "Variable", choices = NULL)),
      hidden(selectInput("country_sel2", "Select a country:", choices = c(unique(data$country_name)), selected = "ARGENTINA")),
      hidden( pickerInput(
        inputId = "years",
        label = "Select year(s):",
        choices = 1983:2024,
        selected = 1983:2024,          # todos seleccionados al inicio
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          #liveSearch = TRUE,
          selectedTextFormat = "count",      # o "count > 0" si quieres que SIEMPRE muestre conteo
          countSelectedText = "{0} years selected",
          noneSelectedText = "Choose a year",
          size = 5,
          virtualScroll = 2
        )
      )),
      hidden(selectInput("state_sel2", "Select a state:", choices = NULL)),
      hidden(uiOutput("country_selector_camera")),
      hidden(uiOutput("state_selector_camera")),
      hidden(uiOutput("chamber_selector_camera"))
      )
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
      HTML("Tool developed by <strong><a href='https://www.linkedin.com/in/felipesotojorquera/' target='_blank'>Felipe Soto Jorquera.</a></strong>")
    ),
    tags$head(
      
      ## move the toggle
      tags$script(HTML("
    $(function () {
      var $logo   = $('.main-header .logo').first();
      var $toggle = $('.main-header .navbar .sidebar-toggle').first();
      if ($logo.length && $toggle.length) {
        // Move the existing toggle into the logo, before the title text
        $toggle.attr('id','sidebar-toggle-relocated'); // give it an id for styling
        $toggle.detach().prependTo($logo);
      }
    });
  ")),
      tags$style(HTML("
      .content-wrapper, .right-side {
        padding-bottom: 200px;  /* adjust value as you like */
      }
    ")),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jstree/3.3.11/jstree.min.js"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/jstree/3.3.11/themes/default/style.min.css"),
      tags$link(id = "theme-css", rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "custom.js")
      
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
                  #title = "Variable description", 
                  solidHeader = F,
                  collapsible = F,
                  collapsed = FALSE,
                  width = NULL,
                  closable = TRUE,
                  uiOutput("var_description_map")
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
                 linePlotModuleUI("lp")
          ),
          column(3,
                 box(
                   #title = "Variable description", 
                   solidHeader = F,
                   collapsible = FALSE, 
                   collapsed = FALSE,
                   width = NULL,
                   closable = TRUE,
                   uiOutput("var_description_graph")
                 ),
                 box(
                   #title = "States:", 
                   #solidHeader = F,
                   width = NULL,
                   height = NULL,
                   linePlotLegendUI("lp")),
                 box(width = NULL,
                     height = NULL,
                     checkboxInput("force_y0", "Y-axis starts at 0", value = FALSE, ))
                 ),
          
        )
      ),
      
      tabItem(tabName = "camera",

              tagList(
                fluidRow(
                  column(9,
                         camaraUI("cam"),
                         div(
                           style = "left: 40%; right: 40%; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
                           uiOutput("year_selector_camera")
                         )
                  ),
                  column(3,
                         box(
                           #title = "Election Description", 
                           solidHeader = F,
                           collapsible = FALSE, 
                           collapsed = FALSE,
                           width = NULL,
                           uiOutput("text_camera")
                         ),
                         box(width = NULL,
                             height = NULL,
                             camaraLegendUI("cam")))

                ),
                
              )

      ),
      
      
        
        
      
      
      
      
      

      tabItem(tabName = "codebook", 
              uiOutput("pdf_visor")),
      
      
      tabItem(
        tabName = "data_tab",
        fluidRow(
          column(9, tableModuleUI("sub_table")),
          column(
            3,
            box(
              title = "Current database",
              solidHeader = TRUE,
              collapsible = FALSE,
              collapsed = FALSE,
              width = NULL,
              uiOutput("texto_db")
            ),
            selectInput(
              "file_format",
              "Select download format:",
              choices = c("CSV" = "csv", "Excel" = "xlsx")
            ),
            downloadButton("download_data", "Download this database"),
            
            br(),br()
            )
        ),
        fluidRow(column(12,databaseInfoModuleUI("dbinfo_data", title = "About the SPP Databases", 
                                                max_width = 1100)))
      ),
      
      tabItem(tabName = "about", aboutSPPUI("about"),
              databaseInfoModuleUI("dbinfo_about", title = "About the SPP Databases"))
      
      
    ),
    skin = "blue"
    
  )
)
