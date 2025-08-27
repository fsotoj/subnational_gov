server <- function(input, output, session) {
  default_states <- c("ARGENTINA-CAPITAL FEDERAL", "BRAZIL-DISTRITO FEDERAL", "MEXICO-CDMX")
  
  observeEvent(session, {
    # 2. Enviamos los datos del árbol Y la lista de nodos a seleccionar
    session$sendCustomMessage(
      "jstree_data",
      list(
        data = jstree_json_data,
        default_selected = default_states # Nuevo parámetro
      )
    )
  })
  
  # Esta es la parte clave para obtener el vector de estados
  # Creamos una variable reactiva para procesar los nodos seleccionados
  selected_states_vector <- reactive({
    if (is.null(input$selected_nodes) || input$selected_nodes == "[]") {
      return(c())
    }
    nodes <- fromJSON(input$selected_nodes)
    states_selected_ids <- nodes[grepl("-", nodes)]
    
    # *** MODIFICADO ***
    # Extraemos el nombre del estado de cada ID y lo mantenemos en mayúsculas para el filtrado
    states_to_filter <- sapply(strsplit(states_selected_ids, "-"), function(x) x[2])
    
    return(states_to_filter)
  })
  
  observeEvent(session, {
    session$sendCustomMessage(
      "jstree_data",
      list(
        data = jstree_json_data
      )
    )
  })
  
  # Esta es la parte clave para obtener el vector de estados
  # Creamos una variable reactiva para procesar los nodos seleccionados
  selected_states_vector <- reactive({
    if (is.null(input$selected_nodes) || input$selected_nodes == "[]") {
      return(c()) # Devolvemos un vector vacío si no hay selección
    }
    
    nodes <- fromJSON(input$selected_nodes)
    
    # Filtramos solo los nodos que son estados (los que contienen "-")
    states_selected_ids <- nodes[grepl("-", nodes)]
    
    # Extraemos el nombre del estado de cada ID
    states_to_filter <- sapply(strsplit(states_selected_ids, "-"), function(x) x[2])
    
    return(states_to_filter)
  })
  
  
  ### ABOUT
  observe({
    showModal(modalDialog(
      title = "About the Subnational Politics Project (SPP)",
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
      "),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("Close")
    ))
  })
  
  #### capture button
  observeEvent(input$captureMapBtn, {
    
    session$sendCustomMessage(
      type = paste0('captureMap', "map1"), # Use the hardcoded module ID here
      message = list(
        filename = paste0("map_", input$country_sel,input$year_sel,"_", var_normal_name(), ".png"),
        scale = 2
      )
    )
  })

  ### show capture button
  observe({
    if (current_tab() == "map_tab") {
      #Sys.sleep(2)
      show("captureMapBtn")
      }
    })
    

  current_tab <- reactive({input$tabs})
  
  output$state_selector <- renderUI({
    
    req(current_tab() == "graph_tab")
    
    states_choices <- data %>%
      distinct(country_name, state_name) %>%
      arrange(country_name, state_name) %>%
      group_by(country_name) %>%
      group_split()
    
    choices_list <- lapply(states_choices, function(group) {
      setNames(as.list(group$state_name), group$state_name)
      })
    
    countries <- sapply(states_choices, function(g) unique(g$country_name))
    names(choices_list) <- countries
    
    pickerInput(
      inputId = "state_sel",
      label = "Select states from any country:",
      choices = choices_list,
      selected = c("CAPITAL FEDERAL", "DISTRITO FEDERAL", "CDMX"),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selectedTextFormat` = "values"
        )
      )
    })


  
  # Actualizar opciones y selección de var_sel y var_sel2 (independiente de hide/show)
  observe({
    updateSelectInput(
      session, "var_sel",
      choices = dict$pretty_name[dict$viewable_map == 1],
      #selected = "Governor Party Ideology")
      selected = "Total Voters")
    
    updateSelectInput(
      session, "var_sel2",
      choices = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Voter Turnout Percentage")
    })
  
  
  output$db_selector <- renderUI({
    selectInput("db_sel",label = "Select a database to view:", choices = c("NED","SED","SEED","SLED"
                                                                           #,"CFTDFLD"
                                                                           ))
  })
  
  # SHOW NO DATA MESSAGE
  observe({ 
    req(current_tab() == "map_tab",data_map())
    
    if (nrow(data_map()) == 0 || all(is.na(data_map()[[var_normal_name()]]))) {
    show("no_data_message")
    
  } else {hide("no_data_message")}
    })
  
  # SHOW HIDE SELECTORS
  observe({
    if (current_tab() == "map_tab") {
      show("var_sel")
      hide("var_sel2")
      show("var_description_map")
      hide("var_description_graph")
      hide("jstree_container")
      hide("years")
    } 
    
    if (current_tab() == "graph_tab") {
      hide("var_sel")
      show("var_sel2")
      hide("var_description_map")
      show("var_description_graph")
      show("state_selector")
      show("jstree_container")
      hide("years")
    }
    
    if (!(current_tab() %in% c("graph_tab", "map_tab"))){
      shinyjs::hide("var_sel")
      shinyjs::hide("var_sel2")
      hide("jstree_container")
    }
  })
  
  
  observe({
    if (current_tab() == "data_tab") {
      show("country_sel2")
      show("state_sel2")
      show("columns_sel")
      show("columns_sel")
      show("db_selector")
      show("years")
    } else {
      hide("country_sel2")
      hide("state_sel2")
      hide("columns_sel")
      hide("db_selector")
    }
  })
  
  
  
  # Render texto descripción variable para map_tab
  output$var_description_map <- renderText({
    req(current_tab() == "map_tab")
    req(input$var_sel)
    var_info <- dict %>% filter(pretty_name == input$var_sel) %>% slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  
  output$no_data_message <- renderText("⚠ No data available for this country, variable and year.")
  
  # Render texto descripción variable para graph_tab
  output$var_description_graph <- renderText({
    req(current_tab() == "graph_tab")
    req(input$var_sel2)
    var_info <- dict %>% filter(pretty_name == input$var_sel2) %>% slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  # Selector de país (solo en map_tab)
  output$country_selector <- renderUI({
    req(current_tab() == "map_tab")
    selectInput("country_sel", "Country", choices = c("Select a country", unique(data$country_name)), 
                selected = "MEXICO")
  })
  
  # Selector de año (solo en map_tab)
  output$year_selector <- renderUI({
    req(current_tab() == "map_tab")
    
    shinyWidgets::sliderTextInput(
      inputId = "year_sel",
      label = "Year",
      choices = as.character(seq(1983, 2024, 1)),
      grid = TRUE,
      width = "90%",
      animate = TRUE,
      selected = 2019
    )
  })
  

  
  # Reactives para variable normalizada
  var_normal_name <- reactive({
    dict %>% filter(pretty_name == input$var_sel) %>% pull(variable)
  }) 
  
  var_normal_name2 <- reactive({
    req(input$var_sel2)
    dict %>% filter(pretty_name == input$var_sel2) %>% pull(variable)
  }) 
  
  # Datos para el mapa filtrados por país y año
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    
    geom_filtered <- geom %>%
      filter(country_name == input$country_sel)
    
    data_filtered <- data %>%
      filter(country_name == input$country_sel, year == input$year_sel)
    
    left_join(geom_filtered, data_filtered, by = "country_state_code")
  }) 
  
  # Resumen líder nacional
  output$leader_summary <- renderUI({
    req(data_map(), input$country_sel)
    
    leader_info <- data_map() %>%
      sf::st_drop_geometry() %>%
      select(
        name_head_nat_exe, sex_head_nat_exe, head_party_nat_exe,
        ideo_party_nat_exe, 
        #years_nat_exe, 
        reelec_nat_exe,
        early_exit_nat_exe, year_election_nat_exe, year
      ) %>%
      slice(1)
    
    country_name <- stringr::str_to_title(input$country_sel)
    
    sex <- ifelse(leader_info$sex_head_nat_exe == 1, "female", "male")
    article <- ifelse(leader_info$sex_head_nat_exe == 1, "She", "He")
    ideologies <- c("Left", "Center Left", "Center Right", "Right")
    ideology_text <- ifelse(
      leader_info$ideo_party_nat_exe %in% 1:4,
      ideologies[leader_info$ideo_party_nat_exe],
      "Unknown"
    )
    reelec <- ifelse(leader_info$reelec_nat_exe == 1, "was reelected", "was not reelected")
    early_exit <- ifelse(leader_info$early_exit_nat_exe == 1, "left office early", "completed the full term")
    #election_year <- ifelse(leader_info$electoral_national_year == 1, "There was a national election that year.", "No national election was held that year.")
    
    text <- glue::glue(
      "<div>",
      " In the year <strong>{leader_info$year}</strong> the national leader in <strong>{country_name}</strong> was <strong>{leader_info$name_head_nat_exe}</strong>, a {sex} politician affiliated with the <strong>{leader_info$head_party_nat_exe}</strong> party, which leans towards the <strong>{ideology_text}</strong> on the ideological spectrum. ",
      #"{article} served for <strong>{leader_info$years_nat_exe}</strong> years, {reelec}, and {early_exit}. </div>",
      # "{election_year}",
       "</div>"
    )
    
    HTML(text)
  }) 
  
  # Módulos para mapa y gráfico de línea
  mapModuleServer(
    "map1",
    data_map = data_map,
    input_var_sel = var_normal_name,
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    active_tab = current_tab 
  )

  
  linePlotModuleServer(
    id = "line_plot1",
    data = reactive(data),
    dict = dict,
    input_variable = var_normal_name2,
    input_states = selected_states_vector,
    Ymin = reactive(if (input$force_y0) 0 else NULL),
    active_tab = current_tab
  )
  
  # Renderizar tabla en data_tab
  output$table_info <- DT::renderDT({
    req(input$country_sel2, input$state_sel2, current_tab() == "data_tab", input$db_sel)
    
    db_name <- input$db_sel
    
    data_sel <- if (db_name == "SED") SED else if (db_name == "SEED") SEED else if (db_name == "NED") NED else if (db_name == "SLED") SLED else if (db_name == "CFTDFLD") CFTDFLD else NULL
    
    
    
    
    filtered_data <- data_sel %>%
      filter(
        country_name == input$country_sel2,
        if (db_name!="NED") state_name == input$state_sel2 else TRUE,
        year %in% input$years
      ) 
    # %>%
    #   select(all_of(input$columns_sel))
    
    DT::datatable(
      filtered_data,
      options = list(
        scrollX = TRUE,
        scrollY = "80vh",
        paging = FALSE,
        scrollCollapse = TRUE,
        dom = 't'
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  
  
  output$texto_db <- renderUI({
    req(input$db_sel)  
    
    texto <- switch(input$db_sel,
                    "NED" = "<b>National Executive Databse:</b> Data on national executive branches per country.",
                    "SED" = "<b>Subnational Executive Database:</b> Data on subnational executive branches per state/province, per country.",
                    "SEED" = "<b>Subnational Executive Elections Database:</b> Data on electoral results for executive branch.",
                    "SLED" = "<b>Subnational Legislative Elections Database:</b> Data on subnational executive elections by state/province and country. It also includes institutional and electoral information on state- or provincial-level legislatures.",
                    "CFTDFLD" = "<b>Capital Federal & Tierra del Fuego Legislatures Database:</b> Data on the Capital Federal and Tierra del Fuego provinces, including detailed information on their legislatures.",
                    "No data"
    )
    HTML(texto)
  })
  
  
  
  # Actualizar estados para data_tab según país seleccionado
  observeEvent(input$country_sel2, {
    req(input$country_sel2)
    states_available <- data %>%
      filter(country_name == input$country_sel2) %>%
      pull(state_name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "state_sel2",
                      choices = states_available,
                      selected = states_available[1])
  })
  
  observeEvent(input$db_sel,{
    req(input$db_sel)
    if (input$db_sel == "NED") {
      disable("state_sel2")
    } else {enable("state_sel2")}
    
  })
  
  
  
  
  # Descarga datos (excel)
  output$download_data <- downloadHandler(
    filename = function() {
      ext <- if (input$file_format == "csv") "csv" else "xlsx"
      paste0(input$db_sel, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(input$db_sel, input$file_format)  # Asegura que ambos inputs existan
      
      # Asegúrate de que el objeto existe
      if (!exists(input$db_sel, envir = .GlobalEnv)) {
        stop("Selected dataset does not exist in the global environment.")
      }
      
      data_to_download <- get(input$db_sel, envir = .GlobalEnv)
      
      if (input$file_format == "csv") {
        write.csv(data_to_download, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        openxlsx::write.xlsx(data_to_download, file)
      }
    }
  )
  

  
  
  # Descarga geometría (geojson)
  output$download_geom <- downloadHandler(
    filename = function() {
      paste("countries_geom_", Sys.Date(), ".geojson", sep = "")
    },
    content = function(file) {
      st_write(geom, file, append = FALSE)
    },
    contentType = "application/geo+json"  
  )
  
  # Visor PDF incrustado
  output$pdf_visor <- renderUI({
    tags$iframe(style = "height:800px; width:100%;",
                src = "codebook.pdf")
  })
  
}
