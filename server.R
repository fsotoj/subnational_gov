server <- function(input, output, session) {
  
  # =========================
  # 0) CONSTANTES / INICIALIZACIÓN
  # =========================
  default_states <- c("ARGENTINA-CAPITAL FEDERAL", "BRAZIL-DISTRITO FEDERAL", "MEXICO-CDMX")
  current_tab   <- reactive({ input$tabs })
  
  # Inicializa JSTree con selección por defecto
  observeEvent(session, {
    session$sendCustomMessage(
      "jstree_data",
      list(
        data = jstree_json_data,
        default_selected = default_states
      )
    )
  })
  
  # =========================
  # 1) REACTIVES GLOBALES
  # =========================
  # Estados seleccionados desde JSTree (para el gráfico)
  selected_states_vector <- reactive({
    if (is.null(input$selected_nodes) || input$selected_nodes == "[]") return(character(0))
    nodes <- jsonlite::fromJSON(input$selected_nodes)
    states_selected_ids <- nodes[grepl("-", nodes)]
    sapply(strsplit(states_selected_ids, "-"), function(x) x[2])
  })
  
  # Nombres de variable normalizados (mapa y gráfico)
  var_normal_name <- reactive({
    req(input$var_sel)
    dict %>% dplyr::filter(pretty_name == input$var_sel) %>% dplyr::pull(variable)
  })
  var_normal_name2 <- reactive({
    req(input$var_sel2)
    dict %>% dplyr::filter(pretty_name == input$var_sel2) %>% dplyr::pull(variable)
  })
  
  # Datos para el mapa (filtrados por país y año)
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    geom_filtered <- geom %>% dplyr::filter(country_name == input$country_sel)
    data_filtered <- data %>% dplyr::filter(country_name == input$country_sel, year == input$year_sel)
    dplyr::left_join(geom_filtered, data_filtered, by = "country_state_code")
  })
  
  # =========================
  # 2) MODALES / MENSAJES
  # =========================
  # About al cargar
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
  
  # Mensaje de “no data” en mapa
  output$no_data_message <- renderText("⚠ No data available for this country, variable and year.")
  
  # =========================
  # 3) CONTROLES DE CAPTURA DE MAPA
  # =========================
  observeEvent(input$captureMapBtn, {
    session$sendCustomMessage(
      type = paste0("captureMap", "map1"),
      message = list(
        filename = paste0("map_", input$country_sel, input$year_sel, "_", var_normal_name(), ".png"),
        scale = 2
      )
    )
  })
  observe({
    if (current_tab() == "map_tab") {
      shinyjs::show("captureMapBtn")
    } else {
      shinyjs::hide("captureMapBtn")
    }
  })
  
  # =========================
  # 4) UI DINÁMICO (SELECTORES)
  # =========================
  # Selector de estados (para gráfico - multipaís)
  output$state_selector <- renderUI({
    req(current_tab() == "graph_tab")
    states_choices <- data %>%
      dplyr::distinct(country_name, state_name) %>%
      dplyr::arrange(country_name, state_name) %>%
      dplyr::group_by(country_name) %>%
      dplyr::group_split()
    
    choices_list <- lapply(states_choices, function(group) {
      stats::setNames(as.list(group$state_name), group$state_name)
    })
    countries <- sapply(states_choices, function(g) unique(g$country_name))
    names(choices_list) <- countries
    
    shinyWidgets::pickerInput(
      inputId = "state_sel",
      label   = "Select states from any country:",
      choices = choices_list,
      selected = c("CAPITAL FEDERAL", "DISTRITO FEDERAL", "CDMX"),
      multiple = TRUE,
      options  = list(`actions-box` = TRUE, `live-search` = TRUE, `selectedTextFormat` = "values")
    )
  })
  
  # Selector de base de datos (para data_tab)
  output$db_selector <- renderUI({
    selectInput("db_sel", label = "Select a database to view:",
                choices = c("NED", "SED", "SEED", "SLED", "CFTDFLD"))
  })
  
  output$camera_selector <- renderUI({
    selectInput("camera_sel", label = "Select a camera:",
                choices = c(1, 2))
  })
  
  # Selectores de país/año (map_tab)
  output$country_selector <- renderUI({
    req(current_tab() == "map_tab")
    selectInput("country_sel", "Country",
                choices = c("Select a country", unique(data$country_name)),
                selected = "MEXICO")
  })
  output$year_selector <- renderUI({
    req(current_tab() == "map_tab")
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel", label = "Year",
      choices  = as.character(seq(1983, 2024, 1)),
      grid     = TRUE, width = "90%", animate = TRUE, selected = 2019
    )
  })
  
  output$year_selector_camera <- renderUI({
    req(current_tab() == "camera")
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel_camera", label = "Year",
      choices  = as.character(seq(1983, 2024, 1)),
      grid     = TRUE, width = "90%", animate = TRUE, selected = 2019
    )
  })
  
  # =========================
  # 5) MOSTRAR / OCULTAR CONTROLES POR PESTAÑA
  # =========================
  # Mostrar/ocultar mensaje “no data” (mapa)
  observe({
    req(current_tab() == "map_tab", data_map())
    if (nrow(data_map()) == 0 || all(is.na(data_map()[[var_normal_name()]]))) {
      shinyjs::show("no_data_message")
    } else {
      shinyjs::hide("no_data_message")
    }
  })
  
  # Mostrar/ocultar controles principales según pestaña activa
  observe({
    if (current_tab() == "map_tab") {
      shinyjs::show("var_sel");       shinyjs::hide("var_sel2")
      shinyjs::show("var_description_map"); shinyjs::hide("var_description_graph")
      shinyjs::hide("jstree_container"); shinyjs::hide("years")
    } else if (current_tab() == "graph_tab") {
      shinyjs::hide("var_sel");       shinyjs::show("var_sel2")
      shinyjs::hide("var_description_map"); shinyjs::show("var_description_graph")
      shinyjs::show("state_selector"); shinyjs::show("jstree_container")
      shinyjs::hide("years")
    } else {
      shinyjs::hide("var_sel"); shinyjs::hide("var_sel2")
      shinyjs::hide("jstree_container")
    }
  })
  
  # Mostrar/ocultar controles de la pestaña de datos
  observe({
    if (current_tab() == "data_tab") {
      shinyjs::show("country_sel2")
      shinyjs::show("state_sel2")
      shinyjs::show("db_selector")
      shinyjs::show("years")
      hide("camera_selector")
    } else if (current_tab() == "camera") {
      show("country_sel2")
      show("state_sel2")
      show("camera_selector")
      show("year_sel_camera")
      hide("years")
      hide("db_selector")
      
    } else {
      hide("camera_selector")
      hide("years")
      hide("country_sel2")
      hide("state_sel2")
      hide("db_selector")
    }
  })
  
  # =========================
  # 6) SINCRONIZACIÓN DE SELECTORES PARA DATA_TAB
  # =========================
  # a) Ajustes especiales al cambiar de base (CFTDFLD / NED)
  observeEvent(input$db_sel, {
    req(input$db_sel)
    if (input$db_sel == "CFTDFLD") {
      # País y estados disponibles en CFTDFLD
      if (!identical(input$country_sel2, "ARGENTINA")) {
        updateSelectInput(session, "country_sel2", selected = "ARGENTINA")
      }
      cft_states <- c("CAPITAL FEDERAL", "TIERRA DEL FUEGO")
      updateSelectInput(session, "state_sel2", choices = cft_states, selected = cft_states[1])
      shinyjs::disable("country_sel2"); shinyjs::enable("state_sel2")
      
    } else {
      shinyjs::enable("country_sel2")
      if (input$db_sel == "NED") {
        shinyjs::disable("state_sel2")
      } else {
        shinyjs::enable("state_sel2")
      }
    }
  })
  
  # b) Estados disponibles según país seleccionado (excepto CFTDFLD)
  observeEvent(input$country_sel2, {
    req(input$country_sel2)
    if (identical(input$db_sel, "CFTDFLD")) {
      updateSelectInput(
        session, "state_sel2",
        choices  = c("CAPITAL FEDERAL", "TIERRA DEL FUEGO"),
        selected = if (is.null(isolate(input$state_sel2))) "CAPITAL FEDERAL" else isolate(input$state_sel2)
      )
      return()
    }
    
    states_available <- data %>%
      dplyr::filter(country_name == input$country_sel2) %>%
      dplyr::pull(state_name) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "state_sel2",
                      choices  = states_available,
                      selected = states_available[1])
  })
  
  # =========================
  # 7) TEXTO DE DESCRIPCIÓN / VARS
  # =========================
  # Descripción de variable (mapa)
  output$var_description_map <- renderText({
    req(current_tab() == "map_tab", input$var_sel)
    var_info <- dict %>% dplyr::filter(pretty_name == input$var_sel) %>% dplyr::slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  # Descripción de variable (gráfico)
  output$var_description_graph <- renderText({
    req(current_tab() == "graph_tab", input$var_sel2)
    var_info <- dict %>% dplyr::filter(pretty_name == input$var_sel2) %>% dplyr::slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  # Sincroniza opciones por defecto de var_sel y var_sel2
  observe({
    updateSelectInput(
      session, "var_sel",
      choices  = dict$pretty_name[dict$viewable_map == 1],
      selected = "Total Voters"
    )
    updateSelectInput(
      session, "var_sel2",
      choices  = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Voter Turnout Percentage"
    )
  })
  
  # =========================
  # 8) RESUMEN LÍDER NACIONAL (MAPA)
  # =========================
  output$leader_summary <- renderUI({
    req(data_map(), input$country_sel)
    leader_info <- data_map() %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        name_head_nat_exe, sex_head_nat_exe, head_party_nat_exe,
        ideo_party_nat_exe, reelec_nat_exe, early_exit_nat_exe,
        year_election_nat_exe, year
      ) %>%
      dplyr::slice(1)
    
    country_name <- stringr::str_to_title(input$country_sel)
    sex <- ifelse(leader_info$sex_head_nat_exe == 1, "female", "male")
    ideologies <- c("Left", "Center Left", "Center Right", "Right")
    ideology_text <- ifelse(leader_info$ideo_party_nat_exe %in% 1:4,
                            ideologies[leader_info$ideo_party_nat_exe], "Unknown")
    
    text <- glue::glue(
      "<div>",
      " In the year <strong>{leader_info$year}</strong> the national leader in <strong>{country_name}</strong> ",
      "was <strong>{leader_info$name_head_nat_exe}</strong>, a {sex} politician affiliated with the ",
      "<strong>{leader_info$head_party_nat_exe}</strong> party, which leans towards the ",
      "<strong>{ideology_text}</strong> on the ideological spectrum.",
      "</div>"
    )
    HTML(text)
  })
  
  # =========================
  # 9) MÓDULOS (MAPA / LÍNEAS / TABLA)
  # =========================
  mapModuleServer(
    id = "map1",
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
    Ymin = reactive(if (isTRUE(input$force_y0)) 0 else NULL),
    active_tab = current_tab
  )
  
  tableModuleServer(
    id         = "sub_table",
    datasets   = list(SED = SED, SEED = SEED, NED = NED, SLED = SLED, CFTDFLD = CFTDFLD),
    db_selector = reactive(input$db_sel),
    country     = reactive(input$country_sel2),
    state       = reactive(input$state_sel2),
    years       = reactive(input$years),
    active_tab  = current_tab,
    force_styles = TRUE
  )
  
  
  camaraServer(
    id = "camara",
    data = SLED, # static SLED
    state_r = reactive(input$state_sel2), # <-- use state_sel2 on the camera tab
    chamber_r = reactive(input$camera_sel), # 1/2 as in your UI
    year_r = reactive(input$year_sel_camera), # camera-year slider
    party_col = "party_name_sub_leg",
    seats_col = "total_seats_party_sub_leg",
    state_col = "state_name",
    chamber_filter_col = "chamber_election_sub_leg",
    year_col = "year",
    title_text = "Chamber composition" # optional
  )
  
  
  # =========================
  # 10) DESCARGAS
  # =========================
  # a) Datos (CSV/XLSX)
  output$download_data <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$file_format, "csv")) "csv" else "xlsx"
      paste0(input$db_sel, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(input$db_sel, input$file_format)
      if (!exists(input$db_sel, envir = .GlobalEnv)) {
        stop("Selected dataset does not exist in the global environment.")
      }
      data_to_download <- get(input$db_sel, envir = .GlobalEnv)
      if (identical(input$file_format, "csv")) {
        write.csv(data_to_download, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        openxlsx::write.xlsx(data_to_download, file)
      }
    }
  )
  
  # b) Geometría (GeoJSON)
  output$download_geom <- downloadHandler(
    filename = function() paste0("countries_geom_", Sys.Date(), ".geojson"),
    content = function(file) sf::st_write(geom, file, append = FALSE),
    contentType = "application/geo+json"
  )
  
  # =========================
  # 11) VISOR PDF
  # =========================
  output$pdf_visor <- renderUI({
    tags$iframe(style = "height:800px; width:100%;", src = "codebook.pdf")
  })
}
