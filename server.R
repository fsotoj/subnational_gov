server <- function(input, output, session) {
  
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
      "),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$captureMapBtn, {
    
    session$sendCustomMessage(
      type = paste0('captureMap', "map1"), # Use the hardcoded module ID here
      message = list(
        filename = paste0("map_", input$country_sel,input$year_,"_", var_normal_name(), ".png"),
        scale = 2
      )
    )
  })

  observe({
    if (current_tab() == "map_tab") {
      Sys.sleep(1)
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
      selected = "Subnatl. Head of State Party Affiliation")
    
    updateSelectInput(
      session, "var_sel2",
      choices = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Subnatl. Turnout Rate")
    })
  
  
  observe({
    if (current_tab() == "map_tab") {
      show("var_sel")
      hide("var_sel2")
      show("var_description_map")
      hide("var_description_graph")
    } 
    
    if (current_tab() == "graph_tab") {
      hide("var_sel")
      show("var_sel2")
      hide("var_description_map")
      show("var_description_graph")
      show("state_selector")
    }
    
    if (!(current_tab() %in% c("graph_tab", "map_tab"))){
      shinyjs::hide("var_sel")
      shinyjs::hide("var_sel2")
    }
  })
  
  
  observe({
    if (current_tab() == "data_tab") {
      shinyjs::show("country_sel2")
      shinyjs::show("state_sel2")
      shinyjs::show("columns_sel")
    } else {
      shinyjs::hide("country_sel2")
      shinyjs::hide("state_sel2")
      shinyjs::hide("columns_sel")
    }
  })
  
  
  
  # Render texto descripción variable para map_tab
  output$var_description_map <- renderText({
    req(current_tab() == "map_tab")
    req(input$var_sel)
    var_info <- dict %>% filter(pretty_name == input$var_sel) %>% slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
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
                selected = "ARGENTINA")
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
      animate = TRUE
    )
  })
  
  # Actualizar ticks del slider solo en map_tab
  observe({
    req(current_tab() == "map_tab")
    
    desired_years <- c(1990, 2000, 2010, 2020)
    all_years <- 1983:2024
    indices <- which(all_years %in% desired_years) - 1  # JS base 0
    
    session$sendCustomMessage("custom_ticks", list(
      labels = as.character(desired_years),
      indices = indices
    ))
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
        head_name_national, sex_head_national, head_party_national,
        ideo_party_national, years_nat_gov, reelec_nat_gov,
        early_exit_nat, electoral_national_year, year
      ) %>%
      slice(1)
    
    country_name <- stringr::str_to_title(input$country_sel)
    
    sex <- ifelse(leader_info$sex_head_national == 1, "female", "male")
    article <- ifelse(leader_info$sex_head_national == 1, "She", "He")
    ideologies <- c("Left", "Center Left", "Center Right", "Right")
    ideology_text <- ifelse(
      leader_info$ideo_party_national %in% 1:4,
      ideologies[leader_info$ideo_party_national],
      "Unknown"
    )
    reelec <- ifelse(leader_info$reelec_nat_gov == 1, "was reelected", "was not reelected")
    early_exit <- ifelse(leader_info$early_exit_nat == 1, "left office early", "completed the full term")
    #election_year <- ifelse(leader_info$electoral_national_year == 1, "There was a national election that year.", "No national election was held that year.")
    
    text <- glue::glue(
      "<div>",
      " In the year <strong>{leader_info$year}</strong> the national leader in <strong>{country_name}</strong> was <strong>{leader_info$head_name_national}</strong>, a {sex} politician affiliated with the <strong>{leader_info$head_party_national}</strong> party, which leans towards the <strong>{ideology_text}</strong> on the ideological spectrum. ",
      "{article} served for <strong>{leader_info$years_nat_gov}</strong> years, {reelec}, and {early_exit}. </div>",
      # "{election_year}",
      # "</div>"
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
    input_states = reactive(input$state_sel),
    active_tab = current_tab
  )
  
  # Renderizar tabla en data_tab
  output$table_info <- DT::renderDT({
    req(input$country_sel2, input$state_sel2, current_tab() == "data_tab")
    
    filtered_data <- data %>%
      filter(
        country_name == input$country_sel2,
        state_name == input$state_sel2
      ) %>%
      select(all_of(input$columns_sel))
    
    DT::datatable(
      filtered_data,
      options = list(
        scrollX = TRUE,
        scrollY = "50vh",
        paging = FALSE,
        scrollCollapse = TRUE,
        dom = 't'
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
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
  
  
  # Descarga datos (excel)
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(data, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
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
