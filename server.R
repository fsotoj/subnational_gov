

server <- function(input, output, session) {
  

  current_tab <- reactive({input$tabs})
  
  
  output$state_selector <- renderUI({
    req(current_tab() == "graph_tab")  # solo renderiza si esta pestaña está activa
    
    # Agrupamos estados por país
    states_choices <- data %>%
      distinct(country_name, state_name) %>%
      arrange(country_name, state_name) %>%
      group_by(country_name) %>%
      group_split()
    
    choices_list <- lapply(states_choices, function(group) {
      setNames(as.list(group$state_name), group$state_name)
    })
    names(choices_list) <- sapply(states_choices, function(g) unique(g$country_name))
    
    selectizeInput(
      inputId = "state_sel",
      label = "Select states from any country:",
      selected = c("CAPITAL FEDERAL","DISTRITO FEDERAL","CDMX"),
      choices = choices_list,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })
  
  
  observe({
    updateSelectInput(
      session, "var_sel",
      choices = dict$pretty_name[dict$viewable_map == 1],
      selected = "Subnatl. Head of State Party Affiliation"
    )
    
    updateSelectInput(
      session, "var_sel2",
      choices = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Subnatl. Turnout Rate"
    )
  })
  
  # Mostrar/ocultar según pestaña
  observe({
    if (current_tab() == "map_tab") {
      shinyjs::show("var_sel")
      shinyjs::hide("var_sel2")
    } 
    
    if (current_tab() == "graph_tab") {
      shinyjs::hide("var_sel")
      shinyjs::show("var_sel2")
    }
  })
  
  # Mostrar descripción de variable
  output$var_description_map <- renderText({
    req(current_tab() == "map_tab")
    req(input$var_sel)
    var_info <- dict %>% filter(pretty_name == input$var_sel) %>% slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  output$var_description_graph <- renderText({
    req(current_tab() == "graph_tab")
    req(input$var_sel2)
    var_info <- dict %>% filter(pretty_name == input$var_sel2) %>% slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  observe({
    if (current_tab() == "map_tab") {
      shinyjs::show("var_description_map")
      shinyjs::hide("var_description_graph")
    } 
    if (current_tab() == "graph_tab") {
      shinyjs::hide("var_description_map")
      shinyjs::show("var_description_graph")
    }
  })


  
  
  
  output$country_selector <- renderUI({
    req(current_tab() == "map_tab")
    selectInput("country_sel", "Country", choices = c("Select a country",unique(data$country_name)), 
                selected = "ARGENTINA")
    })
  
  
  
  
  output$year_selector <- renderUI({
    req(current_tab() == "map_tab")
    
    shinyWidgets::sliderTextInput(
      inputId = "year_sel",
      label = "Year",
      choices = as.character(seq(1983, 2024, 1)),  # Only specific years
      
      grid = TRUE,  # Show tick marks
      width = "90%",
      animate = T
    )
    
  })
  
  observe({
    # Solo actualizar los ticks si estamos en el tab correcto
    req(current_tab() == "map_tab")
    
    # Queremos mostrar ticks solo para estos años:
    desired_years <- c(1990, 2000, 2010, 2020)
    all_years <- 1983:2024
    
    # Calculamos las posiciones de estos años en el vector de choices
    indices <- which(all_years %in% desired_years) - 1  # -1 porque JS usa índice base 0
    
    session$sendCustomMessage("custom_ticks", list(
      labels = as.character(desired_years),
      indices = indices
    ))
  })
  
  


  
  var_normal_name <- reactive({
    dict %>% filter(pretty_name == input$var_sel) %>% 
      pull(variable)
    }) 
  
  var_normal_name2 <- reactive({
    req(input$var_sel2)
    dict %>% filter(pretty_name == input$var_sel2) %>% 
      pull(variable)
  }) 
  

  
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    
    geom_filtered <- geom %>%
      filter(country_name == input$country_sel)
    
    data_filtered <- data %>%
      filter(country_name == input$country_sel, year == input$year_sel)
    
    left_join(geom_filtered, data_filtered, by = "country_state_code")
  }) 
  
  
  

  output$leader_summary <- renderUI({
    req(data_map(),input$country_sel)
    
    leader_info <- data_map() %>%
      st_drop_geometry() %>%
      select(
        head_name_national, sex_head_national, head_party_national,
        ideo_party_national, years_nat_gov, reelec_nat_gov,
        early_exit_nat, electoral_national_year,year,
      ) %>%
      slice(1)
    
    country_name <- stringr::str_to_title(input$country_sel)
    
    
    # Traducir sexo
    sex <- ifelse(leader_info$sex_head_national == 1, "female", "male")
    
    # Traducir ideología
    ideologies <- c("Left", "Center Left", "Center Right", "Right")
    ideology_text <- ifelse(
      leader_info$ideo_party_national %in% 1:4,
      ideologies[leader_info$ideo_party_national],
      "Unknown"
    )
    
    # Traducciones para otros campos binarios
    reelec <- ifelse(leader_info$reelec_nat_gov == 1, "was reelected", "was not reelected")
    early_exit <- ifelse(leader_info$early_exit_nat == 1, "left office early", "completed the full term")
    election_year <- ifelse(leader_info$electoral_national_year == 1, "There was a national election that year.", "No national election was held that year.")
    
    # Texto narrativo
    text <- glue::glue(
      "<div style='padding:10px; font-size:16px; line-height:1.5em;'>",
      " In ther year <strong>{leader_info$year}</strong> the national leader in <strong>{country_name}</strong> was <strong>{leader_info$head_name_national}</strong>, a {sex} politician affiliated with the <strong>{leader_info$head_party_national}</strong> party, which leans towards the <strong>{ideology_text}</strong> on the political spectrum. ",
      "They served for <strong>{leader_info$years_nat_gov}</strong> years in office, {reelec}, and {early_exit}. ",
      "{election_year}",
      "</div>"
    )
    
    HTML(text)
  }) 
  
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
  
  
  
  output$table_info <- DT::renderDT({
    DT::datatable(
      data,
      options = list(
        scrollY = "400px",  # Altura visible con scroll vertical
        paging = FALSE,     # Sin paginación
        scrollCollapse = TRUE,
        dom = 't'           # Solo la tabla, sin barra de búsqueda ni info
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })
  
  
  
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(data, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$download_geom <- downloadHandler(
    filename = function() {
      paste("countries_geom_", Sys.Date(), ".geojson", sep = "")
    },
    content = function(file) {
      st_write(geom, file, append = FALSE)
    },
    contentType = "application/geo+json"  
    
  )
  
  output$pdf_visor <- renderUI({
    tags$iframe(style = "height:800px; width:100%;",
                src = "codebook.pdf")
  })
  
  
  
# 
#   session$onSessionEnded(function() {
#     message("Cleaning global environment...")  # optional: for visibility
#     rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
#     gc()  # optional: trigger garbage collection
#   })


  
  
  
}
