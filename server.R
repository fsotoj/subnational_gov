

server <- function(input, output, session) {
  
  #apply_filters <- reactive(input$apply_filters)
  
  # selected_states <- stateSelectorModuleServer(
  #   id = "state_selector",
  #   data = data,  
  #   active_tab = reactive(input$tabs)
  # )
  # 
  # 
  # output$selected_states <- renderPrint({
  #   selected_states()
  # })
  
  
  output$state_selector <- renderUI({
    req(input$tabs == "graph_tab")  # solo renderiza si esta pestaña está activa
    
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
      choices = choices_list,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })
  
  
  output$variable_selector <- renderUI({
    req(input$tabs)
    
    if (input$tabs == "map_tab"){
      
      variables <- dict %>% filter(viewable_map == 1) %>% pull(pretty_name) %>% unique()
      
      selectInput("var_sel", "Variable", 
                  choices = c("Select a variable",variables), 
                  selected = "Subnat. Leader Sex")
    } else {
      variables <- dict %>% filter(viewable_graph == 1) %>% pull(pretty_name) %>% unique()
      
      selectInput("var_sel", "Variable", 
                  choices = c("Select a variable",variables), 
                  selected = "Select a variable")
      }
    })
  
  
  
  
  output$country_selector <- renderUI({
    req(input$tabs == "map_tab")
    selectInput("country_sel", "Country", choices = c("Select a country",unique(data$country_name)), 
                selected = "ARGENTINA")
    })
  
  
  
  
  output$year_selector <- renderUI({
    req(input$tabs == "map_tab")
    sliderInput("year_sel", "Year", min = min(data$year), max = max(data$year), value = 2024, animate = T, sep = "")
  })
  
  
  
  output$var_description <- reactive({
    ifelse(
      input$var_sel == "Select a variable", 
      "Please select a variable to see the description.",{
        var_info <- dict %>% filter(pretty_name == input$var_sel) %>% slice(1)
        paste0(var_info$pretty_name, ": ", var_info$description)
    })
    })

  
  var_normal_name <- reactive({
    dict %>% filter(pretty_name == input$var_sel) %>% 
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
    active_tab = reactive(input$tabs) 
    )
  
  
  linePlotModuleServer(
    id = "line_plot1",
    data = reactive(data),
    input_variable = var_normal_name,
    input_states = reactive(input$state_sel),
    active_tab = reactive(input$tabs)
  )
  
  
  
  
  
  
  
  
  
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
