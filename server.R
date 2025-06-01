

server <- function(input, output, session) {
  
  apply_filters <- reactive(input$apply_filters)
  
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
  
  
  output$last_elect_nat_box <- renderValueBox({
    
    ifelse(input$year_sel == "Select a year" || input$country_sel == "Select a country",
           {
             value = "-"
             subtitle = "Please use the apply filters button"
             },
           {
             value = data %>% 
               filter(year <= input$year_sel, 
                      country_name == input$country_sel,
                      electoral_national_year == 1) %>% 
               pull(year) %>% max()
             subtitle = "Last National Election Year"
             })
    
    valueBox(value = value, subtitle = subtitle,
             icon = icon("calendar"), color = "aqua",width = 12)
    }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  

  
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    
    geom_filtered <- geom %>%
      filter(country_name == input$country_sel)
    
    data_filtered <- data %>%
      filter(country_name == input$country_sel, year == input$year_sel)
    
    left_join(geom_filtered, data_filtered, by = "country_state_code")
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  
  

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
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  mapModuleServer(
    "map1",
    data_map = data_map,
    input_var_sel = var_normal_name,
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    apply_filters = apply_filters
  )
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8-BOM")
    },
    contentType = "text/csv"
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
  
  
  
  
  session$onSessionEnded(function() {
    message("Cleaning global environment...")  # optional: for visibility
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    gc()  # optional: trigger garbage collection
  })


  
  
  
}
