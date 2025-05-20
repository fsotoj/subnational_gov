

server <- function(input, output, session) {
  
  output$conditional_year_ui <- renderUI({
    if (is.null(input$tabs) || input$tabs == "map_tab") {
      selectInput("year_sel", "Year", choices = c(unique(data$year), "Select a year"), selected = "Select a year")
    }
  })
  
  
  output$conditional_apply_ui <- renderUI({
    if (is.null(input$tabs) || input$tabs == "map_tab") {
      actionButton("apply_filters", "Apply Filters", icon = icon("arrows-rotate"))
    }
  })
  
  
  output$conditional_select_var_ui <- renderUI({
    if (is.null(input$tabs) || input$tabs == "map_tab") {
      selectInput("var_sel", "Variable", choices = c(unique(dict$variable), "Select a variable"), selected = "Select a variable")
    }
  })
  
  # Update state choices based on selected country
  output$conditional_state_ui <- renderUI({
    
    if (input$tabs != "timeline" || input$tabs == "votes_tab") {
      states <- c(unique(data$state_name[data$country_name == input$country_sel])) %>% sort()
      states <- c("Select a state",states)
      
      
      selectInput("state_sel", "Select a state", choices = states)
    }
  })
  
  
  
  apply_filters <- reactive(input$apply_filters)
  
  output$var_description <- reactive({
    ifelse(
      input$var_sel == "Select a variable", 
      "Please select a variable to see the description.",{
        var_info <- dict %>% filter(variable == input$var_sel) %>% slice(1)
        paste0(var_info$variable, ": ", var_info$description)
    })
    })
  
  data_map <- reactive({
    
    req(input$country_sel)
    req(input$year_sel)
    
    data %>%
      filter(country_name == input$country_sel, year == input$year_sel) %>%
      left_join(geom, by = c("country_state_code")) %>%
      st_as_sf()
    
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  
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
  
  
  output$table_info <- DT::renderDT({
    DT::datatable(
      data_info,
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
  
  
  

  output$table <- DT::renderDT({
    
    validate(
      need(input$year_sel != "Select a year" && input$country_sel != "Select a country",
           "Please use the apply filters button.")
    )
    
    table <- data %>%
      filter(country_name == input$country_sel, year == input$year_sel) %>%
      select(
        head_name_national, sex_head_national, head_party_national,
        ideo_party_national, years_nat_gov, reelec_nat_gov,
        early_exit_nat, electoral_national_year
        ) %>%
      slice(1) %>%
      t() %>%
      as.data.frame()
    
    colnames(table) <- NULL
    
    DT::datatable(table, colnames = NULL, options = list(dom = 't',
                                                         headerCallback = DT::JS("function(thead, data, start, end, display){ $(thead).remove(); }")))
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  mapModuleServer(
    "map1",
    data_map = data_map,
    input_var_sel = reactive(input$var_sel),
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    apply_filters = apply_filters
  )
  

  
  # Reactive expression to filter and preprocess data
  timeline_data1 <- reactive({
    req(input$state_sel)
    
    
    data %>%
      filter(country_name == input$country_sel, state_name == input$state_sel) %>%
      tidyr::separate(term_head_sub, into = c("start", "end"), sep = "-") %>%
      select(year, content = head_name_sub, group = ideo_party_sub, 
             party = head_party_sub, early_exit = early_exit_sub, start, end) %>%
      distinct() %>% #seguira siendo necesario?
      mutate(start = as.POSIXct(start, format = "%d/%m/%Y"),
             end = as.POSIXct(end, format = "%d/%m/%Y")
      ) %>% 
      filter(!is.na(start))
  })
  
  vistime_module_server("timeline1", data = timeline_data1, "Governor")
  
  timeline_data2 <- reactive({
    req(input$state_sel)
    
    data %>%
      filter(country_name == input$country_sel) %>%
      tidyr::separate(term_head_national, into = c("start", "end"), sep = "-") %>%
      select(year, content = head_name_national, group = ideo_party_national, 
             party = head_party_national, early_exit = early_exit_nat, start, end) %>%
      distinct() %>% #seguira siendo necesario?
      mutate(start = as.POSIXct(start, format = "%d/%m/%Y"),
             end = as.POSIXct(end, format = "%d/%m/%Y")
      ) %>% 
      filter(!is.na(start))
    })
  
  vistime_module_server("timeline2", data = timeline_data2, "Presidential")
  
  data_votes <- reactive({
    
    req(input$state_sel)
    
    data %>%
      filter(country_name == input$country_sel, state_name == input$state_sel) %>%
      filter(electoral_sub_year == 1)
  })
  
  mod_vote_bubbles_server("votes_module", data = data_votes)
  


  
  
}
