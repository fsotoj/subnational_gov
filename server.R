

server <- function(input, output, session) {
  
  output$conditional_year_ui <- renderUI({
    if (is.null(input$tabs) || input$tabs != "timeline") {
      selectInput("year_sel", "Year", choices = c(unique(data$year), "Select a year"), selected = "Select a year")
    }
  })
  
  
  
  #current_tab <- reactive(input$tabs)
  
  apply_filters <- reactive(input$apply_filters)
  
  output$var_description <- reactive({
    var_info <- dict %>% filter(variable == input$var_sel) %>% slice(1)
    paste0(var_info$variable, ": ", var_info$description)
  })
  
  data_map <- reactive({
    
    req(input$country_sel)
    req(input$year_sel)
    
    data %>%
      filter(country_name == input$country_sel, year == input$year_sel) %>%
      left_join(filter(geom, country_name == input$country_sel), by = c("state_code")) %>%
      st_as_sf()
    
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  
  output$last_elect_nat_box <- renderValueBox({
    req(input$country_sel)
    req(input$year_sel)
    
    max_year <- data %>% 
      filter(
        year <= input$year_sel, 
        country_name == input$country_sel,
        electoral_national_year == 1
      ) %>% 
      pull(year) %>% max()
    
    valueBox(
      value = max_year,
      subtitle = "Last National Election Year",
      icon = icon("calendar"),
      color = "aqua",
      width = 12
    )
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
    table <- data_map() %>%
      st_drop_geometry() %>%
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
  
  # Update state choices based on selected country
  output$conditional_state_ui <- renderUI({
    req(input$country_sel)
    
    if (input$tabs == "timeline") {
      states <- c(unique(data$state_name[data$country_name == input$country_sel]),"Select a state")
      selectInput("state", "Select a state", choices = states)
    }
  })
  
  # Reactive expression to filter and preprocess data
  timeline_data1 <- reactive({
    data %>%
      filter(country_name == input$country_sel, state_name == input$state) %>%
      select(year, head_name_sub, ideo_party_sub, term_head_sub,party = head_party_sub) %>%
      tidyr::separate(term_head_sub, into = c("start", "end"), sep = "-") %>%
      rename(content = head_name_sub, group = ideo_party_sub) %>%
      select(content, group, start, end,group,party) %>%
      distinct() %>% 
      mutate(start = as.POSIXct(start, format = "%d/%m/%Y"),
             end = as.POSIXct(end, format = "%d/%m/%Y")
      )
  })
  
  vistime_module_server("timeline1", data = timeline_data1, "Governor")
  
  timeline_data2 <- reactive({
    data %>%
      filter(country_name == input$country_sel) %>%
      select(year, head_name_national, ideo_party_national, term_head_national,party = head_party_national) %>%
      tidyr::separate(term_head_national, into = c("start", "end"), sep = "-") %>%
      rename(content = head_name_national, group = ideo_party_national) %>%
      select(content, group, start, end,group,party) %>%
      distinct() %>% 
      mutate(start = as.POSIXct(start, format = "%d/%m/%Y"),
             end = as.POSIXct(end, format = "%d/%m/%Y")
      )
  })
  
  vistime_module_server("timeline2", data = timeline_data2, "Presidential")
  


  
  
}
