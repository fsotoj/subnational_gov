

server <- function(input, output, session) {
  
  apply_filters <- reactive(input$apply_filters)
  
  output$var_description <- reactive({
    ifelse(
      input$var_sel == "Select a variable", 
      "Please select a variable to see the description.",{
        var_info <- dict %>% filter(variable == input$var_sel) %>% slice(1)
        paste0(var_info$variable, ": ", var_info$description)
    })
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
  
  
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    
    geom_filtered <- geom %>%
      filter(country_name == input$country_sel)
    
    data_filtered <- data %>%
      filter(country_name == input$country_sel, year == input$year_sel)
    
    left_join(geom_filtered, data_filtered, by = "country_state_code")
  }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
  
  
  

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
  



  
  
}
