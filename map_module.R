
get_leaflet_palette <- function(type, palette_vector, values) {
  if (length(values) != 0){
    
    
    if (type == "binary") {
      
      pal <- colorFactor(palette = palette_vector, domain = c(0,1))
      legend_labels <- c("0", "1")
      
      return(list(pal = pal, legend = legend_labels))
      
      
    } else if (type == "categorical" || type == "ordinal") {
      
      pal <- colorFactor(palette = palette_vector, domain = unique(values))
      legend_labels <- as.character(unique(values))
      
      return(list(pal = pal, legend = legend_labels))
      
      
      
    } else if (type == "numerical" || type == "ratio") {
      
      
      pal <- tryCatch({
        
        ci <- classInt::classIntervals(values, n = length(palette_vector), style = "jenks")
        breaks <- ci$brks
        
        if(anyDuplicated(breaks)){ ####
          
          ci <- classInt::classIntervals(values, n = length(palette_vector), style = "pretty")
          breaks <- ci$brks
        }
        
        pal <- colorBin(palette = palette_vector, domain = values, bins = breaks, pretty = T)
        
        
        legend_labels <- paste0(
          format(round(breaks[-length(breaks)], 2), nsmall = 2),
          " – ",
          format(round(breaks[-1], 2), nsmall = 2)
        )
        
        list(pal = pal, legend = legend_labels)
        
      }, error = function(e) {
        if (grepl("single unique value", e$message)) {
          val <- unique(values)[1]
          breaks <- c(val, val + 1e-6)  # crea un rango mínimo artificial
          pal <- colorBin(palette = tail(palette_vector, 1), domain = values, bins = breaks, pretty = FALSE)
          
          list(pal = pal, legend = paste0(val, " (único valor)"))
          
        } else {
          list(pal = NULL, legend = NULL)
        }
      })
      
    } else {
      pal <- NULL
      legend_labels <- NULL
      
    }
  }
}

mapModuleUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "600px")
}

mapModuleServer <- function(id, data_map, input_var_sel, dict, country_bboxes, input_country_sel, apply_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive components for optimization -------------------------------
    
    df_map <- reactive({
      req(data_map())
      data <- data_map()
      data[[".leaflet_value"]] <- data[[input_var_sel()]]  # Precompute for efficiency
      data
    }) %>% bindEvent(apply_filters())
    
    values <- reactive({
      df_map()[[".leaflet_value"]]
    })
    
    var_info <- reactive({
      dict %>%
        filter(variable == input_var_sel()) %>%
        slice(1)
    })
    
    palette_vector <- reactive({
      unlist(strsplit(var_info()$palette, ","))
    })
    
    pal <- reactive({
      get_leaflet_palette(var_info()$type, palette_vector(), values())
    })
    
    # Initial map render -------------------------------------------------
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = F)) %>%
        fitBounds(
          lng1 = country_bboxes[[input_country_sel()]]$lng1,
          lat1 = country_bboxes[[input_country_sel()]]$lat1,
          lng2 = country_bboxes[[input_country_sel()]]$lng2,
          lat2 = country_bboxes[[input_country_sel()]]$lat2
        ) %>%
        addTiles()
    }) %>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
    
    # Clear map when filters are applied ---------------------------------
    observeEvent(apply_filters(), {
      leafletProxy(ns("map"), data = data_map()) %>%
        clearShapes() %>%
        clearControls()
    }, ignoreNULL = FALSE)
    
    # Draw map polygons and legend ---------------------------------------
    observeEvent(apply_filters(), {
      shinybusy::show_spinner()
      
      leafletProxy(ns("map"), data = df_map()) %>%
        fitBounds(
          lng1 = country_bboxes[[input_country_sel()]]$lng1,
          lat1 = country_bboxes[[input_country_sel()]]$lat1,
          lng2 = country_bboxes[[input_country_sel()]]$lng2,
          lat2 = country_bboxes[[input_country_sel()]]$lat2
        ) %>%
        addPolygons(
          fillColor = ~pal()$pal(.leaflet_value),
          color = "#444444",
          weight = 1,
          fillOpacity = 0.8,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
          popup = ~paste0(
            "<b>",input_var_sel(),":</b> ",.leaflet_value, "<br/>",
            "<b>State:</b> ", state_name, "<br/>",
            "<b>Region:</b> ", region_name, "<br/>",
            "<b>Governor:</b> ", head_name_sub, "<br/>",
            "<b>Governor sex:</b> ", sex_head_sub, "<br/>",
            "<b>Party:</b> ", head_party_sub, "<br/>",
            "<b>Ideology:</b> ", ideo_party_sub, "<br/>",
            "<b>Alignment:</b> ", alignment, "<br/>",
            "<b>Years in office:</b> ", years_sub_gov, "<br/>",
            "<b>Early exit:</b> ", early_exit_sub, "<br/>",
            "<b>Reelected:</b> ", reelec_sub_gov, "<br/>",
            "<b>Electoral sub. year:</b> ", electoral_sub_year
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal()$pal,
          values = df_map()$.leaflet_value,
          labFormat = function(type, cuts, p) { pal()$legend },
          opacity = 0.8,
          title = input_var_sel()
        )
      
      shinybusy::hide_spinner()
    })
  })
}
