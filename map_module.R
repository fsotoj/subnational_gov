
get_leaflet_palette <- function(type, palette_vector, values) {
  values <- values[!is.na(values)]
  if (type == "binary") {
    pal <- colorFactor(palette = palette_vector, domain = c(0,1))
  } else if (type == "categorical" || type == "ordinal") {
    pal <- colorFactor(palette = palette_vector, domain = unique(values))
  } else if (type == "numerical" || type == "ratio") {
    breaks <- classInt::classIntervals(values, n = length(palette_vector), style = "quantile")$brks
    pal <- colorBin(palette = palette_vector, domain = values, bins = breaks, pretty = FALSE)
  } else {
    pal <- NULL
  }
  return(pal)
}

mapModuleUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "600px")
}

mapModuleServer <- function(id, data_map, input_var_sel, dict, country_bboxes, input_country_sel, apply_filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(apply_filters(), {
      leafletProxy(ns("map"), data = data_map()) %>%
        clearShapes() %>%
        clearControls()
    }, ignoreNULL = FALSE)
    
    output$map <- renderLeaflet({
      leaflet() %>%
        fitBounds(
          lng1 = country_bboxes[[input_country_sel()]]$lng1,
          lat1 = country_bboxes[[input_country_sel()]]$lat1,
          lng2 = country_bboxes[[input_country_sel()]]$lng2,
          lat2 = country_bboxes[[input_country_sel()]]$lat2
        ) %>%
        addTiles()
    })%>% bindEvent(input$apply_filters, ignoreNULL = FALSE)
    
    observeEvent(apply_filters(), {
      
      shinybusy::show_spinner()
      
      df_map <- data_map()
      values <- df_map[[input_var_sel()]]
      var_info <- dict %>% filter(variable == input_var_sel()) %>% slice(1)
      palette_vector <- unlist(strsplit(var_info$palette, ","))
      type <- var_info$type
      pal <- get_leaflet_palette(type, palette_vector, values)
      
      leafletProxy(ns("map"), data = df_map) %>%
        fitBounds(
          lng1 = country_bboxes[[input_country_sel()]]$lng1,
          lat1 = country_bboxes[[input_country_sel()]]$lat1,
          lng2 = country_bboxes[[input_country_sel()]]$lng2,
          lat2 = country_bboxes[[input_country_sel()]]$lat2
        ) %>%
        addPolygons(
          fillColor = ~pal(get(input_var_sel())),
          color = "#444444",
          weight = 1,
          fillOpacity = 0.8,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
          popup = ~paste0(
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
          pal = pal,
          values = ~get(input_var_sel()),
          opacity = 0.8,
          title = input_var_sel()
        )
      
      shinybusy::hide_spinner()
      
    })
  })
}




    
