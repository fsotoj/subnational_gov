
get_leaflet_palette <- function(type, palette_vector, values) {
  if (length(values) != 0){
    
    
    if (type == "binary") {
      
      pal <- colorFactor(palette = palette_vector, domain = c(0,1))
      legend_labels <- c("No", "Yes")
      
      return(list(pal = pal, legend = legend_labels))
      
      
    } else if(type == "gender"){
      
      pal <- colorFactor(palette = palette_vector, domain = c(0,1))
      legend_labels <- c("Male", "Female")
      
      return(list(pal = pal, legend = legend_labels))
      
    } else if(type == "ordinal") {
      
      pal <- colorFactor(palette = palette_vector, domain = 1:4)
      legend_labels <- c("Left","Center Left", "Center Right", "Right")
      
      return(list(pal = pal, legend = legend_labels))
      
      
      
    } else if (type == "discrete" || type == "continuous") {
      
      
      
      pal <- tryCatch({
        
        ci <- classInt::classIntervals(values, n = length(palette_vector), style = "jenks")
        breaks <- ci$brks
        
        if(anyDuplicated(breaks)){ ####
          
          ci <- classInt::classIntervals(values, n = length(palette_vector), style = "pretty")
          breaks <- ci$brks
        }
        
        pal <- colorBin(palette = palette_vector, domain = values, bins = breaks, pretty = T)
        
        n_round <- ifelse(type=="continuous", 2, 0)
        
        legend_labels <- paste0(
          format(round(breaks[-length(breaks)], n_round), nsmall = n_round, big.mark = ","),
          "–",
          format(round(breaks[-1], n_round), nsmall = n_round, big.mark = ",")
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

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = TRUE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      full_levels <- if(length(unique(values)) == 1 && all(unique(values) %in% c(0,1))) c(0,1) else sort(unique(na.omit(values)))
      
      # Use full levels to get colors and labels
      if (decreasing == TRUE) {
        colors <- pal(rev(full_levels))
        labels <- rev(labFormat(type = "factor", full_levels))
      } else {
        colors <- pal(full_levels)
        labels <- labFormat(type = "factor", full_levels)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  if (all(is.na(values))) {
    colors <- na.color
    labels <- na.label
    na.color <- NULL  # evita mostrar color NA doble
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
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
        addProviderTiles("CartoDB.DarkMatterNoLabels")
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
          color = "black",
          weight = 1,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(weight = 5, color = "#666", fillOpacity = 1),
          popup = ~paste0(
            "<b>",input_var_sel(),": ",.leaflet_value, "</b><br/>",
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
        addLegend_decreasing(
          position = "bottomright",
          pal = pal()$pal,
          values = df_map()$.leaflet_value,
          labFormat = function(type, cuts, p) { pal()$legend },
          opacity = 0.9,
          title = var_info()$pretty_name
        )
      
      shinybusy::hide_spinner()
    })
  })
}
