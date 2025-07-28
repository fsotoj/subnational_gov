# Devuelve etiquetas de leyenda en función del tipo de variable
get_legend_labels <- function(breaks, type) {
  n_round <- ifelse(type == "continuous", 2, 0)
  
  if (type %in% c("discrete", "continuous")) {
    legend_labels <- paste0(
      format(round(breaks[-length(breaks)], n_round), nsmall = n_round, big.mark = ","),
      " – ",
      format(round(breaks[-1], n_round), nsmall = n_round, big.mark = ",")
    )
  } else if (type == "percentage") {
    legend_labels <- paste0(
      format(round(breaks[-length(breaks)] * 100, 2), nsmall = 2, big.mark = ","),
      "% – ",
      format(round(breaks[-1] * 100, 2), nsmall = 2, big.mark = ","),
      "%"
    )
  } else {
    legend_labels <- as.character(breaks)
  }
  
  return(legend_labels)
}

# Tu función de paletas
get_ggplot_palette <- function(type, palette_vector, values, party_colors = NULL) {
  na_color <- "#999999"
  na_label_text <- "Not available" 
  
  if (length(values) == 0) return(NULL)
  
  final_labels <- NULL
  final_colors_values <- NULL
  
  if (type == "binary") {
    current_labels <- c("No", "Yes")
    if(length(palette_vector) < length(current_labels)) {
      warning(paste("Palette vector too short for binary type. Expected", length(current_labels), "colors, got", length(palette_vector)))
      palette_vector <- c(palette_vector, rep("#CCCCCC", length(current_labels) - length(palette_vector)))
    } else if (length(palette_vector) > length(current_labels)) {
      palette_vector <- palette_vector[1:length(current_labels)]
    }
    
    final_labels <- c(current_labels, na_label_text)
    final_colors_values <- c(palette_vector, na_color)
    
  } else if (type == "gender") {
    current_labels <- c("Male", "Female", "Other")
    if(length(palette_vector) < length(current_labels)) {
      warning(paste("Palette vector too short for gender type. Expected", length(current_labels), "colors, got", length(palette_vector)))
      palette_vector <- c(palette_vector, rep("#CCCCCC", length(current_labels) - length(palette_vector)))
    } else if (length(palette_vector) > length(current_labels)) {
      palette_vector <- palette_vector[1:length(current_labels)]
    }
    
    final_labels <- c(current_labels, na_label_text)
    final_colors_values <- c(palette_vector, na_color)
    
  } else if (type == "categorical") {
    current_domain <- levels(factor(values)) # This 'values' already includes 'Not available' as a level from ggplotMapServer
    
    final_labels <- current_domain # The levels of 'values' are our final labels
    
    # Extract colors from palette_vector based on these levels to maintain order
    final_colors_values <- palette_vector[final_labels]
    
    # Ensure no NA values in colors if some level somehow missed a color,
    # though this should be handled by ggplotMapServer for parties and na_color
    final_colors_values[is.na(final_colors_values)] <- "#CCCCCC" 
    
  } else if (type == "ordinal") {
    current_labels <- c("Left", "Center Left", "Center Right", "Right")
    if(length(palette_vector) < length(current_labels)) {
      warning(paste("Palette vector too short for ordinal type. Expected", length(current_labels), "colors, got", length(palette_vector)))
      palette_vector <- c(palette_vector, rep("#CCCCCC", length(current_labels) - length(palette_vector)))
    } else if (length(palette_vector) > length(current_labels)) {
      palette_vector <- palette_vector[1:length(current_labels)]
    }
    
    final_labels <- c(current_labels, na_label_text)
    final_colors_values <- c(palette_vector, na_color)
    
  } else if (type %in% c("discrete", "continuous", "percentage")) {
    
    numeric_values <- suppressWarnings(as.numeric(as.character(values)))
    
    if (all(is.na(numeric_values)) && !all(is.na(values))) {
      warning("Input 'values' for 'discrete', 'continuous', or 'percentage' type were non-numeric and could not be coerced.")
      return(NULL)
    }
    
    values_for_calc <- numeric_values[!is.na(numeric_values)]
    
    single_value_mode <- FALSE
    
    if (length(unique(values_for_calc)) <= 1 && length(values_for_calc) > 0) {
      single_value_mode <- TRUE
    }
    
    if (single_value_mode) {
      val <- unique(values_for_calc)[1]
      
      if (type == "percentage") {
        formatted_val <- paste0(format(round(val * 100, 2), nsmall = 2, big.mark = ","), "%")
      } else {
        formatted_val <- format(round(val, ifelse(type == "continuous", 2, 0)), 
                                nsmall = ifelse(type == "continuous", 2, 0), big.mark = ",")
      }
      
      single_label <- paste0(formatted_val, " (único valor)")
      
      single_color <- tail(palette_vector, 1) 
      
      final_labels <- c(single_label)
      final_colors_values <- c(single_color)
      
      if (any(is.na(numeric_values))) {
        final_labels <- c(final_labels, na_label_text)
        final_colors_values <- c(final_colors_values, na_color)
      }
      
    } else {
      
      
      ci <- classInt::classIntervals(values, n = length(palette_vector), style = "jenks")
      breaks <- ci$brks
      
      if (anyDuplicated(breaks)) {
        ci <- classInt::classIntervals(values, n = length(palette_vector), style = "pretty")
        breaks <- ci$brks
      }
      
      legend_labels <- get_legend_labels(breaks, type)
      
      ordered_labels <- rev(legend_labels)
      colors_for_bins <- palette_vector[seq_along(legend_labels)]
      ordered_colors_values <- rev(colors_for_bins)
      
      final_labels <- c(ordered_labels, na_label_text)
      final_colors_values <- c(ordered_colors_values, na_color)
    }
  } else {
    return(NULL)
  }
  
  # Apply names ONLY ONCE at the very end of get_ggplot_palette
  final_colors <- setNames(final_colors_values, final_labels)
  
  scale_fun <- scale_fill_manual(
    values = final_colors,
    labels = final_labels,
    drop = FALSE,
    na.value = na_color,
    guide = guide_legend(reverse = FALSE)
  )
  
  if (type %in% c("discrete", "continuous", "percentage")) {
    if (single_value_mode) {
      cut_values <- as.character(numeric_values)
      cut_values[!is.na(numeric_values)] <- single_label
      cut_values[is.na(numeric_values)] <- na_label_text
    } else {
      cut_values <- cut(numeric_values, breaks = breaks, include.lowest = TRUE, labels = legend_labels)
      cut_values[is.na(numeric_values)] <- na_label_text
    }
    cut_values <- factor(cut_values, levels = final_labels)
    attr(scale_fun, "cut_values") <- cut_values
  }
  
  return(scale_fun)
}


# Módulo que solo devuelve un ggplot reactivo con el mapa
ggplotMapServer <- function(id, data_map, input_var_sel, dict, party_colors = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Define na_color and na_label_text here so they are accessible throughout the module
    na_color <- "#999999"
    na_label_text <- "Not available" 
    
    
    map_plot <- reactive({
      req(data_map(), input_var_sel())
      
      var_info <- dict %>% 
        filter(variable == input_var_sel()) %>% 
        slice(1)
      
      palette_from_dict <- unlist(strsplit(var_info$palette, ",")) 
      
      data_plot <- data_map() %>%
        mutate(.gg_value = .data[[input_var_sel()]])
      
      
      palette_vector_for_get_ggplot_palette <- NULL
      
      if (var_info$type == "binary") {
        full_levels <- c(0,1)
        labels <- c("No", "Yes")
        palette_vector_for_get_ggplot_palette <- palette_from_dict 
        
        data_plot <- data_plot %>%
          mutate(.gg_value = factor(
            .gg_value, 
            levels = c(full_levels, NA), 
            labels = c(labels, na_label_text),
            exclude = NULL 
          ))
      } else if (var_info$type == "gender") {
        full_levels <- c(0,1,2)
        labels <- c("Male", "Female", "Other")
        palette_vector_for_get_ggplot_palette <- palette_from_dict 
        
        data_plot <- data_plot %>%
          mutate(.gg_value = factor(
            .gg_value, 
            levels = c(full_levels, NA), 
            labels = c(labels, na_label_text),
            exclude = NULL
          ))
      } else if (var_info$type == "ordinal") {
        full_levels <- 1:4
        labels <- c("Left", "Center Left", "Center Right", "Right")
        palette_vector_for_get_ggplot_palette <- palette_from_dict 
        
        data_plot <- data_plot %>%
          mutate(.gg_value = factor(
            .gg_value, 
            levels = c(full_levels, NA), 
            labels = c(labels, na_label_text),
            exclude = NULL
          ))
      } else if (var_info$type == "categorical") {
        req(!is.null(party_colors))
        
        # Original unique domain values BEFORE handling NAs or adding "Not available"
        original_domain_values <- sort(unique(data_plot$.gg_value[!is.na(data_plot$.gg_value)]))
        
        # Prepare party colors for all *original* domain values
        party_colors_prepared <- tibble(
          head_party_sub = original_domain_values
        ) %>%
          left_join(party_colors, by = "head_party_sub") %>%
          mutate(color = ifelse(is.na(color), "#999999", color)) 
        
        party_colors_ordered <- party_colors_prepared %>%
          slice(match(original_domain_values, head_party_sub))
        
        # Determine the full set of levels for the factor including potentially "Not available"
        all_factor_levels <- original_domain_values
        all_factor_labels <- original_domain_values
        
        # Check if there are NAs in the *original* data column for this variable
        if (any(is.na(data_map()[[input_var_sel()]]))) {
          all_factor_levels <- c(original_domain_values, NA)
          all_factor_labels <- c(original_domain_values, na_label_text)
        }
        
        # Factorize .gg_value with the COMPLETE set of levels and labels
        data_plot <- data_plot %>%
          mutate(.gg_value = factor(
            .gg_value, 
            levels = all_factor_levels, 
            labels = all_factor_labels, 
            exclude = NULL 
          ))
        
        # Now, prepare the palette_vector to be passed to get_ggplot_palette
        # It must include colors for ALL_FACTOR_LABELS in the correct order
        colors_for_all_levels <- party_colors_ordered$color # Colors for original domain values
        
        if (any(is.na(data_map()[[input_var_sel()]]))) {
          colors_for_all_levels <- c(colors_for_all_levels, na_color) # Add NA color if NAs exist
        }
        
        # IMPORTANT: palette_vector_for_get_ggplot_palette must be named
        # with ALL_FACTOR_LABELS and have colors_for_all_levels as values
        palette_vector_for_get_ggplot_palette <- setNames(colors_for_all_levels, all_factor_labels)
        
      } else { # For discrete, continuous, percentage
        palette_vector_for_get_ggplot_palette <- palette_from_dict 
      }
      
      scale_fill <- get_ggplot_palette(var_info$type, palette_vector_for_get_ggplot_palette, data_plot$.gg_value)
      
      if (!is.null(attr(scale_fill, "cut_values"))) {
        data_plot$.gg_value <- attr(scale_fill, "cut_values")
      }
      
      ggplot(data_plot) +
        geom_sf(aes(fill = .gg_value), color = "black", size = 0.2) +
        scale_fill +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#041d2d", color = NA),
          panel.background = element_rect(fill = "#041d2d", color = NA),
          legend.background = element_rect(fill = "#041d2d"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white", face = "bold"),
          legend.position = "right",
          legend.direction = "vertical"
        ) +
        labs(fill = var_info$pretty_name)
    })
    
    return(map_plot)
  })
}