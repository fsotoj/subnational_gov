# =============== UI HELPERS ==================
linePlotModuleUI <- function(id) {
  ns <- NS(id)
  # Only the plot here. Legend is handled by linePlotLegendUI() wherever you place it.
  plotlyOutput(ns("line_plot"), height = "80vh")
}

# Place this wherever you want the legend to appear (e.g., below your last box)
linePlotLegendUI <- function(id, width = NULL) {
  ns <- NS(id)
  tagList(
    # Optional: pass a width (e.g., 3/12) from the main UI grid; otherwise it fills container
    div(
      class = "lp-legend-host",
      style = "max-width: 100%;",
      # Light-theme legend styling using project palette
      tags$style(HTML(sprintf('
        #%1$s-legend .legend-title {margin: 0 0 8px 0; color: #4D4D4D; font-weight: 600; font-size: 20px;}
        #%1$s-legend .legend-wrap { display: flex; flex-direction: column; gap: 12px; }

        #%1$s-legend .legend-group { display: flex; flex-direction: column; gap: 8px; }
        #%1$s-legend .legend-country { display: inline-flex; align-items: center; gap: 8px; color: #4D4D4D; font-weight: 700; font-size: 12px; opacity: 0.95; }
        #%1$s-legend .country-swatch { width: 10px; height: 10px; border-radius: 2px; display: inline-block; box-shadow: 0 0 0 1px #DDDDDD; }

        #%1$s-legend .legend-row { display: flex; flex-wrap: wrap; gap: 8px 10px; }
        #%1$s-legend .legend-item { 
          display: inline-flex; align-items: center; gap: 8px; 
          padding: 6px 10px; border-radius: 9999px; 
          background: #FFFFFF; 
          box-shadow: 0 0 0 1px #E6E6E6 inset; 
          color: #4D4D4D; font-size: 12px; transition: box-shadow .12s ease;
        }
        #%1$s-legend .legend-item:hover { box-shadow: 0 0 0 1px #FFA92A inset; }

        #%1$s-legend .legend-swatch { width: 14px; height: 14px; display: inline-flex; align-items: center; justify-content: center; }
        #%1$s-legend .legend-text { white-space: nowrap; }
        #%1$s-legend svg { display: block; }
      ', ns("line_plot")))),
      div(id = paste0(ns("line_plot"), "-legend"), 
          tags$div(class = "legend-title", "States"),  # <-- your title here
          uiOutput(ns("legend"), container = div, inline = FALSE)
      )
    )
  )
}

# =============== SERVER ==================
linePlotModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab) {
  moduleServer(id, function(input, output, session) {
    
    # Helper to format values according to dict$type --------------------------
    format_value <- function(values, type) {
      values <- as.double(values)
      if (isTRUE(type == "discrete")) return(format(values, big.mark = ",", scientific = FALSE))
      if (isTRUE(type == "continuous")) return(format(round(values, 2), nsmall = 2, big.mark = ","))
      if (isTRUE(type == "percentage")) return(paste0(format(round(values, 2), nsmall = 2, big.mark = ","), "%"))
      format(values, big.mark = ",", scientific = FALSE)
    }
    
    # Small helper to draw Plotly-like symbols as inline SVG ------------------
    symbol_svg <- function(symbol = "circle", color = "#60A5FA") {
      s <- tolower(symbol)
      svg <- switch(
        s,
        "circle" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" fill="%1$s"/></svg>', color),
        "circle-open" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" fill="none" stroke="%1$s" stroke-width="2"/></svg>', color),
        "square" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><rect x="3" y="3" width="8" height="8" fill="%1$s"/></svg>', color),
        "square-open" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><rect x="3" y="3" width="8" height="8" fill="none" stroke="%1$s" stroke-width="2"/></svg>', color),
        "diamond" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><polygon points="7,2 12,7 7,12 2,7" fill="%1$s"/></svg>', color),
        "x" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><line x1="3" y1="3" x2="11" y2="11" stroke="%1$s" stroke-width="2"/><line x1="11" y1="3" x2="3" y2="11" stroke="%1$s" stroke-width="2"/></svg>', color),
        "star" = sprintf('<svg width="14" height="14" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><polygon points="12,2 15,9 22,9 16,13 18,21 12,17 6,21 8,13 2,9 9,9" fill="%1$s"/></svg>', color),
        sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" fill="%1$s"/></svg>', color)
      )
      htmltools::HTML(svg)
    }
    
    output$line_plot <- renderPlotly({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab")
      
      # Prepare data -----------------------------------------------------------
      df <- data() %>%
        dplyr::filter(state_name %in% input_states()) %>%
        dplyr::select(country_name, state_name, year, value = dplyr::all_of(input_variable())) %>%
        dplyr::mutate(date_year = suppressWarnings(as.Date(paste0(year, "-01-01")))) %>%
        tidyr::drop_na(date_year)
      req(nrow(df) > 0)
      
      # Pretty label and type from dict
      pretty_name <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(pretty_name) %>%
        { if (length(.) == 0 || is.na(.)) input_variable() else . }
      var_type <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(type) %>%
        { if (length(.) == 0 || is.na(.)) "continuous" else . }
      
      # Colors ----------------------------------------------------------------
      country_colors_base <- c(
        "ARGENTINA" = "#74ACDF",
        "BRAZIL"    = "#3CB371",
        "MEXICO"    = "#E03C31"
      )
      countries_in_df <- unique(df$country_name)
      missing_countries <- setdiff(countries_in_df, names(country_colors_base))
      if (length(missing_countries) > 0) {
        fallback_cols <- grDevices::hcl.colors(length(missing_countries), palette = "TealGrn")
        names(fallback_cols) <- missing_countries
        country_colors <- c(country_colors_base, fallback_cols)
      } else country_colors <- country_colors_base
      
      # Marker symbols cycle (mapped by state, keeps user order)
      symbols <- c("x", "circle", "diamond", "square", "star", "square-open", "circle-open")
      selected_states <- input_states()
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      
      # Determine y-range (optional external control)
      y_min <- Ymin()
      y_max <- max(df$value, na.rm = TRUE)
      y_range <- if (is.null(y_min)) NULL else c(y_min, y_max * 1.05)
      
      # x-axis bounds from data
      x_min <- min(df$date_year, na.rm = TRUE)
      x_max <- max(df$date_year, na.rm = TRUE)
      
      # Precompute pretty tick values and labels for Y axis --------------------
      y_lower <- if (is.null(y_min)) min(df$value, na.rm = TRUE) else y_min
      y_vals  <- pretty(c(y_lower, y_max))
      y_labs  <- format_value(y_vals, var_type)
      
      # Build figure -----------------------------------------------------------
      fig <- plotly::plot_ly()
      for (state in selected_states) {
        df_state <- df %>% dplyr::filter(state_name == state)
        if (nrow(df_state) == 0) next
        country <- unique(df_state$country_name)[1]
        color <- country_colors[[country]] %||% "#666666"
        df_state <- df_state %>% dplyr::mutate(.formatted_value = format_value(value, var_type))
        
        fig <- fig %>% plotly::add_trace(
          data = df_state,
          x = ~date_year,
          y = ~value,
          type = "scatter",
          mode = "lines+markers",
          name = state,
          legendgroup = country,
          customdata = df_state$.formatted_value,
          hovertemplate = paste0("<b>", state, "</b><br>", pretty_name, ": %{customdata}<extra></extra>"),
          line = list(color = color, width = 2.5),
          marker = list(symbol = symbol_map[[state]], size = 9, color = color)
        )
      }
      
      # Light theme ------------------------------------------------------------
      paper_bg <- "#FFFFFF"   # surrounding
      plot_bg  <- "#FFFFFF"   # canvas
      grid_col <- "#E5E7EB"   # light gray grid
      axis_txt <- "#4D4D4D"   # dark gray text
      accent   <- "#FFA92A"   # primary (orange)
      
      fig <- fig %>% plotly::layout(
        xaxis = list(
          title = list(text = "Year", font = list(size = 13, color = axis_txt)),
          type = "date",
          range = c(x_min, x_max),
          tickformat = "%Y",
          ticks = "outside",
          tickfont = list(size = 11, color = axis_txt),
          showgrid = TRUE,
          gridcolor = grid_col,
          zeroline = FALSE,
          showspikes = TRUE,
          spikemode = "across",
          spikedash = "solid",
          spikethickness = 1
        ),
        yaxis = list(
          title = list(text = pretty_name, standoff = 14, font = list(size = 13, color = axis_txt)),
          automargin = TRUE,
          tickfont = list(size = 11, color = axis_txt),
          showgrid = TRUE,
          gridcolor = grid_col,
          zeroline = FALSE,
          tickvals = y_vals,
          ticktext = y_labs,
          range = y_range
        ),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = paper_bg, bordercolor = accent, font = list(color = axis_txt, size = 12)),
        showlegend = FALSE,
        margin = list(l = 56, r = 16, b = 40, t = 16),
        plot_bgcolor = plot_bg,
        paper_bgcolor = paper_bg
      )
      
      fig %>% plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "autoScale2d","toggleSpikelines","lasso2d","select2d","zoomIn2d","zoomOut2d","resetScale2d"
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = paste0(gsub("[^A-Za-z0-9_]", "_", pretty_name), "_lineplot"),
          width = 1400, height = 800, scale = 1
        )
      )
    })
    
    # External legend (rendered wherever linePlotLegendUI() is placed) --------
    output$legend <- renderUI({
      req(data(), input_states())
      df <- data() %>% dplyr::filter(state_name %in% input_states())
      if (nrow(df) == 0) return(NULL)
      
      countries_in_df <- unique(df$country_name)
      country_colors_base <- c(
        "ARGENTINA" = "#74ACDF",
        "BRAZIL"    = "#3CB371",
        "MEXICO"    = "#E03C31"
      )
      missing_countries <- setdiff(countries_in_df, names(country_colors_base))
      if (length(missing_countries) > 0) {
        fallback_cols <- grDevices::hcl.colors(length(missing_countries), palette = "TealGrn")
        names(fallback_cols) <- missing_countries
        country_colors <- c(country_colors_base, fallback_cols)
      } else country_colors <- country_colors_base
      
      symbols <- c("x", "circle", "diamond", "square", "star", "square-open", "circle-open")
      selected_states <- input_states()
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      
      order_df <- df %>% dplyr::mutate(state_order = match(state_name, selected_states)) %>% dplyr::arrange(state_order)
      country_order <- unique(order_df$country_name)
      
      groups_ui <- lapply(country_order, function(ctry) {
        col <- country_colors[[ctry]] %||% "#666666"
        states_ctry <- selected_states[selected_states %in% (df %>% dplyr::filter(country_name == ctry) %>% dplyr::pull(state_name) %>% unique())]
        items <- lapply(states_ctry, function(st) {
          htmltools::tags$span(class = "legend-item",
                               htmltools::span(class = "legend-swatch", symbol_svg(symbol_map[[st]], col)),
                               htmltools::span(class = "legend-text", st)
          )
        })
        htmltools::div(
          class = "legend-group",
          htmltools::span(class = "legend-country", htmltools::span(class = "country-swatch", style = paste0("background:", col, ";")), ctry),
          htmltools::div(class = "legend-row", items)
        )
      })
      
      htmltools::div(id = paste0(session$ns("line_plot"), "-legend"), class = "legend-wrap", groups_ui)
    })
    
  })
}
