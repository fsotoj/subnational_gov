# ================= UI HELPERS ==================
linePlotModuleUI <- function(id) {
  ns <- NS(id)
  highcharter::highchartOutput(ns("line_plot"), height = "80vh")
}

linePlotLegendUI <- function(id, width = NULL) {
  ns <- NS(id)
  tagList(
    div(
      id = paste0(ns("line_plot"), "-legend"),
      style = "max-width:100%;",
      tags$div(style = "margin:0 0 8px 0; color:#4D4D4D; font-weight:600; font-size:18px;",
               "States"),
      uiOutput(ns("legend"), container = div, inline = FALSE)
    )
  )
}

# =============== SERVER ==================
linePlotModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab) {
  moduleServer(id, function(input, output, session) {
    
    format_value <- function(values, type) {
      values <- as.double(values)
      if (isTRUE(type == "discrete"))   return(format(values, big.mark = ",", scientific = FALSE))
      if (isTRUE(type == "continuous")) return(format(round(values, 2), nsmall = 2, big.mark = ","))
      if (isTRUE(type == "percentage")) return(paste0(format(round(values, 2), nsmall = 2, big.mark = ","), "%"))
      format(values, big.mark = ",", scientific = FALSE)
    }
    
    # Small helper to draw Highcharts-like symbols as inline SVG
    symbol_svg_hc <- function(symbol = "circle", color = "#60A5FA") {
      s <- tolower(symbol)
      svg <- switch(
        s,
        "circle" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" fill="%1$s"/></svg>', color),
        "square" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><rect x="3" y="3" width="8" height="8" fill="%1$s"/></svg>', color),
        "diamond" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><polygon points="7,2 12,7 7,12 2,7" fill="%1$s"/></svg>', color),
        "triangle" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><polygon points="7,2 12,12 2,12" fill="%1$s"/></svg>', color),
        "triangle-down" = sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><polygon points="2,2 12,2 7,12" fill="%1$s"/></svg>', color),
        sprintf('<svg width="14" height="14" viewBox="0 0 14 14" xmlns="http://www.w3.org/2000/svg"><circle cx="7" cy="7" r="5" fill="%1$s"/></svg>', color)
      )
      htmltools::HTML(svg)
    }
    
    output$line_plot <- highcharter::renderHighchart({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab")
      
      df <- data() %>%
        dplyr::filter(state_name %in% input_states()) %>%
        dplyr::select(country_name, state_name, year, value = dplyr::all_of(input_variable())) %>%
        dplyr::mutate(
          date_year = suppressWarnings(as.Date(paste0(year, "-01-01"))),
          x_dt = as.POSIXct(date_year, tz = "UTC"),
          x_ms = as.numeric(x_dt) * 1000
        ) %>%
        tidyr::drop_na(date_year)
      req(nrow(df) > 0)
      
      pretty_name <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(pretty_name) %>%
        { if (length(.) == 0 || is.na(.)) input_variable() else . }
      var_type <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(type) %>%
        { if (length(.) == 0 || is.na(.)) "continuous" else . }
      
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
      
      symbols <- c("circle", "diamond", "square", "triangle", "triangle-down")
      selected_states <- input_states()
      # Map symbols to states (match Highcharts markers)
      symbols <- c("circle", "diamond", "square", "triangle", "triangle-down")
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      
      y_min <- Ymin()
      y_max <- max(df$value, na.rm = TRUE)
      y_max_pad <- as.numeric(y_max) * 1.05
      
      x_min <- min(df$x_ms, na.rm = TRUE)
      x_max <- max(df$x_ms, na.rm = TRUE)
      
      df <- df %>% dplyr::mutate(.formatted_value = format_value(value, var_type))
      
      series_list <- purrr::map(selected_states, function(st) {
        df_state <- df %>% dplyr::filter(state_name == st)
        if (nrow(df_state) == 0) return(NULL)
        country <- unique(df_state$country_name)[1]
        color <- if (!is.null(country_colors[[country]])) country_colors[[country]] else "#666666"
        
        list(
          name = st,
          type = "line",
          data = purrr::transpose(list(
            x = df_state$x_ms,
            y = df_state$value,
            f = df_state$.formatted_value
          )),
          color = color,
          lineWidth = 2,
          marker = list(enabled = TRUE, radius = 4, symbol = symbol_map[[st]]),
          states = list(hover = list(lineWidthPlus = 1)),
          tooltip = list(valueDecimals = ifelse(var_type == "continuous" || var_type == "percentage", 2, 0))
        )
      }) %>% purrr::compact()
      
      grid_col <- "#E5E7EB"
      axis_txt <- "#4D4D4D"
      header_fmt <- "<span style='font-size:11px'>{point.x:%Y}</span><br/>"
      point_fmt  <- "<span style='font-size:9px'><b>{series.name}</b>: {point.f}</span><br/>"
      
      y_label <- switch(
        var_type,
        percentage = list(format = "{value:.2f}%"),
        continuous = list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 2); }")),
        list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 0); }"))
      )
      
      hc <- highcharter::highchart() %>%
        highcharter::hc_chart(
          type = "line",
          animation = FALSE,
          zoomType = NULL,
          panning = FALSE,
          events = list(
            load = highcharter::JS(sprintf("
    function() {
      var chart = this;
      var root = document.getElementById('%s');
      if(!root) return;
      var locked = null;

      function setInactiveAllExcept(name){
        chart.series.forEach(function(s){
          if(!s.visible) return;
          if(s.name === name){
            s.setState('hover');
            if(s.group) s.group.toFront();
            if(s.markerGroup) s.markerGroup.toFront();
          } else {
            s.setState('inactive');
          }
        });
        // Highlight corresponding tag
        root.querySelectorAll('[data-state]').forEach(function(el){
          el.style.opacity = (el.getAttribute('data-state') === name) ? '1.0' : '0.4';
          el.style.boxShadow = (el.getAttribute('data-state') === name) ? 'inset 0 0 0 1px #FFA92A' : 'inset 0 0 0 1px #E6E6E6';
        });
      }

      function clearStates(){
        chart.series.forEach(function(s){
          if(!s.visible) return;
          s.setState('normal');
        });
        root.querySelectorAll('[data-state]').forEach(function(el){
          el.style.opacity = '1';
          el.style.boxShadow = 'inset 0 0 0 1px #E6E6E6';
        });
      }

      chart.series.forEach(function(s){
        s.update({
          events: {
            click: function(){
              var name = this.name;
              if(locked === name){ locked = null; clearStates(); }
              else { locked = name; setInactiveAllExcept(name); }
            }
          }
        });
      });

      root.querySelectorAll('[data-state]').forEach(function(el){
        el.style.cursor = 'pointer';
        el.addEventListener('mouseenter', function(){
          if(locked) return;
          var sName = this.getAttribute('data-state');
          setInactiveAllExcept(sName);
        });
        el.addEventListener('mouseleave', function(){
          if(locked) return;
          clearStates();
        });
        el.addEventListener('click', function(){
          var sName = this.getAttribute('data-state');
          if(locked === sName){ locked = null; clearStates(); }
          else { locked = sName; setInactiveAllExcept(sName); }
        });
      });
    }",
                                           paste0(session$ns("line_plot"), "-legend")
            ))
          )
          
        ) %>%
        highcharter::hc_title(text = NULL) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_xAxis(
          type = "datetime",
          min = x_min, max = x_max,
          tickInterval = 365 * 24 * 3600 * 1000,
          gridLineWidth = 1, gridLineColor = grid_col,
          lineColor = grid_col, tickColor = grid_col,
          labels = list(style = list(color = axis_txt, fontSize = "11px"), rotation = -90)
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = pretty_name, style = list(color = axis_txt, fontSize = "13px")),
          min = if (!is.null(y_min)) y_min else NULL,
          max = if (!is.null(y_min)) y_max_pad else NULL,
          gridLineColor = grid_col,
          labels = y_label
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          headerFormat = header_fmt,
          pointFormat = point_fmt,
          sort = TRUE,
          borderWidth = 1
        ) %>%
        highcharter::hc_plotOptions(
          series = list(
            marker = list(enabled = TRUE),
            states = list(inactive = list(opacity = 0.25)),
            turboThreshold = 0,
            enableMouseTracking = TRUE
          )
        ) %>%
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = paste0(gsub("[^A-Za-z0-9_]", "_", pretty_name), "_lineplot"),
          sourceWidth = 1400,
          sourceHeight = 800
        ) %>%
        highcharter::hc_credits(enabled = FALSE)
      
      if (length(series_list) > 0) {
        hc <- highcharter::hc_add_series_list(hc, series_list)
      }
      
      hc
    })
    
    output$legend <- renderUI({
      req(data(), input_states())
      df <- data() %>% dplyr::filter(state_name %in% input_states())
      if (nrow(df) == 0) return(NULL)
      
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
      
      selected_states <- input_states()
      # Map symbols to states (match Highcharts markers)
      symbols <- c("circle", "diamond", "square", "triangle", "triangle-down")
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      order_df <- df %>%
        dplyr::mutate(state_order = match(state_name, selected_states)) %>%
        dplyr::arrange(state_order)
      country_order <- unique(order_df$country_name)
      
      groups_ui <- lapply(country_order, function(ctry) {
        col <- if (!is.null(country_colors[[ctry]])) country_colors[[ctry]] else "#666666"
        states_ctry <- selected_states[selected_states %in% (df %>% dplyr::filter(country_name == ctry) %>% dplyr::pull(state_name) %>% unique())]
        items <- lapply(states_ctry, function(st) {
          tags$span(
            `data-state` = st,
            style = paste(
              "display:inline-flex;align-items:center;gap:8px;",
              "padding:4px 10px;border-radius:9999px;",
              "background:#FFFFFF;box-shadow:inset 0 0 0 1px #E6E6E6;",
              "color:#4D4D4D;font-size:12px;margin:2px;"
            ),
            htmltools::span(
              style = "display:inline-flex;width:14px;height:14px;align-items:center;justify-content:center;",
              symbol_svg_hc(symbol_map[[st]], col)
            ),
            tags$span(st)
          )
        })
        htmltools::div(
          style = "display:flex;flex-direction:column;gap:6px;margin-bottom:8px;",
          htmltools::span(style = "display:inline-flex;align-items:center;gap:8px;color:#4D4D4D;font-weight:700;font-size:12px;", 
                          htmltools::span(style = paste("width:10px;height:10px;border-radius:2px;display:inline-block;",
                                                        sprintf("box-shadow:0 0 0 1px #DDDDDD;background:%s;", col))),
                          ctry),
          htmltools::div(style = "display:flex;flex-wrap:wrap;gap:6px;", items)
        )
      })
      
      htmltools::div(style = "display:flex;flex-direction:column;gap:8px;", groups_ui)
    })
  })
}
