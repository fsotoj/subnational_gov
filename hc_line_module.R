# ---- UI ----
lineHCModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("hc"), height = "80vh")
}

# ---- SERVER ----
# Args are analogous to your Plotly module:
# data(): reactive df with columns country_name, state_name, year, <var>
# dict: tibble with columns variable, pretty_name, type in {"continuous","discrete","percentage"}
# input_variable(): reactive string naming the column in data to plot
# input_states(): reactive character vector of state_name to show
# Ymin(): reactive numeric or NULL
# active_tab(): reactive string; only render if == "graph_tab"
lineHCModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab) {
  moduleServer(id, function(input, output, session) {
    
    `%or%` <- function(x, alt) if (!is.null(x)) x else alt
    
    format_value <- function(values, type) {
      values <- as.double(values)
      if (isTRUE(type == "discrete")) return(format(values, big.mark = ",", scientific = FALSE))
      if (isTRUE(type == "continuous")) return(format(round(values, 2), nsmall = 2, big.mark = ","))
      if (isTRUE(type == "percentage")) return(paste0(format(round(values, 2), nsmall = 2, big.mark = ","), "%"))
      format(values, big.mark = ",", scientific = FALSE)
    }
    
    output$hc <- renderHighchart({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab")
      
      df <- data() %>%
        dplyr::filter(state_name %in% input_states()) %>%
        dplyr::select(country_name, state_name, year, value = dplyr::all_of(input_variable())) %>%
        dplyr::mutate(date_year = as.Date(paste(year, "01", "01", sep = "-"))) %>%
        tidyr::drop_na(date_year)
      
      req(nrow(df) > 0)
      
      # Labels & typing
      pretty_name <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(pretty_name) %>%
        { if (length(.) == 0 || is.na(.)) input_variable() else . }
      var_type <- dict %>% dplyr::filter(variable == input_variable()) %>% dplyr::pull(type) %>%
        { if (length(.) == 0 || is.na(.)) "continuous" else . }
      
      # Colors
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
      
      # Build series list (one per state)
      states <- input_states()
      series_list <- lapply(states, function(st) {
        d <- df %>% dplyr::filter(state_name == st)
        if (nrow(d) == 0) return(NULL)
        ctry <- unique(d$country_name)[1]
        col  <- (country_colors[[ctry]]) %or% "#666666"
        # Highcharts wants milliseconds since epoch
        pts <- purrr::transpose(list(
          x = highcharter::datetime_to_timestamp(d$date_year),
          y = d$value
        ))
        list(
          type = "line",
          name = st,
          data = pts,
          color = col,
          lineWidth = 2,
          marker = list(radius = 3),
          states = list(
            hover    = list(lineWidth = 3, halo = list(size = 6)),
            inactive = list(opacity = 0.18)  # dim others when one is hovered
          ),
          findNearestPointBy = "xy",     # pick the truly closest series/point
          stickyTracking = FALSE,        # don't “stick” to previous series
          enableMouseTracking = TRUE
        )
      })
      series_list <- Filter(Negate(is.null), series_list)
      
      # Y range
      y_min <- Ymin()
      y_max <- max(df$value, na.rm = TRUE)
      y_min_use <- if (is.null(y_min) || is.na(y_min)) NULL else y_min
      
      # Chart
      highcharter::highchart() %>%
        highcharter::hc_chart(zoomType = "x") %>%
        highcharter::hc_xAxis(
          type = "datetime",
          crosshair = list(width = 1, color = "#E5E7EB"),
          tickInterval = 365.25 * 24 * 3600 * 1000 # yearly ticks
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = pretty_name),
          min = y_min_use,
          gridLineColor = "#E5E7EB"
        ) %>%
        # Shared tooltip = show ALL series at the hovered x
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          borderColor = "#FFA92A",
          backgroundColor = "#FFFFFF",
          shadow = FALSE,
          split = FALSE,
          headerFormat = "<span style='font-size:10px;line-height:1;'>Year: <b>{point.key:%Y}</b></span><br/>",
          # single-line per series, small text
          pointFormat = paste0(
            "<span style='font-size:10px;line-height:1;white-space:nowrap;'>",
            "<b>{series.name}</b>: ",
            "{point.y", if (identical(var_type, "percentage")) ":,.2f}%" else ":,.2f}",  # tweak if discrete
            "</span><br/>"
          )
        ) %>%
        highcharter::hc_plotOptions(
          series = list(
            animation = FALSE,
            turboThreshold = 0,
            states = list(
              hover    = list(lineWidth = 3, halo = list(size = 6)),
              inactive = list(opacity = 0.18)   # <- dims non-hovered series
            ),
            findNearestPointBy = "xy",
            stickyTracking = FALSE
          )
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_add_series_list(series_list) %>%
        # Tight layout look
        highcharter::hc_exporting(enabled = FALSE) %>%
        highcharter::hc_credits(enabled = FALSE) 
      
    })
  })
}
