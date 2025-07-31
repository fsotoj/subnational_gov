linePlotModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("line_plot"),height = "80vh")
}

linePlotModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab) {
  moduleServer(id, function(input, output, session) {
    output$line_plot <- renderPlotly({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab")
      
      
      
      df <- data() %>%
        filter(state_name %in% input_states()) %>%
        select(country_name, state_name, year, value = all_of(input_variable()))
      

      pretty_name <- dict %>% 
        filter(variable == input_variable()) %>% 
        pull(pretty_name)
      
      req(nrow(df) > 0)
      
      colores_pais <- c(
        "ARGENTINA" = "#74ACDF",
        "BRAZIL" = "#3CB371",
        "MEXICO" = "#E03C31"
      )
      
      colores_pais <- colores_pais[names(colores_pais) %in% unique(df$country_name)]
      
      # Asignar símbolos cíclicos por orden de input_states
      symbols <- c("x", "circle", "diamond", "square","star","square-open","circle-open")
      selected_states <- input_states()
      symbol_map <- setNames(rep(symbols, length.out = length(selected_states)), selected_states)
      
      # Crear gráfico vacío
      fig <- plot_ly()
      
      # Agregar una traza por estado
      for (state in selected_states) {
        df_state <- df %>% filter(state_name == state)
        country <- unique(df_state$country_name)
        color <- colores_pais[[country]]
        
        fig <- fig %>%
          add_trace(
            data = df_state,
            x = ~year,
            y = ~value,
            type = "scatter",
            mode = "lines+markers",
            name = state,
            text = ~paste("State:", state_name, "<br>Year:", year, "<br>Value:", value),
            hoverinfo = "text",
            line = list(color = color, width=3),
            marker = list(
              symbol = symbol_map[[state]],
              size = 12,
              color = color
            )
          )
      }
      
      # Control del eje Y mínimo desde fuera del módulo
      y_min <- Ymin()
      if (is.null(y_min)) {
        y_range <- NULL  # deja que Plotly lo maneje
      } else {
        y_max <- max(df$value, na.rm = TRUE)
        y_range <- c(y_min, y_max * 1.05)  # pequeño margen superior
      }
      
      layout_yaxis <- list(
        title = pretty_name,
        gridcolor = "lightgray",
        titlefont = list(size = 14),
        tickfont = list(size = 10),
        domain = c(0.025, 0.975),
        range = y_range  
      )

      
      fig %>%
        layout(
          # title = list(
          #   text = "Comparison of States across Countries",
          #   x = 0.05,
          #   xanchor = "left",
          #   font = list(size = 18)
          # ),
          xaxis = list(
            tickmode = "array",
            tickvals = seq(1983, 2024, by = 2),
            range = c(1983, 2024),
            ticks = "outside",
            title = "Year",
            showgrid = FALSE,
            tickangle = -45,
            tickfont = list(size = 10),
            domain = c(0.05, 0.95)
          ),
          yaxis = layout_yaxis,
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.25,
            xanchor = "left",
            yanchor = "top",
            font = list(size = 10)
          ),
          margin = list(l = 20, r = 20, b = 20, t = 40),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff"
        )
    })
    
  })
}
