linePlotModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("line_plot"))
}

linePlotModuleServer <- function(id, data, dict, input_variable, input_states, active_tab) {
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
      
      # Colores fijos para los países específicos
      colores_pais <- c(
        "ARGENTINA" = "#74ACDF",  # celeste
        "BRAZIL" = "#3CB371",     # verde
        "MEXICO" = "#E03C31"      # rojo
      )
      
      # Filtrar solo los colores para los países presentes en df
      colores_pais <- colores_pais[names(colores_pais) %in% unique(df$country_name)]

      plot_ly(df, x = ~year, y = ~value,
              color = ~country_name,
              colors = colores_pais,
              split = ~state_name,
              type = "scatter", mode = "lines+markers",
              text = ~paste0(state_name)) %>%
        layout(
          title = "Comparison of States across Countries",
          xaxis = list(
            tickmode = "array",
            tickvals = 1983:2024,
            range = c(1983, 2024),
            ticks = "outside",     # Make ticks point outward
            title = "Year"
          ),
          yaxis = list(title = pretty_name),
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.2,
            xanchor = "left",
            yanchor = "top",
            traceorder = "normal",
            font = list(size = 10)
          ),
          margin = list(b = 80)
        )
    })
  })
}
