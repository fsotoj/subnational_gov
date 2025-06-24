linePlotModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("line_plot"))
}

linePlotModuleServer <- function(id, data, input_variable, input_states, active_tab) {
  moduleServer(id, function(input, output, session) {
    output$line_plot <- renderPlotly({
      req(data(), input_variable(), input_states(), active_tab() == "graph_tab")
      df <- data() %>%
        filter(state_name %in% input_states()) %>%
        select(country_name, state_name, year, value = all_of(input_variable()))
      
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
          xaxis = list(title = "Year"),
          yaxis = list(title = input_variable()),
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
