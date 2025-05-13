# vistime_module.R

library(vistime)

vistime_module_ui <- function(id) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("timeline"))
}

vistime_module_server <- function(id, data, gov_type) {
  moduleServer(id, function(input, output, session) {
    output$timeline <- plotly::renderPlotly({
      
      validate(
        need(!is.null(data()) && nrow(data()) > 0, "Please select a country to display the timeline.")
      )
      
      
      # Copy the reactive data to avoid mutating data()
      df <- data()
      
      # Replace NA end dates with today's date
      df$end[is.na(df$end)] <- Sys.Date()
      
      # Define group labels
      group_labels <- c("Left", "Center Left", "Center Right", "Right")
      names(group_labels) <- c(1, 2, 3, 4)
      
      # Create popup content
      df$popup_content <- paste0(
        df$content,
        "<br>Party: ", df$party
      )
      
      # Map group numbers to labels 
      df$group <- factor(df$group, levels = 1:4, labels = group_labels)
      
      # Render the timeline
      vistime(
        df,
        col.event = "popup_content",
        col.group = "group",
        col.start = "start",
        col.end   = "end",
        title = paste0(gov_type," Terms"),
        optimize_y = TRUE,
        show_labels = FALSE
      )
    })
  })
}
