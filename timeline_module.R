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
      
      
      
      
      early_exit_dates <- df %>% filter(early_exit == 1) %>% 
        pull(end)
      
      df <- df %>% 
        select(!c(year,early_exit)) %>% 
        distinct()
      
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
      plot <- vistime(
        df,
        col.event = "popup_content",
        col.group = "group",
        col.start = "start",
        col.end   = "end",
        title = paste0(gov_type," Terms"),
        optimize_y = TRUE,
        show_labels = FALSE
      )
      
      if (!is.null(early_exit_dates)) {
        
        # Build annotation list
        annotations <- list()
        
        for (date in early_exit_dates) {
          date <- as.POSIXct(date, format = "%Y-%m-%d")
          
          # Add vertical line
          plot <- plot %>%
            add_trace(
              x = c(date, date),
              y = c(0, 9),  # adjust to your actual y range
              type = "scatter",
              mode = "lines",
              line = list(color = "red", width = 2, dash = "dot"),
              showlegend = FALSE,
              inherit = FALSE
            )
          
          # Add rotated label
          annotations <- c(annotations, list(
            list(
              x = date,
              y = 1,  # slightly above top
              xref = "x",
              yref = "paper",
              text = format(date, "%Y-%m-%d"),
              textangle = -90,  # rotate text
              showarrow = FALSE,
              #align = "right",
              xanchor = "right",
              font = list(color = "red", size = 10)
            )
          ))
        }
        
        # Add all annotations at once
        plot <- plot %>% layout(annotations = annotations)
      }
      
      
      plot
      
      
    })
  })
}
