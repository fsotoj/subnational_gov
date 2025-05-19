# UI module
mod_vote_bubbles_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("bubblePlot"), height = "600px")
}

# Server module
mod_vote_bubbles_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$bubblePlot <- renderPlotly({
      
      validate(
        need(!is.null(data()) && nrow(data()) > 0, "Please select a country to display the timeline.")
      )
      
      df <- data()
      
      votes_na <- df %>% filter(is.na(voters_registered_sub)) %>% 
        pull(year)
      
      df <- df %>% filter(!is.na(voters_registered_sub))
      
      group_labels <- c("Left", "Center Left", "Center Right", "Right")
      names(group_labels) <- c(1, 2, 3, 4)
      
      # Map ideology and sort
      df$ideo_party_sub <- factor(df$ideo_party_sub, levels = 1:4, labels = group_labels)
      df <- df %>% arrange(year)
      
      # Define color palette
      ideology_colors <- c(
        "Left" = "#d73027",
        "Center Left" = "#fc8d59",
        "Center Right" = "#91bfdb",
        "Right" = "#4575b4"
      )
      
      # Hover content
      df$popup_winner <- paste0(
        "<b>", df$head_name_sub, "</b><br>",
        "Party: ", df$head_party_sub, "<br>",
        "Winner Votes: ", df$votes_candidate_winner, "<br>",
        "Winner margin: ", df$margin_victory_exe
      )
      
      df$popup_registered <- paste0(
        "Registered voters: ", df$voters_registered_sub, "<br>",
        "Valid votes ratio: ", df$valid_votes_ratio_sub, "<br>",
        "Invalid votes ratio: ", df$invalid_votes_ratio_sub
      )
      
      df$popup_second <- paste0("Second Place Votes: ", df$second_place_votes)
      df$popup_last   <- paste0("Last Place Votes: ", df$last_place_votes)
      
      # Main plot
      plot <- plot_ly(
        data = df,
        x = ~year,
        y = ~votes_candidate_winner,
        type = 'scatter',
        mode = 'markers',
        color = ~ideo_party_sub,
        colors = ideology_colors,
        text = ~popup_winner,
        hoverinfo = "text",
        marker = list(size = 30, opacity = 0.9)
      ) %>%
        add_trace(
          x = ~year,
          y = ~second_place_votes,
          type = 'scatter',
          mode = 'markers',
          marker = list(symbol = 'triangle-up', size = 10, color = 'black'),
          text = ~popup_second,
          hoverinfo = "text",
          name = "Second Place",
          inherit = FALSE
        ) %>%
        add_trace(
          x = ~year,
          y = ~last_place_votes,
          type = 'scatter',
          mode = 'markers',
          marker = list(symbol = 'square', size = 10, color = 'gray'),
          text = ~popup_last,
          hoverinfo = "text",
          name = "Last Place",
          inherit = FALSE
        ) %>%
        add_trace(
          x = ~year,
          y = ~voters_registered_sub,
          type = 'scatter',
          mode = 'markers',
          marker = list(symbol = 'x', size = 20, color = "black"),
          text = ~popup_registered,
          hoverinfo = "text",
          name = "Registered Voters",
          inherit = FALSE
        )
      
      # Add vertical segments
      for (i in seq_len(nrow(df))) {
        plot <- plot %>%
          add_segments(
            x = df$year[i],
            xend = df$year[i],
            y = df$votes_candidate_winner[i],
            yend = df$voters_registered_sub[i],
            line = list(color = 'gray', width = 1),
            showlegend = FALSE,
            hoverinfo = "none",
            inherit = FALSE
          )
      }
      
      plot <- plot %>%
        layout(
          title = "Votes for Winner, Second and Last Place Over Time",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Votes"),
          legend = list(orientation = "h", y = -0.2)
        )
      
      if (length(votes_na) > 0) {
        missing_text <- paste("<b>Years with missing subnational election data:\n", paste(votes_na, collapse = ", "),"</b>")
        
        plot <- plot %>%
          layout(
            annotations = list(
              x = 1,
              y = -0.2,  # slightly below the plot
              text = missing_text,
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              align = "right",
              font = list(size = 12),
              xanchor = "right",
              yanchor = "bottom"
            )
          )
      }
      
      plot
      
    })
  })
}
