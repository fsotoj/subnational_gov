# camara_module_hemi_novalidate.R
# Minimal hemicycle module, no validate()/need(), robust to empty filters
# install.packages(c("dplyr","tibble","purrr","echarts4r"))

library(dplyr)
library(tibble)
library(purrr)
library(echarts4r)

#-------------------------------
# Hemicycle layout (clean wedges)
#-------------------------------
hemicycle_layout <- function(N, layers = NULL, r_min = 0.2, r_max = 1) {
  if (N <= 0) return(tibble(theta = numeric(), r = numeric(), x = numeric(), y = numeric(), layer = integer(), pos = integer()))
  if (is.null(layers)) {
    layers <- dplyr::case_when(
      N <= 60 ~ 4L,
      N <= 150 ~ 6L,
      N <= 300 ~ 8L,
      TRUE ~ 10L
    )
  }
  radii <- seq(r_min, r_max, length.out = layers)
  weights <- radii / sum(radii)
  n_per_layer <- pmax(1L, round(N * weights))
  dif <- N - sum(n_per_layer)
  if (dif != 0) {
    idx <- order(weights, decreasing = (dif > 0))
    for (i in seq_len(abs(dif))) {
      j <- idx[((i - 1) %% length(idx)) + 1]
      n_per_layer[j] <- n_per_layer[j] + sign(dif)
    }
  }
  
  coords <- map2_dfr(
    .x = seq_along(radii),
    .y = n_per_layer,
    ~{
      L <- .x; nL <- .y; r <- radii[L]
      theta <- if (nL == 1) pi/2 else seq(pi, 0, length.out = nL)  # left (π) -> right (0)
      tibble(theta = theta, r = r, x = r * cos(theta), y = r * sin(theta), layer = L)
    }
  ) %>%
    arrange(desc(theta), r)
  
  coords$pos <- seq_len(nrow(coords))
  coords
}

#-------------------------------------------
# Expand seats by party & assign contiguous blocks
#-------------------------------------------
expand_and_assign <- function(df_agg, party_col = "party", seats_col = "seats") {
  if (nrow(df_agg) == 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  df_agg <- df_agg %>%
    mutate(across(all_of(party_col), as.character)) %>%
    filter(.data[[seats_col]] > 0) %>%
    arrange(desc(.data[[seats_col]]), .data[[party_col]])
  
  N <- sum(df_agg[[seats_col]])
  if (N <= 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  lay <- hemicycle_layout(N)
  
  spans <- df_agg %>%
    mutate(
      start = cumsum(dplyr::lag(.data[[seats_col]], default = 0)) + 1L,
      end   = cumsum(.data[[seats_col]])
    )
  
  expanded <- map_dfr(seq_len(nrow(spans)), function(i) {
    tibble(party = spans[[party_col]][i], seat_index = seq.int(spans$start[i], spans$end[i]))
  })
  
  expanded %>%
    left_join(lay %>% select(pos, x, y), by = c("seat_index" = "pos")) %>%
    select(party, x, y)
}

#-------------------------------
# Distinct palette
#-------------------------------
palette_distinct <- function(labels) {
  labs <- unique(as.character(labels))
  n <- length(labs)
  if (n == 0) return(character())
  hues <- seq(15, 375, length.out = n + 1)[-1]  # start at 15° instead of 0° to avoid harsh reds
  cols <- grDevices::hcl(h = hues, c = 100, l = 60)  # higher chroma, mid lightness
  stats::setNames(cols, labs)
}


#-------------------------------
# Module UI
#-------------------------------
camaraUI <- function(id, height = "460px") {
  ns <- NS(id)
  echarts4rOutput(ns("chart"), height = height)
}

#-------------------------------
# Module Server (no validate/need)
#-------------------------------
camaraServer <- function(id,
                         data,                      # static data.frame OR reactive
                         state_r,                   # reactive: state (string) or vector; "" ignored
                         chamber_r,                 # reactive: 1/2 (integer/char)
                         year_r,                    # reactive: year (integer/char)
                         party_col   = "party_name_sub_leg",
                         seats_col   = "total_seats_party_sub_leg",
                         state_col   = "state_name",
                         chamber_filter_col = "chamber_election_sub_leg",
                         year_col    = "year",
                         title_text  = "Chamber composition") {
  
  moduleServer(id, function(input, output, session) {
    
    # wrap static as reactive if needed
    data_r <- if (inherits(data, "reactive")) data else reactive(data)
    
    output$chart <- renderEcharts4r({
      df <- data_r()
      if (!inherits(df, c("data.frame","tbl","tbl_df"))) {
        # draw empty chart with message
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>%
            e_charts(x) %>%
            e_scatter(y) %>%
            e_title("No data (invalid input table)") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # coerce types safely
      cols_needed <- c(state_col, chamber_filter_col, year_col, party_col, seats_col)
      missing <- setdiff(cols_needed, names(df))
      if (length(missing) > 0) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>%
            e_charts(x) %>%
            e_scatter(y) %>%
            e_title(paste("Missing columns:", paste(missing, collapse = ", "))) %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      df[[state_col]]            <- as.character(df[[state_col]])
      df[[party_col]]            <- as.character(df[[party_col]])
      df[[seats_col]]            <- suppressWarnings(as.integer(df[[seats_col]]))
      df[[year_col]]             <- suppressWarnings(as.integer(df[[year_col]]))
      df[[chamber_filter_col]]   <- suppressWarnings(as.integer(df[[chamber_filter_col]]))
      
      # inputs
      sel_states  <- tryCatch(state_r(), error = function(e) NULL)
      if (length(sel_states) == 1 && identical(sel_states, "")) sel_states <- NULL
      sel_states  <- if (!is.null(sel_states)) as.character(sel_states) else NULL
      sel_year    <- suppressWarnings(as.integer(tryCatch(year_r(), error = function(e) NA_integer_)))
      sel_chamber <- suppressWarnings(as.integer(tryCatch(chamber_r(), error = function(e) NA_integer_)))
      
      # filter (same as your working logic)
      dff <- df %>% filter(.data[[seats_col]] != 0)
      if (!is.null(sel_states) && length(sel_states) > 0) {
        dff <- dff %>% filter(.data[[state_col]] %in% sel_states)
      }
      if (!is.na(sel_chamber)) {
        dff <- dff %>% filter(.data[[chamber_filter_col]] == sel_chamber)
      }
      if (!is.na(sel_year)) {
        dff <- dff %>% filter(.data[[year_col]] == sel_year)
      }
      
      if (nrow(dff) == 0) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>%
            e_charts(x) %>%
            e_scatter(y) %>%
            e_title("No data for current selection") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # aggregate like your script
      agg <- dff %>%
        transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        group_by(party) %>%
        summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(seats), party)
      
      # seat expansion & ordering
      pts <- expand_and_assign(agg, party_col = "party", seats_col = "seats")
      if (nrow(pts) == 0) {
        return(
          tibble(x = 0, y = 0, party = "No seats") %>%
            group_by(party) %>%
            e_charts(x) %>% e_scatter(y) %>%
            e_title("No seats to plot") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      seat_totals <- agg %>% select(party)
      pts <- pts %>% mutate(party = factor(party, levels = seat_totals$party))
      
      pal <- palette_distinct(levels(pts$party))
      
      N <- nrow(pts)
      pt_size <- dplyr::case_when(N <= 25 ~ 70, N <= 50 ~ 40, 
                                  N <= 75 ~ 30, N <= 100 ~ 20,
                                  N <= 150 ~ 16, 
                                  TRUE ~ 10)
      
      pts %>%
        group_by(party) %>%
        e_charts(x) %>%
        e_scatter(y, symbolSize = pt_size, itemStyle = list(opacity = 0.95)) %>%
        e_color(unname(pal[levels(pts$party)])) %>%
        e_x_axis(min = -1.1, max = 1.1, show = FALSE) %>%
        e_y_axis(min = 0, max = 1.1, show = FALSE) %>%
        #e_title(title_text) %>%
        e_theme("infographic") %>%
        e_legend(
          show = TRUE,
          textStyle = list(
            color = "#FFFFFF",    # <- legend text color
            fontSize = 14,
            fontWeight = "bold"
          )
        ) %>%
        e_grid(left = '2%', right = '2%', top = '10%', bottom = '10%') %>%
        e_animation(duration = 100)
    })
  })
}
