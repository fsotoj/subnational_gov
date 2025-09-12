
#-------------------------------
# Hemicycle layout (clean wedges)
#-------------------------------
hemicycle_layout <- function(N, layers = NULL, r_min = 0.4, r_max = 1) {
  if (N <= 0) return(tibble(theta = numeric(), r = numeric(), x = numeric(), y = numeric(), layer = integer(), pos = integer()))
  if (is.null(layers)) {
    layers <- dplyr::case_when(
      N <= 30 ~ 3L,
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
  ) %>% arrange(desc(theta), r)
  
  coords$pos <- seq_len(nrow(coords))
  coords
}

#-------------------------------------------
# Expand seats by party & assign contiguous blocks
#  - Respeta el orden de df_agg tal cual viene (no reordena).
#  - Si existe el label de "non-contested", lo coloca al FINAL => derecha.
#-------------------------------------------
expand_and_assign <- function(df_agg,
                              party_col = "party",
                              seats_col = "seats",
                              non_contested_label = "NON-CONTESTED SEATS") {
  if (nrow(df_agg) == 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  
  df_agg <- df_agg %>%
    mutate(across(all_of(party_col), as.character)) %>%
    filter(.data[[seats_col]] > 0)
  
  # separar non-contested y reales, respetando el orden actual
  df_nc   <- df_agg %>% filter(.data[[party_col]] == non_contested_label)
  df_real <- df_agg %>% filter(.data[[party_col]] != non_contested_label)
  
  # combinamos: reales primero en el orden dado, luego non-contested
  df_ord <- bind_rows(df_real, df_nc)
  
  N   <- sum(df_ord[[seats_col]])
  if (N <= 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  lay <- hemicycle_layout(N)
  
  # construir spans según ese orden
  spans <- df_ord %>%
    mutate(
      start = cumsum(dplyr::lag(.data[[seats_col]], default = 0)) + 1L,
      end   = cumsum(.data[[seats_col]])
    )
  
  expanded <- map_dfr(seq_len(nrow(spans)), function(i) {
    tibble(
      party = spans[[party_col]][i],
      seat_index = seq.int(spans$start[i], spans$end[i])
    )
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
  hues <- seq(15, 375, length.out = n + 1)[-1]
  cols <- grDevices::hcl(h = hues, c = 100, l = 60)
  stats::setNames(cols, labs)
}

#-------------------------------
# Module UI
#-------------------------------
camaraUI <- function(id, height = "90vh") {
  ns <- NS(id)
  echarts4rOutput(ns("chart"), height = height)
}

#-------------------------------
# Module Server (no validate/need)
#-------------------------------
camaraServer <- function(id,
                         data,                      # static data.frame OR reactive
                         state_r,                   # reactive: estado (string) o vector; "" se ignora
                         chamber_r,                 # reactive: 1/2 (integer/char)
                         year_r,                    # reactive: year (integer/char)
                         party_col   = "party_name_sub_leg",
                         seats_col   = "total_seats_party_sub_leg",
                         state_col   = "state_name",
                         chamber_filter_col = "chamber_election_sub_leg",
                         year_col    = "year",
                         title_text  = "Chamber composition",
                         previous_name  = "NON-CONTESTED SEATS",
                         previous_color = "rgba(154,160,166,0.40)") {
  
  moduleServer(id, function(input, output, session) {
    
    data_r <- if (inherits(data, "reactive")) data else reactive(data)
    
    output$chart <- renderEcharts4r({
      req(state_r(), chamber_r(), year_r())
      df <- data_r()
      if (!inherits(df, c("data.frame","tbl","tbl_df"))) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>%
            e_charts(x) %>% e_scatter(y) %>%
            e_title("No data (invalid input table)") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # columnas mínimas
      cols_needed <- c(state_col, chamber_filter_col, year_col, party_col, seats_col)
      missing <- setdiff(cols_needed, names(df))
      if (length(missing) > 0) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title(paste("Missing columns:", paste(missing, collapse = ", "))) %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # tipos
      df[[state_col]]          <- as.character(df[[state_col]])
      df[[party_col]]          <- as.character(df[[party_col]])
      df[[seats_col]]          <- suppressWarnings(as.integer(df[[seats_col]]))
      df[[year_col]]           <- suppressWarnings(as.integer(df[[year_col]]))
      df[[chamber_filter_col]] <- suppressWarnings(as.integer(df[[chamber_filter_col]]))
      
      # inputs
      sel_states  <- tryCatch(state_r(), error = function(e) NULL)
      if (length(sel_states) == 1 && identical(sel_states, "")) sel_states <- NULL
      sel_states  <- if (!is.null(sel_states)) as.character(sel_states) else NULL
      sel_year    <- suppressWarnings(as.integer(tryCatch(year_r(), error = function(e) NA_integer_)))
      sel_chamber <- suppressWarnings(as.integer(tryCatch(chamber_r(), error = function(e) NA_integer_)))
      
      # filtro
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
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title("No data for current selection") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # agregación base
      agg_base <- dff %>%
        transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        group_by(party) %>%
        summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop")
      
      # total de cámara
      total_chamber_vec <- suppressWarnings(as.integer(na.omit(dff$total_chamber_seats_sub_leg)))
      total_chamber <- if (length(total_chamber_vec)) {
        tb <- sort(table(total_chamber_vec), decreasing = TRUE)
        cand <- as.integer(names(tb[ tb == max(tb) ]))
        max(cand)
      } else NA_integer_
      
      agg <- agg_base
      if (!is.na(total_chamber)) {
        falta <- total_chamber - sum(agg_base$seats, na.rm = TRUE)
        if (falta > 0) {
          agg <- bind_rows(
            agg_base,
            tibble(party = previous_name, seats = falta)
          )
        }
      }
      
      # ordenar reales por tamaño y dejar NON-CONTESTED al final
      agg <- agg %>%
        mutate(.is_prev = as.integer(party == previous_name)) %>%
        arrange(.is_prev, desc(seats), party) %>%
        select(-.is_prev)
      
      # expansión de asientos (forzando non-contested al final/derecha)
      pts <- expand_and_assign(
        agg,
        party_col = "party",
        seats_col = "seats",
        non_contested_label = previous_name
      )
      if (nrow(pts) == 0) {
        return(
          tibble(x = 0, y = 0, party = "No seats") %>%
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title("No seats to plot") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # series order & palette
      seat_totals <- agg %>% select(party)
      pts <- pts %>% mutate(party = factor(party, levels = seat_totals$party))
      
      pal <- palette_distinct(levels(pts$party))
      if (previous_name %in% names(pal)) {
        pal[previous_name] <- previous_color
      }
      
      # sizes
      N <- nrow(pts)
      pt_size <- dplyr::case_when(
        N <= 30 ~ 60, N <= 50 ~ 40, N <= 100 ~ 30, N <= 150 ~ 20,
        TRUE ~ 16
      )
      
      # plot
      pts %>%
        group_by(party) %>%
        e_charts(x) %>%
        e_scatter(
          y,
          symbolSize = pt_size,
          itemStyle = list(opacity = 0.95)
        ) %>%
        e_color(unname(pal[levels(pts$party)]) #, background = "white") %>%
        ) %>%
        e_x_axis(min = -1.1, max = 1.1, show = FALSE) %>%
        e_y_axis(min = 0, max = 1.1, show = FALSE) %>%
          # Leyenda a la derecha del grid, en la parte inferior (afuera del grid)
        e_legend(
          show = T,
          orient = "horizontal",            # columna
          #type = "scroll",                # scroll si hay muchas series
          #right = "-10%",                      # pegada al borde derecho
          #bottom = "2%",                  # en la parte inferior (no abajo-abajo)
          padding = 0,
          itemGap = 10,
          textStyle = list(
            color = "white",            # texto oscuro sobre fondo blanco
            fontSize = 13,
            fontWeight = "bold"
          )
        ) %>%
        # Deja espacio a la derecha para la leyenda (afuera del grid)
        e_grid(
          left = "4%",
          right = "0%",                  # <--- reserva espacio para la leyenda
          top = "12%",
          bottom = "30%",
          containLabel = FALSE
        ) %>%
        e_animation(duration = 100)
      
      
      
      
    })
  })
}
