# modules/table_module.R
tableModuleUI <- function(id, height = "75vh") {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("table"), height = height)
}



tableModuleServer <- function(
    id,
    datasets,
    db_selector,
    country,
    state,
    years,
    active_tab      = NULL,
    force_styles    = TRUE,
    minWidth        = 120,
    maxWidth        = 600,
    enable_tooltips = TRUE,
    long_txt_thr    = 40,   # umbral para activar tooltip por columna de texto
    rows_vrt        = 250,  # usado solo si tu reactable soporta 'virtualized'
    rows_pag        = 1500  # no se usa (sin paginación)
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Compat: sólo usar args existentes en tu versión de reactable
    has_arg <- function(fn, arg) arg %in% names(formals(fn))
    has_virtualized <- has_arg(reactable::reactable, "virtualized")
    
    # ¿Estamos en la pestaña de datos?
    is_data_tab <- shiny::reactive({
      if (is.null(active_tab)) TRUE else identical(active_tab(), "data_tab")
    })
    
    # Dataset activo por selección
    ds_active <- shiny::reactive({
      db <- db_selector()
      if (!length(db) || is.null(db)) return(NULL)
      datasets[[db]]
    })
    
    # Filtro de datos (incluye lógica especial para CFTDFLD)
    data_sel <- shiny::reactive({
      if (!is_data_tab()) return(tibble::tibble())
      db  <- db_selector(); df <- ds_active()
      cty <- country();      yrs <- years()
      if (is.null(db) || is.null(df) || is.null(cty) || is.null(yrs)) return(tibble::tibble())
      
      if (identical(db, "CFTDFLD")) {
        allowed <- c("CAPITAL FEDERAL", "TIERRA DEL FUEGO")
        st_in   <- tryCatch(state(), error = function(e) NULL)
        st_use  <- if (is.null(st_in) || !length(st_in) || !all(st_in %in% allowed)) allowed else st_in
        
        out <- df
        if ("country_name" %in% names(df)) {
          out <- dplyr::filter(out, .data$country_name == cty)
        }
        dplyr::filter(out, .data$state_name %in% st_use, .data$year %in% yrs)
        
      } else if (!identical(db, "NED")) {
        st <- state(); if (is.null(st) || !length(st)) return(tibble::tibble())
        dplyr::filter(df, .data$country_name == cty, .data$state_name == st, .data$year %in% yrs)
        
      } else {
        dplyr::filter(df, .data$country_name == cty, .data$year %in% yrs)
      }
    })
    
    # Estilos oscuros (scoped) una sola vez
    if (isTRUE(force_styles)) {
      session$onFlushed(function() {
        shiny::insertUI(
          selector = "head", where = "beforeEnd",
          ui = htmltools::tags$style(htmltools::HTML(paste0(
            "/* Scoped styles for ", ns("table"), " */",
            "#", ns("table"), " .reactable { background-color:#1e1e1e !important; color:#d4d4d4 !important; border:1px solid #333 !important; }",
            "#", ns("table"), " .reactable .rt-th { background-color:#2d2d30 !important; color:#e5e5e5 !important; border-bottom:1px solid #444 !important; font-weight:600 !important; }",
            "#", ns("table"), " .reactable .rt-tr.-odd { background-color:#252526 !important; }",
            "#", ns("table"), " .reactable .rt-tr.-even { background-color:#1e1e1e !important; }",
            "#", ns("table"), " .reactable .rt-tr:hover { background-color:#094771 !important; color:#ffffff !important; }",
            "#", ns("table"), " .reactable .rt-td, #", ns("table"), " .reactable .rt-td-inner { background:transparent !important; color:#d4d4d4 !important; border-bottom:1px solid #333 !important; }"
          )))
        )
      }, once = TRUE)
    }
    
    # Celda segura (siempre devuelve un tag)
    safe_cell_with_title <- function(value) {
      if (is.null(value) || length(value) == 0 || (is.na(value) && !is.character(value))) {
        return(htmltools::span(""))
      }
      v <- as.character(value); if (is.na(v)) v <- ""
      htmltools::span(title = v, v)
    }
    
    # Render
    output$table <- reactable::renderReactable({
      df <- data_sel()
      if (!is_data_tab() || nrow(df) == 0) {
        return(reactable::reactable(
          tibble::tibble(`No data` = "Select country/state/years in Data tab"),
          pagination = FALSE, highlight = FALSE, striped = FALSE,
          theme = reactable::reactableTheme(
            color = "#d4d4d4", backgroundColor = "#1e1e1e", borderColor = "#333"
          )
        ))
      }
      
      # Normalizaciones
      names(df) <- make.names(names(df), unique = TRUE)
      
      # Quitar columnas todo-NA
      all_na_cols <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
      if (length(all_na_cols)) {
        df <- df[, setdiff(names(df), all_na_cols), drop = FALSE]
      }
      if (ncol(df) == 0) {
        return(reactable::reactable(
          tibble::tibble(`No data` = "All columns are NA for this selection"),
          pagination = FALSE
        ))
      }
      
      # list-columns -> character
      if (any(vapply(df, is.list, logical(1)))) {
        df <- dplyr::mutate(df, dplyr::across(
          where(is.list),
          ~ vapply(., function(x) if (is.null(x)) NA_character_ else paste(as.character(x), collapse = ", "), character(1))
        ))
      }
      
      # Columnas con tooltip (solo texto largo)
      char_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      num_cols  <- names(df)[vapply(df, is.numeric, logical(1))]
      
      tooltip_cols <- character(0)
      if (isTRUE(enable_tooltips) && length(char_cols)) {
        max_nchar <- vapply(df[char_cols], function(x) {
          x <- if (is.factor(x)) as.character(x) else x
          suppressWarnings(max(nchar(x), na.rm = TRUE))
        }, numeric(1))
        tooltip_cols <- char_cols[max_nchar >= long_txt_thr]
      }
      
      columns_defs <- list()
      if (length(tooltip_cols)) {
        columns_defs <- c(columns_defs,
                          stats::setNames(
                            lapply(tooltip_cols, function(nm) reactable::colDef(cell = safe_cell_with_title, html = TRUE)),
                            tooltip_cols
                          )
        )
      }
      if (length(num_cols)) {
        columns_defs <- c(columns_defs,
                          stats::setNames(lapply(num_cols, function(nm) reactable::colDef(align = "right")), num_cols)
        )
      }
      
      # Perfil de vista: sin paginación; hover & resize activos
      use_virtualized <- has_virtualized && (nrow(df) > rows_vrt)
      
      args <- list(
        data       = df,
        height     = "75vh",
        striped    = TRUE,
        highlight  = TRUE,
        compact    = TRUE,
        resizable  = TRUE,
        wrap       = FALSE,
        pagination = FALSE,
        defaultColDef = reactable::colDef(
          minWidth = minWidth, maxWidth = maxWidth, align = "left",
          style = list(
            whiteSpace="nowrap", overflow="hidden", textOverflow="clip",
            color="#d4d4d4", backgroundColor="transparent", fontSize="13px"
          ),
          headerStyle = list(
            backgroundColor="#2d2d30", color="#e5e5e5",
            borderBottom="1px solid #444", fontWeight="600"
          )
        ),
        theme = reactable::reactableTheme(
          color="#d4d4d4", backgroundColor="#1e1e1e", borderColor="#333",
          stripedColor="#252526", highlightColor="#094771",
          inputStyle = list(backgroundColor="#1e1e1e", color="#e5e5e5", borderColor="#444"),
          cellPadding="8px 10px",
          style=list(fontFamily="system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif")
        )
      )
      if (length(columns_defs)) args$columns <- columns_defs
      if (has_virtualized) args$virtualized <- use_virtualized
      
      do.call(reactable::reactable, args)
    })
  })
}
