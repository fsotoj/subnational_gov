server <- function(input, output, session) {
  
  # ==== 0) CONSTANTS / INITIALIZATION =======================================
  default_states <- c("ARGENTINA-CAPITAL FEDERAL", "BRAZIL-DISTRITO FEDERAL", "MEXICO-CDMX")
  current_tab   <- reactive({ input$tabs })
  
  # Initialize JSTree with default selection
  observeEvent(session, {
    session$sendCustomMessage(
      "jstree_data",
      list(
        data = jstree_json_data,
        default_selected = default_states
      )
    )
  })
  

  # Send to the client (message name is up to you; example: "jstree_vars_data")
  observeEvent(session, {
    session$sendCustomMessage(
      "jstree_vars_data",
      list(
        data = jstree_json_vars, 
        default_selected = list("SEED-Valid Votes") # optional
      )
    )
  })
  
  
  # ==== 1) GLOBAL REACTIVES ==================================================
  # -- 1.0) states JSTree graph ----------------------
  selected_states_vector <- reactive({
    if (is.null(input$selected_nodes) || input$selected_nodes == "[]") return(character(0))
    nodes <- jsonlite::fromJSON(input$selected_nodes)
    states_selected_ids <- nodes[grepl("-", nodes)]
    sapply(strsplit(states_selected_ids, "-"), function(x) x[2])
  })
  
  
  # -- 1.0) vars JSTree map ----------------------
  selected_vars_vector <- reactive({
    x <- input$selected_nodes_vars
    if (is.null(x) || identical(x, "[]")) return(NULL)
    ids <- jsonlite::fromJSON(x)
    if (!length(ids)) return(NULL)
    
    parts <- strsplit(ids[1], "-", fixed = TRUE)[[1]]
    # SLED-Lower-Pretty... / SLED-Upper-Pretty...
    if (identical(parts[1], "SLED") && length(parts) >= 3 && parts[2] %in% c("Lower","Upper")) {
      
      n_chamber <- case_when(parts[2] == "Lower" ~ 1,
                             parts[2] == "Upper" ~ 2)
      
      dict %>% 
        filter(pretty_name == paste(parts[3:length(parts)], collapse = "-")) %>% 
        pull(variable) %>% 
        paste0(.,"_",n_chamber)
      
    } else {
      # Generic: DATASET-Pretty...
      
      dict %>% filter(pretty_name == paste(parts[2:length(parts)], collapse = "-")) %>% pull(variable)
      
    }
  })
  
  
  # -- 1.0) Normalized variable names (map & graph) ---------------------------

  var_normal_name2 <- reactive({
    req(input$var_sel2)
    dict %>% dplyr::filter(pretty_name == input$var_sel2) %>% dplyr::pull(variable)
  })
  
  # -- 1.0) Data for the map (filtered by country & year) ---------------------
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    geom_filtered <- geom %>% dplyr::filter(country_name == input$country_sel)
    data_filtered <- data %>% dplyr::filter(country_name == input$country_sel, year == input$year_sel)
    dplyr::left_join(geom_filtered, data_filtered, by = "country_state_code")
  })
  
  
  # ==== 1.1) DATA TAB SELECTORS (based on active dataset) ====================
  active_df <- reactive({
    req(input$db_sel)
    switch(input$db_sel,
           SED = SED, SEED = SEED, SLED = SLED, NED = NED, CFTDFLD = CFTDFLD)
  })
  has_col <- function(df, nm) nm %in% names(df)
  uniq_sorted <- function(x) sort(unique(stats::na.omit(x)))
  
  available_countries <- reactive({
    df <- active_df()
    if (!has_col(df, "country_name")) character(0) else uniq_sorted(df$country_name)
  })
  available_states <- reactive({
    df <- active_df()
    if (!has_col(df, "state_name")) character(0) else {
      if (!length(input$country_sel2)) uniq_sorted(df$state_name)
      else uniq_sorted(df$state_name[df$country_name == input$country_sel2])
    }
  })
  available_years_scoped <- reactive({
    df <- active_df()
    if (!has_col(df, "year")) return(integer(0))
    if (has_col(df, "country_name") && !is.null(input$country_sel2) && nzchar(input$country_sel2)) {
      df <- df[df$country_name == input$country_sel2, , drop = FALSE]
    }
    if (has_col(df, "state_name") && !is.null(input$state_sel2) && nzchar(input$state_sel2)) {
      df <- df[df$state_name == input$state_sel2, , drop = FALSE]
    }
    uniq_sorted(df$year)
  })
  
  # Update Data Tab selectors when active dataset changes
  observeEvent(active_df(), {
    req(current_tab() == "data_tab")
    shinyjs::disable("country_sel2"); shinyjs::disable("state_sel2"); shinyjs::disable("years")
    
    # Countries
    countries <- available_countries()
    new_country <- if (length(input$country_sel2) && input$country_sel2 %in% countries) input$country_sel2 else countries[1]
    updateSelectInput(session, "country_sel2", choices = countries, selected = new_country)
    
    # States
    states <- isolate(available_states())
    if (length(states)) {
      new_state <- if (length(input$state_sel2) && input$state_sel2 %in% states) input$state_sel2 else states[1]
      updateSelectInput(session, "state_sel2", choices = states, selected = new_state)
      shinyjs::enable("state_sel2")
    } else {
      updateSelectInput(session, "state_sel2", choices = character(0), selected = character(0))
      shinyjs::disable("state_sel2")
    }
    
    # Years (pickerInput)
    yrs <- available_years_scoped()
    old_sel <- isolate(input$years)
    new_sel <- if (length(old_sel) && all(old_sel %in% yrs)) old_sel else yrs
    shinyWidgets::updatePickerInput(session, "years", choices = yrs, selected = new_sel)
    
    shinyjs::enable("country_sel2"); shinyjs::enable("years")
  })
  
  # Refresh states when country changes (Data Tab)
  observeEvent(input$country_sel2, {
    req(current_tab() == "data_tab")
    shinyjs::disable("state_sel2")
    states <- available_states()
    if (length(states)) {
      new_state <- if (length(input$state_sel2) && input$state_sel2 %in% states) input$state_sel2 else states[1]
      updateSelectInput(session, "state_sel2", choices = states, selected = new_state)
      shinyjs::enable("state_sel2")
    } else {
      updateSelectInput(session, "state_sel2", choices = character(0), selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  # Refresh years (Data Tab) on dataset/country/state changes
  observeEvent(list(active_df(), input$country_sel2, input$state_sel2), {
    req(current_tab() == "data_tab")
    yrs <- available_years_scoped()
    old_sel <- isolate(input$years)
    new_sel <- if (length(old_sel) && all(old_sel %in% yrs)) old_sel else yrs
    shinyjs::disable("years")
    shinyWidgets::updatePickerInput(session, "years", choices = yrs, selected = new_sel)
    shinyjs::enable("years")
  }, ignoreInit = TRUE)
  
  
  # ==== 1.2) FILTERED DATA FOR TABLE MODULE (outside module) =================
  data_filtered <- reactive({
    df <- active_df()
    if (has_col(df, "country_name") && length(input$country_sel2)) {
      df <- df[df$country_name == input$country_sel2, , drop = FALSE]
    }
    if (has_col(df, "state_name") && length(input$state_sel2)) {
      df <- df[df$state_name == input$state_sel2, , drop = FALSE]
    }
    if (has_col(df, "year") && length(input$years)) {
      df <- df[df$year %in% input$years, , drop = FALSE]  # pickerInput → membership
    }
    df
  })
  
  
  # ==== 1.3) CAMERA TAB SELECTORS (SLED-driven) ==============================
  # UI renderers (camera-only)
  output$country_selector_camera <- renderUI({
    selectInput(
      "country_sel_camera", "Country",
      choices  = sort(unique(SLED$country_name)),
      selected = "ARGENTINA"
    )
  })
  
  output$state_selector_camera <- renderUI({
    req(input$country_sel_camera)
    choices <- SLED |>
      dplyr::filter(country_name == input$country_sel_camera) |>
      dplyr::pull(state_name) |>
      unique() |>
      sort()
    selectInput(
      "state_sel_camera", "State",
      choices  = choices,
      selected = if (length(choices)) choices[1] else NULL
    )
  })
  
  output$chamber_selector_camera <- renderUI({
    selectInput(
      "chamber_sel_camera", "Chamber",
      choices = c("Lower chamber" = 1, "Upper chamber" = 2),
      selected = 1
    )
  })
  
  # Year scoping helpers (camera)
  sled_years_scoped_camera <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    sort(unique(df$year))
  })
  
  # Keep year slider in sync (camera)
  observeEvent(list(current_tab(), input$country_sel_camera, input$state_sel_camera), {
    req(current_tab() == "camera")
    yrs <- sled_years_scoped_camera()
    shinyWidgets::updateSliderTextInput(
      session, "year_sel_camera",
      choices  = as.character(yrs),
      selected = if (!is.null(input$year_sel_camera) &&
                     input$year_sel_camera %in% as.character(yrs)) {
        input$year_sel_camera
      } else {
        as.character(tail(yrs, 1))
      }
    )
  }, ignoreInit = FALSE)
  
  # Available chambers (scoped)
  available_chambers_camera <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
      df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    }
    ch <- sort(unique(suppressWarnings(as.integer(df$chamber_election_sub_leg))))
    ch <- ch[!is.na(ch) & ch %in% c(1L, 2L)]
    ch
  })
  
  .label_chambers <- function(v) {
    labs <- ifelse(v == 1L, "Lower chamber", ifelse(v == 2L, "Upper chamber", as.character(v)))
    stats::setNames(v, labs)
  }
  
  # Keep chamber selector in sync
  observeEvent(
    list(current_tab(), input$country_sel_camera, input$state_sel_camera, input$year_sel_camera),
    {
      req(current_tab() == "camera")
      ch <- available_chambers_camera()
      
      if (!length(ch)) {
        shinyjs::disable("chamber_sel_camera")
        updateSelectInput(session, "chamber_sel_camera",
                          choices = setNames(numeric(0), character(0)),
                          selected = character(0))
        return(invisible(NULL))
      }
      
      shinyjs::enable("chamber_sel_camera")
      choices_named <- .label_chambers(ch)
      
      old_sel <- suppressWarnings(as.integer(isolate(input$chamber_sel_camera)))
      new_sel <- if (length(old_sel) && !is.na(old_sel) && old_sel %in% ch) old_sel else ch[1]
      
      updateSelectInput(session, "chamber_sel_camera",
                        choices = choices_named,
                        selected = new_sel)
    },
    ignoreInit = FALSE
  )
  
  
  # ==== 1.4) FILTERED DATA FOR CAMERA =======================================
  sled_cam_filtered <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$chamber_sel_camera)) {
      df <- df[df$chamber_election_sub_leg == as.integer(input$chamber_sel_camera), , drop = FALSE]
    }
    if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
      df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    }
    df
  })
  
  
  # ==== 2) MODALS / MESSAGES ================================================
  observe({
    showModal(modalDialog(
      title = "About the Subnational Politics Project (SPP)",
      HTML("
      <p style='text-align:justify;'>
        The Subnational Politics Project (SPP) is part of a broader research project designed to compile, generate, and disseminate systematic, transparent, and publicly accessible data on subnational political institutions, subnational political processes, and subnational electoral outcomes in Latin America.
      </p>
      <p style='text-align:justify;'>
        The primary objective of the project is to create a centralized and standardized data infrastructure that facilitates both in-depth within-country analyses and cross-national comparative research on subnational political dynamics.
      </p>
      <p style='text-align:justify;'>
        By providing longitudinal and spatially disaggregated data, the SPP seeks to support empirical scholarship on a wide range of topics, including federalism, decentralization, party competition, electoral accountability, and territorial governance.
      </p>
      <p style='text-align:justify;'>
        This application provides direct access to the SPP databases and interactive tools for exploring subnational political dynamics. As of September 2025, the project includes comprehensive databases for three federal countries in Latin America—Argentina, Brazil, and Mexico—covering the period from the 1980s through 2024.
      </p>
      <hr style='border-color:#17a2b8;'/>
      <h4 style='color:#17a2b8;'>References</h4>
      <p style='font-size: 0.9em; color: #bbb;'>
        Giraudy, Agustina. 2025. “Codebook Subnational Politics Project (SPP) (v. 1).” <em>Subnational Politics Project</em>. 
        <a href='https://doi.org/doi:10.7910/DVN/IBSJO2' target='_blank'>https://doi.org/doi:10.7910/DVN/IBSJO2</a>.
      </p>
    "),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("Close")
    ))
  })
  
  

  
  observeEvent(input$show_db_img, ignoreInit = TRUE, {
    showModal(modalDialog(
      #title = "Database structure", 
      size = "l", easyClose = TRUE, footer = NULL,
      tags$img(src = "databases_spp.jpg", style = "width:100%; height:auto;")
    ))
  })
  
  observeEvent(input$show_vars_img, ignoreInit = TRUE, {
    showModal(modalDialog(
      #title = "Variables description",
      size = "l", easyClose = TRUE, footer = NULL,
      tags$img(src = "variables_database.jpg", style = "width:100%; height:auto;")
    ))
  })
  

  
  # “No data” message (map)
  output$no_data_message <- renderText("⚠ No data available for this country, variable and year.")
  
  
  # ==== 3) MAP CAPTURE CONTROLS =============================================
  observeEvent(input$captureMapBtn, {
    session$sendCustomMessage(
      type = paste0("captureMap", "map1"),
      message = list(
        filename = paste0("map_", input$country_sel, input$year_sel, "_", selected_vars_vector(), ".png"),
        scale = 2
      )
    )
  })
  observe({
    if (current_tab() == "map_tab") {
      shinyjs::show("captureMapBtn")
    } else {
      shinyjs::hide("captureMapBtn")
    }
  })
  
  
  # ==== 4) DYNAMIC UI (SELECTORS) ===========================================
  # -- 4.1) States selector (graph - multi-country) --------------------------
  output$state_selector <- renderUI({
    req(current_tab() == "graph_tab")
    states_choices <- data %>%
      dplyr::distinct(country_name, state_name) %>%
      dplyr::arrange(country_name, state_name) %>%
      dplyr::group_by(country_name) %>%
      dplyr::group_split()
    
    choices_list <- lapply(states_choices, function(group) {
      stats::setNames(as.list(group$state_name), group$state_name)
    })
    countries <- sapply(states_choices, function(g) unique(g$country_name))
    names(choices_list) <- countries
    
    shinyWidgets::pickerInput(
      inputId = "state_sel",
      label   = "Select states from any country:",
      choices = choices_list,
      selected = c("CAPITAL FEDERAL", "DISTRITO FEDERAL", "CDMX"),
      multiple = TRUE,
      options  = list(`actions-box` = TRUE, `live-search` = TRUE, `selectedTextFormat` = "values")
    )
  })
  
  # -- 4.2) Dataset selector (data_tab) --------------------------------------
  output$db_selector <- renderUI({
    selectInput("db_sel", label = "Select a database to view:",
                choices = c("NED", "SED", "SEED", "SLED", "CFTDFLD"))
  })
  

  # -- 4.4) Country/Year selectors (map_tab) ---------------------------------
  output$country_selector <- renderUI({
    selectInput("country_sel", "Country",
                choices = c("Select a country", unique(data$country_name)),
                selected = "MEXICO")
  })
  output$year_selector <- renderUI({
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel", label = "Year",
      choices  = as.character(seq(1983, 2024, 1)),
      grid     = TRUE, width = "90%", animate = TRUE, selected = 1999
    )
  })
  
  # -- 4.5) Camera year selector (UI; updated above) -------------------------
  output$year_selector_camera <- renderUI({
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel_camera", label = "Year",
      choices  = as.character(seq(1983, 2024, 1)),
      grid     = TRUE, width = "90%",
      selected = 2019,
      animate  = shiny::animationOptions(
        interval = 1000,
        loop = FALSE
      )
    )
  })
  
  
  # ==== 5) SHOW / HIDE CONTROLS BY TAB ======================================
  # “No data” visibility (map)
  observe({
    req(current_tab() == "map_tab", data_map())
    if (nrow(data_map()) == 0 || all(is.na(data_map()[[selected_vars_vector()]]))) {
      shinyjs::show("no_data_message")
    } else {
      shinyjs::hide("no_data_message")
    }
  })
  
  # Batch toggle helpers
  .combine_selector <- function(ids) if (length(ids)) paste0("#", ids, collapse = ", ") else NULL
  .batch_show <- function(ids) { sel <- .combine_selector(ids); if (!is.null(sel)) shinyjs::show(selector = sel) }
  .batch_hide <- function(ids) { sel <- .combine_selector(ids); if (!is.null(sel)) shinyjs::hide(selector = sel) }
  
  # Everything we may toggle anywhere in the app
  ALL_TOGGLES <- c(
    # map/graph controls
    "country_selector","var_sel","var_sel2","var_description_map","var_description_graph","jstree_container",
    "state_selector","jstree_vars_container",
    # data-tab selectors
    "country_sel2","state_sel2","db_selector","years",
    # legacy camera selector
    "camera_selector",
    # camera-tab selectors (SLED driven)
    "country_selector_camera","state_selector_camera","chamber_selector_camera","year_selector_camera"
  )
  
  # Given a tab, return vector of IDs to show
  .ids_to_show_for_tab <- function(tab) {
    switch(tab,
           "map_tab"   = c("country_selector","var_sel","var_description_map","jstree_vars_container"),
           "graph_tab" = c("var_sel2","var_description_graph","state_selector","jstree_container"),
           "data_tab"  = c("country_sel2","state_sel2","db_selector","years"),
           "camera"    = c("country_selector_camera","state_selector_camera","chamber_selector_camera","year_selector_camera"),
           character(0)
    )
  }
  
  # Main visibility controller
  observeEvent(current_tab(), {
    to_show <- .ids_to_show_for_tab(current_tab())
    to_hide <- setdiff(ALL_TOGGLES, to_show)
    .batch_hide(to_hide)
    .batch_show(to_show)
  }, ignoreInit = FALSE)
  
  # ==== 6) TEXT BLOCKS (DB & CAMERA) ========================================
  # -- 6.1) Database info text (data_tab) ------------------------------------
  output$texto_db <- renderUI({ 
    req(input$db_sel) 
    texto <- switch(
      input$db_sel, 
      "NED"  = "<b>National Executive Databse:</b> Data on national executive branches per country.",
      "SED"  = "<b>Subnational Executive Database:</b> Data on subnational executive branches per state/province, per country.",
      "SEED" = "<b>Subnational Executive Elections Database:</b> Data on electoral results for executive branch.",
      "SLED" = "<b>Subnational Legislative Elections Database:</b> Data on subnational executive elections by state/province and country. It also includes institutional and electoral information on state- or provincial-level legislatures.",
      "No data"
    ) 
    HTML(texto) 
  })
  
  # -- 6.2) Camera info text (camera tab) ------------------------------------
  output$text_camera <- renderUI({
    req(current_tab() == "camera", sled_cam_filtered(), input$state_sel_camera)
    df <- sled_cam_filtered()
    
    # helpers --------------------------------------------------------
    first_or_summary <- function(x) {
      vals <- unique(na.omit(x))
      if (length(vals) == 0) return("—")
      if (length(vals) == 1) return(as.character(vals))
      paste0("varies (", paste(sort(vals), collapse = ", "), ")")
    }
    
    map_renewal <- function(x) {
      labs <- c(
        `1` = "Staggered every 2 years",
        `2` = "Full renewal"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) return("—")
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }
    
    map_system <- function(x) {
      labs <- c(
        `1` = "Proportional Representation",
        `2` = "Simple Majority",
        `3` = "Mixed (PR + Simple Majority)",
        `4` = "Mixed (PR with predefined districts)"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) return("—")
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }
    
    # compute --------------------------------------------------------
    total_chamber  <- first_or_summary(df$total_chamber_seats_sub_leg)
    seats_contest  <- first_or_summary(df$total_seats_in_contest_sub_leg)
    renewal_type   <- map_renewal(df$renewal_type_sub_leg)
    elec_system    <- map_system(df$electoral_system_sub_leg)
    n_parties_cont <- first_or_summary(df$num_parties_election_contest_sub_leg)
    
    enp_vals <- sort(unique(na.omit(as.numeric(df$enp_leg_sub))))
    enp_txt <- if (!length(enp_vals)) {
      "—"
    } else if (length(enp_vals) == 1) {
      sprintf("%.2f", enp_vals)
    } else {
      sprintf("varies (%.2f–%.2f)", min(enp_vals), max(enp_vals))
    }
    
    # assemble -------------------------------------------------------
    HTML(paste0(
      "<b>Chamber seats (total):</b> ", total_chamber, "<br/>",
      "<b>Seats in contest:</b> ", seats_contest, "<br/>",
      "<b>Renewal type:</b> ", renewal_type, "<br/>",
      "<b>Electoral system:</b> ", elec_system, "<br/>",
      "<b>Parties contesting:</b> ", n_parties_cont, "<br/>",
      "<b>ENPL:</b> ", enp_txt
    ))
  })
  
  
  
  # ==== 7) VARIABLE DESCRIPTIONS (map & graph) ==============================
  output$var_description_map <- renderText({
    req(current_tab() == "map_tab", selected_vars_vector())
    clean_var <- sub("_[12]$", "", selected_vars_vector())  # remove "_1" or "_2" at the end
    
    var_info <- dict %>% dplyr::filter(variable == clean_var) %>% dplyr::slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  output$var_description_graph <- renderText({
    req(current_tab() == "graph_tab", input$var_sel2)
    var_info <- dict %>% dplyr::filter(pretty_name == input$var_sel2) %>% dplyr::slice(1)
    paste0(var_info$pretty_name[1], ": ", var_info$description[1])
  })
  
  # Keep default options of var_sel and var_sel2 in sync
  observe({
    updateSelectInput(
      session, "var_sel",
      choices  = dict$pretty_name[dict$viewable_map == 1],
      selected = "Valid Votes"
    )
    updateSelectInput(
      session, "var_sel2",
      choices  = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Voter Turnout Percentage"
    )
  })
  
  
  # ==== 8) NATIONAL LEADER SUMMARY (map) ====================================
  output$leader_summary <- renderUI({
    req(data_map(), input$country_sel)
    leader_info <- data_map() %>%
      sf::st_drop_geometry() %>%
      dplyr::select(
        name_head_nat_exe, sex_head_nat_exe, head_party_nat_exe,
        ideo_party_nat_exe, 
        early_exit_nat_exe,
        year_election_nat_exe, year
      ) %>%
      dplyr::slice(1)
    
    country_name <- stringr::str_to_title(input$country_sel)
    leader_name <- stringr::str_to_title(leader_info$name_head_nat_exe)
    sex <- ifelse(leader_info$sex_head_nat_exe == 1, "female", "male")
    leader_party <- stringr::str_to_title(leader_info$head_party_nat_exe)
    ideologies <- c("Left", "Center Left", "Center Right", "Right")
    ideology_text <- ifelse(leader_info$ideo_party_nat_exe %in% 1:4,
                            ideologies[leader_info$ideo_party_nat_exe], "Unknown")
    
    text <- glue::glue(
      "<div>",
      " In the year <strong>{leader_info$year}</strong> the national leader in <strong>{country_name}</strong> ",
      "was <strong>{leader_name}</strong>, a {sex} politician affiliated with the ",
      "<strong>{leader_party}</strong> party, which leans towards the ",
      "<strong>{ideology_text}</strong> on the ideological spectrum.",
      "</div>"
    )
    HTML(text)
  })
  
  aboutSPPServer("about")
  
  
  # ==== 9) MODULES (map / lines / table / camera) ===========================
  mapModuleServer(
    id = "map1",
    data_map = data_map,
    input_var_sel = selected_vars_vector,
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    active_tab = current_tab
  )
  
  linePlotModuleServer(
    id = "line_plot1",
    data = reactive(data),
    dict = dict,
    input_variable = var_normal_name2,
    input_states = selected_states_vector,
    Ymin = reactive(if (isTRUE(input$force_y0)) 0 else NULL),
    active_tab = current_tab
  )
  
  tableModuleServer(
    id          = "sub_table",
    data_r      = data_filtered,
    active_tab  = current_tab,
    force_styles = TRUE
  )
  
  # Hemicycle: still using original module (with inputs), now pointing to camera-only selectors.
  camaraServer(
    id = "camara",
    data = SLED, # if you later refactor the module, change to data_r = sled_cam_filtered
    state_r   = reactive(input$state_sel_camera),
    chamber_r = reactive(input$chamber_sel_camera),
    year_r    = reactive(input$year_sel_camera),
    party_col = "party_name_sub_leg",
    seats_col = "total_seats_party_sub_leg",
    state_col = "state_name",
    chamber_filter_col = "chamber_election_sub_leg",
    year_col = "year",
    title_text = "Chamber composition"
  )
  
  
  # ==== 10) DOWNLOADS =======================================================
  output$download_data <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$file_format, "csv")) "csv" else "xlsx"
      paste0(input$db_sel, "_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(input$db_sel, input$file_format)
      if (!exists(input$db_sel, envir = .GlobalEnv)) {
        stop("Selected dataset does not exist in the global environment.")
      }
      data_to_download <- get(input$db_sel, envir = .GlobalEnv)
      if (identical(input$file_format, "csv")) {
        write.csv(data_to_download, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        openxlsx::write.xlsx(data_to_download, file)
      }
    }
  )
  
  output$download_geom <- downloadHandler(
    filename = function() paste0("countries_geom_", Sys.Date(), ".geojson"),
    content = function(file) sf::st_write(geom, file, append = FALSE),
    contentType = "application/geo+json"
  )
  
  
  # ==== 11) PDF VIEWER ======================================================
  output$pdf_visor <- renderUI({
    tags$iframe(style = "height:800px; width:100%;", src = "codebook.pdf")
  })
}
