# --- Packages -----------------------------------------------------------------
library(shiny)
library(DT)
library(htmltools)

# --- Small helpers ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || all(is.na(x))) y else x

# --- Citations dictionary (abbr -> citation + DOI) ----------------------------
spp_citations <- data.frame(
  abbr  = c("SED","SEED","SLED","SDI","CFTDFLD","NED"),
  title = c(
    "Subnational Executive Database",
    "Subnational Executive Elections Database",
    "Subnational Legislative Elections Database",
    "Subnational Democracy Indices",
    "Capital Federal & Tierra del Fuego Legislatures Database",
    "National Executive Database"
  ),
  citation_text = c(
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CSubnational Executive Database (SED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CSubnational Executive Elections Database (SEED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. \u201CSubnational Legislative Elections Database (SLED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina. 2025. \u201CSubnational Democracy Indices (SDI) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. \u201CCapital Federal & Tierra Del Fuego Legislatures Database (CFTDFLD) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CNational Executive Database (NED) (v. 1).\u201D Subnational Politics Project. "
  ),
  doi  = c(
    "10.7910/DVN/1D3P3J",
    "10.7910/DVN/UPOWMW",
    "10.7910/DVN/084FXF",
    "10.7910/DVN/7TNLBW",
    "10.7910/DVN/AJJLHX",
    "10.7910/DVN/HNKQUH"
  ),
  stringsAsFactors = FALSE
)

spp_citations$citation_html <- sprintf(
  '%s<a href="https://doi.org/%s" target="_blank" class="spp-link">https://doi.org/%s</a>',
  htmlEscape(spp_citations$citation_text), spp_citations$doi, spp_citations$doi
)

# --- Dataverse fetcher --------------------------------------------------------
dv_search_mvp <- function(subtree = "spp", per_page = 50) {
  url <- sprintf(
    "https://dataverse.harvard.edu/api/search?q=*&type=dataset&subtree=%s&per_page=%d&start=0&sort=date&order=desc",
    utils::URLencode(subtree, reserved = TRUE), per_page
  )
  res <- tryCatch(jsonlite::fromJSON(url, flatten = TRUE), error = function(e) NULL)
  if (is.null(res) || is.null(res$data) || is.null(res$data$items))
    return(data.frame())
  as.data.frame(res$data$items, stringsAsFactors = FALSE)
}

# --- UI -----------------------------------------------------------------------
spp_mvp_ui <- function(id,
                       title = "SPP databases",
                       subtree = "spp",
                       per_page = 50,
                       max_width = 1050,
                       open_sections = c("datasets")) {
  ns <- NS(id)
  root_id <- ns("root")
  
  open_attr <- function(key, open_sections) if (key %in% open_sections) "open" else NULL
  
  # Scoped styles
  style_css <- paste0(
    "/* Palette via CSS variables (fallbacks included) */\n",
    "#", root_id, " { background:#fff; color:var(--gray, #4D4D4D); font-family: Helvetica, Arial, sans-serif; padding:24px 10px; }\n",
    "#", root_id, " .spp-container { max-width:", max_width, "px; margin:0 auto; }\n",
    "#", root_id, " .kicker { color:var(--orange, #FFA92A); text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 12px 0; }\n",
    "#", root_id, " p.description { text-align:center; color:#444; margin-bottom:20px; }\n",
    "#", root_id, " details.card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; margin-bottom:18px; overflow:visible; transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease; }\n",
    "#", root_id, " details.card:hover { transform: translateY(-2px); box-shadow: 0 6px 14px rgba(0,0,0,0.18); border-color: rgba(255,169,42,0.35); }\n",
    "#", root_id, " details.card[open] { box-shadow:0 6px 14px rgba(0,0,0,0.12); border-color: rgba(255,169,42,0.45); }\n",
    "#", root_id, " summary { list-style:none; cursor:pointer; padding:14px 16px; display:flex; align-items:center; gap:10px; }\n",
    "#", root_id, " summary::-webkit-details-marker { display:none; }\n",
    "#", root_id, " .chev { margin-left:auto; transition: transform .15s ease; }\n",
    "#", root_id, " details[open] .chev { transform: rotate(180deg); }\n",
    "#", root_id, " .card-title { font-weight:800; font-size:18px; color:var(--purple, #722464); }\n",
    "#", root_id, " .card-body { padding: 0 16px 16px 16px; color:var(--gray, #4D4D4D); }\n",
    "#", root_id, " .spp-text { color:var(--gray, #4D4D4D); line-height:1.6; text-align:justify; }\n",
    "#", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0)); margin:14px 0; }\n",
    "#", root_id, " .figure { margin:10px 0 12px 0; text-align:center; }\n",
    "#", root_id, " .figure img { max-width:100%; height:auto; border-radius:12px; border:1px solid #eee; }\n",
    "#", root_id, " ol.vars { margin:10px 0 0 18px; }\n",
    "#", root_id, " ol.vars li { margin:6px 0; }\n",
    "#", root_id, " table.dataTable td:nth-child(1) { min-width: 490px; }\n",
    "#", root_id, " .loading-wrap { position:relative; min-height:120px; display:flex; align-items:center; justify-content:center; flex-direction:column; gap:10px; }\n",
    "#", root_id, " .spinner { width:42px; height:42px; border:4px solid #eee; border-top-color:var(--orange, #FFA92A); border-radius:50%; animation: spp-spin 0.9s linear infinite; }\n",
    "@keyframes spp-spin { to { transform: rotate(360deg); } }\n",
    "#", root_id, " .loading-text { color:#666; font-size:0.95rem; }\n"
  )
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(
      id = root_id,
      tags$div(
        class = "spp-container",
        tags$p(class = "kicker", title),
        
        # --- Card 1: Datasets table ------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("datasets", open_sections),
          tags$summary(
            tags$span(class = "card-title", "SPP Databases"),
            shiny::icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(
              class = "spp-text",
              "Click a dataset to open and download the corresponding data from Harvard Dataverse."
            ),
            uiOutput(ns("datasets_panel"))
          )
        ),
        
        # --- Card 2: About Databases (unchanged text + figures) ---------------
        tags$details(
          class = "card",
          open  = open_attr("databases", open_sections),
          tags$summary(
            tags$span(class = "card-title", "About SPP Databases"),
            shiny::icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(class = "spp-text", tags$strong("Databases’ Structure")),
            tags$p(
              class = "spp-text",
              "The Subnational Politics Project (SPP) is made up of different databases. Each database employs a country–state–year structure, with observations at the subnational unit level for each electoral year. Each observation represents a subnational unit (province/state) in a given year."
            ),
            tags$div(class = "figure", tags$img(src = "databases_spp.svg", alt = "Figure. SPP Databases")),
            tags$hr(class = "spp-hr"),
            tags$p(class = "spp-text", tags$strong("Variable Information")),
            tags$p(class = "spp-text", "The databases in the Subnational Politics Project divide variables into the following types:"),
            tags$ol(
              class = "vars",
              tags$li(tags$b("Identifier Variables:"), " Data identifying country names and codes, state names and codes, region names, and time periods."),
              tags$li(tags$b("Executive Branch Variables:"), " Data on national and subnational executive branches, such as length of term, incumbent party, cumulative years of president/governor in office, etc."),
              tags$li(tags$b("Electoral Variables:"), " Data on subnational executive and subnational legislative elections, including legislatures’ composition."),
              tags$li(tags$b("Indices:"), " Data generated by adding and combining variables, or creating cumulative scales.")
            ),
            tags$div(class = "figure", tags$img(src = "variables_database.svg", alt = "Figure. Variable Types in SPP"))
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
spp_mvp_server <- function(id,
                           subtree = "spp",
                           per_page = 50,
                           current_tab = NULL,
                           tab_id = "data_tab") {
  moduleServer(id, function(input, output, session) {
    
    items_rv <- reactiveVal(NULL)
    
    observeEvent({
      if (is.null(current_tab)) TRUE else current_tab()
    }, {
      if (!is.null(current_tab)) req(current_tab() == tab_id)
      
      # show spinner now
      items_rv(NULL)
      
      # delay so spinner paints, then fetch
      later::later(function() {
        items_rv(dv_search_mvp(subtree = subtree, per_page = per_page))
      }, delay = 0.05)
    }, ignoreInit = FALSE)
    
    # helper: detect first matching abbreviation token in a dataset name
    detect_abbr <- function(dataset_name) {
      hits <- spp_citations$abbr[
        vapply(
          spp_citations$abbr,
          function(a) grepl(paste0("\\b", a, "\\b"), dataset_name %||% "", ignore.case = TRUE),
          logical(1)
        )
      ]
      if (length(hits)) hits[[1]] else NA_character_
    }
    
    output$datasets_panel <- renderUI({
      if (!is.null(current_tab) && current_tab() != tab_id) return(NULL)
      
      items <- items_rv()
      if (is.null(items)) {
        tags$div(
          class = "loading-wrap",
          `aria-live` = "polite",
          tags$div(class = "spinner"),
          tags$p(class = "loading-text", "Fetching datasets from Harvard Dataverse…")
        )
      } else {
        DT::DTOutput(session$ns("tbl"))
      }
    })
    
    output$tbl <- DT::renderDT({
      if (!is.null(current_tab)) req(current_tab() == tab_id)
      items <- items_rv()
      req(items)
      
      if (!nrow(items)) {
        return(DT::datatable(
          data.frame(Message = "No results found."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      
      # Ensure expected fields exist
      for (col in c("url","name")) if (is.null(items[[col]])) items[[col]] <- NA_character_
      items[is.na(items$url),  "url"]  <- "#"
      items[is.na(items$name), "name"] <- "Untitled dataset"
      
      # Match by abbreviation found in the dataset name
      items$abbr <- vapply(items$name, detect_abbr, character(1))
      
      # Join to bring in the citation HTML; keep unmatched rows (citation empty)
      merged <- merge(
        items,
        spp_citations[, c("abbr","citation_html")],
        by = "abbr",
        all.x = TRUE
      )
      
      # Build the two-column display (NO abbreviation column)
      df <- data.frame(
        `Database Name` = vapply(seq_len(nrow(merged)), function(i) {
          as.character(
            tags$a(href = merged$url[i], target = "_blank", rel = "noopener",
                   HTML(htmlEscape(merged$name[i])))
          )
        }, character(1)),
        `Suggested Citation` = merged$citation_html %||% "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        df,
        escape = c(FALSE, FALSE),   # keep link + citation HTML
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          info = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        )
      )
    })
  })
}
