
# --- Helper -------------------------------------------------------------------
dv_search_mvp <- function(subtree = "spp", per_page = 50) {
  url <- sprintf(
    "https://dataverse.harvard.edu/api/search?q=*&type=dataset&subtree=%s&per_page=%d&start=0&sort=date&order=desc",
    utils::URLencode(subtree, reserved = TRUE), per_page
  )
  res <- tryCatch(fromJSON(url, flatten = TRUE), error = function(e) NULL)
  if (is.null(res) || is.null(res$data) || is.null(res$data$items))
    return(data.frame())
  as.data.frame(res$data$items, stringsAsFactors = FALSE)
}

# --- UI -----------------------------------------------------------------------
spp_mvp_ui <- function(id) {
  ns <- NS(id)
  root_id <- ns("root")
  
  tagList(
    # Scoped CSS for styling and layout
    tags$style(HTML(paste0(
      "#", root_id, " {",
      "  max-width: 900px;",         # narrower layout
      "  margin: 0 auto;",           # centered horizontally
      "  padding: 0 20px;",          # side padding
      "}",
      "#", root_id, " .kicker {",
      "  color: var(--orange, #FFA92A);",
      "  text-transform: uppercase;",
      "  letter-spacing: 0.12em;",
      "  font-size: 24px;",
      "  margin: 0 0 12px 0;",
      "  text-align: center;",
      "}",
      "#", root_id, " p.description {",
      "  text-align: center;",
      "  color: #444;",
      "  margin-bottom: 20px;",
      "}"
    ))),
    
    tags$div(
      id = root_id,
      tags$p(class = "kicker", "SPP datasets"),
      tags$p(
        class = "description",
        "Click a title to open the dataset on Harvard Dataverse. ",
        "There you can select your preferred data format to download."
      ),
      DTOutput(ns("tbl"))
    )
  )
}

# --- Server -------------------------------------------------------------------
spp_mvp_server <- function(id, subtree = "spp", per_page = 50,
                           current_tab = NULL, tab_id = "data_tab") {
  moduleServer(id, function(input, output, session) {
    
    items_rv <- reactiveVal(NULL)
    
    # Fetch only when the data tab is active
    observeEvent({
      if (is.null(current_tab)) TRUE else current_tab()
    }, {
      if (!is.null(current_tab)) req(current_tab() == tab_id)
      if (is.null(items_rv()))
        items_rv(dv_search_mvp(subtree = subtree, per_page = per_page))
    }, ignoreInit = FALSE)
    
    output$tbl <- renderDT({
      if (!is.null(current_tab)) req(current_tab() == tab_id)
      items <- items_rv()
      if (is.null(items))
        return(datatable(data.frame(Message = "Loading..."),
                         options = list(dom = "t"), rownames = FALSE))
      if (nrow(items) == 0)
        return(datatable(data.frame(Message = "No results found."),
                         options = list(dom = "t"), rownames = FALSE))
      
      df <- data.frame(
        Title = sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>',
                        items$url, items$name),
        Citation = items$citation,
        stringsAsFactors = FALSE
      )
      
      datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
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
