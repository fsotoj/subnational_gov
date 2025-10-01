# ---- Database Info Module (SPP) — Card Style (no modals) -------------------
# Updated per request:
# - Section titles: "Databases’ Structure" and "Variable Information"
# - Removed image modals (images remain inline, responsive)
# - Added parameter `max_width` to control container width (in pixels)

# UI -------------------------------------------------------------------------
databaseInfoModuleUI <- function(id,
                                 title = "Database Information",
                                 section1_title = "Databases’ Structure",
                                 section2_title = "Variable Information",
                                 max_width = 900) {  # container width in px
  ns <- NS(id)
  root_id <- ns("root")
  
  # Inline CSS scoped to this module root (move to styles.css if desired)
  style_css <- paste0(
    "/* === Palette (reused) ===\n",
    "   orange: #FFA92A (primary)\n",
    "   purple: #722464 (secondary)\n",
    "   magenta: #E5007D (accent)\n",
    "   gray:   #4D4D4D (neutral text)\n",
    "*/\n\n",
    "#", root_id, " { background:#ffffff; color:#111; font-family: Helvetica, Arial, sans-serif; padding:24px 16px 8px; }\n",
    "#", root_id, " .spp-container { max-width:", max_width, "px; margin:0 auto; }\n\n",
    "#", root_id, " .card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; padding:18px; margin-bottom:28px;\n",
    "  transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease; }\n",
    "#", root_id, " .card:hover { transform: translateY(-2px); box-shadow: 0 6px 14px rgba(0,0,0,0.18); border-color: rgba(255,169,42,0.45); }\n\n",
    "#", root_id, " .kicker { color:#FFA92A; text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 8px 0; opacity:0.95; }\n",
    "#", root_id, " .section-title { font-size:20px; font-weight:700; margin:0 0 8px 0; color:#222; }\n",
    "#", root_id, " .spp-text { color:#333; line-height:1.6; text-align:justify; }\n",
    "#", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0)); margin:18px 0; }\n\n",
    "#", root_id, " .figure { margin:10px 0 12px 0; text-align:center; }\n",
    "#", root_id, " .figure img { max-width:100%; height:auto; border-radius:12px; border:1px solid #eee; }\n",
    "#", root_id, " .caption { font-size:0.92rem; color:#4D4D4D; margin-top:6px; }\n\n",
    "#", root_id, " ol.vars { margin:10px 0 0 18px; }\n",
    "#", root_id, " ol.vars li { margin:6px 0; }\n"
  )
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(id = root_id,
             tags$div(class = "spp-container",
                      
                      # Header kicker ---------------------------------------------------------
                      if (!is.null(title) && nzchar(title)) tags$p(class = "kicker", title),
                      
                      # Card 1: Databases’ Structure -----------------------------------------
                      tags$div(class = "card",
                               tags$p(class = "section-title", section1_title),
                               tags$p(class = "spp-text",
                                      "As Figure 1 shows, the Subnational Politics Project (SPP) is made up of different databases. ",
                                      "Each database employs a country–state–year structure, with observations at the subnational unit level for each electoral year. ",
                                      "Each observation represents a subnational unit (province/state) in a given year."),
                               
                               tags$div(class = "figure",
                                        tags$img(src = "databases_spp.jpg", alt = "Figure 1. SPP Databases"),
                                        tags$div(class = "caption", "Figure 1. Databases that comprise the Subnational Politics Project (SPP).")
                               )
                      ),
                      
                      # Card 2: Variable Information -----------------------------------------
                      tags$div(class = "card",
                               tags$p(class = "section-title", section2_title),
                               tags$p(class = "spp-text",
                                      "As shown in Figure 2, the databases in the Subnational Politics Project divide variables into the following variable types:"),
                               
                               tags$div(class = "figure",
                                        tags$img(src = "variables_database.jpg", alt = "Figure 2. Variable Types in SPP"),
                                        tags$div(class = "caption", "Figure 2. Variable types in the SPP databases.")
                               ),
                               
                               tags$ol(class = "vars",
                                       tags$li(tags$b("Identifier Variables:"), " Data identifying country names and codes, state names and codes, region names, and time periods."),
                                       tags$li(tags$b("Executive Branch Variables:"), " Data on national and subnational executive branches, such as length of term, incumbent party, cumulative years of president/governor in office, etc."),
                                       tags$li(tags$b("Electoral Variables:"), " Data on subnational executive and subnational legislative elections, including legislatures’ composition."),
                                       tags$li(tags$b("Indices:"), " Data generated by adding and combining variables, or creating cumulative scales.")
                               )
                      )
                      
             )
    )
  )
}

# Server (no modal handlers needed) -------------------------------------------
databaseInfoModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

