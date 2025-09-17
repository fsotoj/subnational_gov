aboutSPPUI <- function(id) {
  ns <- NS(id)
  root_id <- ns("root")
  
  style_css <- paste0("
    #", root_id, " { background-color:#0F1D2C; color:#eef1f4; font-family: Helvetica, Arial, sans-serif; padding:24px 16px 40px; }
    #", root_id, " .spp-container { max-width:900px; margin:0 auto; }

    /* Panels */
    #", root_id, " .spp-panel { background:rgba(255,255,255,0.06); border:1px solid rgba(255,255,255,0.08); border-radius:12px; padding:18px; margin-bottom:28px; }

    /* Section kickers + compact titles */
    #", root_id, " .kicker { color:#8fd2df; text-transform:uppercase; letter-spacing:0.12em; font-size:12px; margin:0 0 6px 0; opacity:0.9; }
    #", root_id, " .spp-title { color:#dfe7ee; font-size:20px; font-weight:600; margin:0 0 12px 0; }

    #", root_id, " .spp-text { color:#dbe2ea; line-height:1.6; text-align:justify; }
    #", root_id, " .spp-meta { color:#b9c4d0; font-size:0.92em; }
    #", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(23,162,184,0), rgba(23,162,184,0.7), rgba(23,162,184,0)); margin:18px 0; }

    /* Badges for roles */
    #", root_id, " .badge { display:inline-block; padding:2px 8px; border-radius:999px; font-size:11px; font-weight:700; vertical-align:middle; margin-left:8px; }
    #", root_id, " .badge-pi { background:#17a2b8; color:#0F1D2C; }
    #", root_id, " .badge-collab { background:rgba(23,162,184,0.18); color:#9bdbe7; border:1px solid rgba(23,162,184,0.35); }

    /* PI card */
    #", root_id, " .pi-card { display:flex; gap:16px; align-items:flex-start; border-left:3px solid #17a2b8; padding-left:12px; background:rgba(255,255,255,0.06); border:1px solid rgba(255,255,255,0.08); border-radius:12px; padding:14px; }
    #", root_id, " .pi-img { width:120px; border-radius:10px; flex:0 0 120px; }
    #", root_id, " .pi-name { margin:0; color:#fff; font-weight:650; font-size:1.08em; }
    #", root_id, " .pi-affil { margin:2px 0 6px 0; color:#b9c4d0; font-size:0.92em; }
    #", root_id, " .pi-bio { color:#dbe2ea; line-height:1.6; margin:6px 0 0 0; text-align:justify; }

    /* Collaborators list */
    #", root_id, " .collab-panel { background:rgba(255,255,255,0.06); border:1px solid rgba(255,255,255,0.08); border-radius:12px; padding:14px; }
    #", root_id, " .collab-list { list-style:none; padding-left:0; margin:0; }
    #", root_id, " .collab-item { margin-bottom:12px; color:#dbe2ea; }
    #", root_id, " .collab-item:last-child { margin-bottom:0; }
    #", root_id, " .collab-name { color:#ffffff; font-weight:600; }

    /* LinkedIn button */
    #", root_id, " .lnk-btn { display:inline-block; margin-left:6px; width:20px; height:20px; line-height:20px; text-align:center; border-radius:50%; background:#0A66C2; color:#fff; text-decoration:none; font-weight:700; font-family:Arial, Helvetica, sans-serif; font-size:11px; transition:filter .15s ease; vertical-align:middle; }
    #", root_id, " .lnk-btn:hover { filter:brightness(1.15); }

    /* Links */
    #", root_id, " .spp-link { color:#17a2b8; text-decoration:none; }
    #", root_id, " .spp-link:hover { text-decoration:underline; }
  ")
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(id = root_id,
             tags$div(class = "spp-container",
                      
                      # ABOUT
                      tags$p(class="kicker", "About"),
                      #tags$h3(class="spp-title", "The Subnational Politics Project (SPP)"),
                      tags$div(class="spp-panel",
                               tags$p(class="spp-text",
                                      tags$strong("The Subnational Politics Project (SPP)"),
                                      " is part of a broader research project designed to compile, ",
                                      "generate, and disseminate systematic, transparent, and publicly accessible data on subnational political ",
                                      "institutions, subnational political processes, and subnational electoral outcomes in Latin America."
                               ),
                               tags$p(class="spp-text",
                                      "The primary objective of the project is to create a centralized and standardized data infrastructure ",
                                      "that facilitates both in-depth within-country analyses and cross-national comparative research on ",
                                      "subnational political dynamics."
                               ),
                               tags$p(class="spp-text",
                                      "By providing longitudinal and spatially disaggregated data, the SPP seeks to support empirical scholarship ",
                                      "on a wide range of topics, including federalism, decentralization, party competition, electoral accountability, ",
                                      "and territorial governance."
                               ),
                               tags$p(class="spp-text",
                                      "This application provides direct access to the SPP databases and interactive tools for exploring subnational ",
                                      "political dynamics. As of September 2025, the project includes comprehensive databases for three federal countries ",
                                      "in Latin America—Argentina, Brazil, and Mexico—covering the period from the 1980s through 2024."
                               ),
                               tags$hr(class="spp-hr"),
                               tags$p(class="kicker", "References"),
                               tags$p(class="spp-meta",
                                      HTML("Giraudy, Agustina. 2025. “Codebook Subnational Politics Project (SPP) (v. 1).” "),
                                      tags$em("Subnational Politics Project"), ". ",
                                      tags$a(href="https://doi.org/doi:10.7910/DVN/IBSJO2", target="_blank", class="spp-link",
                                             "https://doi.org/doi:10.7910/DVN/IBSJO2"), "."
                               )
                      ),
                      
                      # TEAM
                      tags$p(class="kicker", "Team"),
                      #tags$h3(class="spp-title", "People behind the project"),
                      
                      # Principal Investigator
                      tags$div(class="pi-card",
                               tags$img(src="agustina_picture.jpg", alt="Agustina Giraudy", class="pi-img"),
                               tags$div(
                                 tags$p(class="pi-name",
                                        "Agustina Giraudy",
                                        tags$span(class="badge badge-pi", "Principal Investigator"),
                                        tags$a(href="https://www.linkedin.com/in/agustina-giraudy-72a3b81a9/",
                                               target="_blank", class="lnk-btn", "in")
                                 ),
                                 tags$p(class="pi-affil", "American University / Tecnológico de Monterrey"),
                                 tags$p(class="pi-bio",
                                        "Political scientist focused on governance, subnational regimes, and federalism. PhD (UNC Chapel Hill), postdoc (Harvard). Widely published; extensive fieldwork across Latin America."
                                 )
                               )
                      ),
                      
                      # Collaborators
                      tags$div(class="collab-panel",
                               tags$ul(class="collab-list",
                                       tags$li(
                                         tags$span(class="collab-name", "Francisco Urdinez"),
                                         tags$span(class="badge badge-collab", "Collaborator"),
                                         tags$a(href="https://www.linkedin.com/in/francisco-urdinez-a8061813/", target="_blank", class="lnk-btn", "in"),
                                         HTML("&nbsp;— Universidad Católica de Chile / Tecnológico de Monterrey")
                                       ),
                                       tags$li(
                                         tags$span(class="collab-name", "Sergio Huertas Hernández"),
                                         tags$span(class="badge badge-collab", "Collaborator"),
                                         tags$a(href="https://www.linkedin.com/in/sergio-huertas-hern%C3%A1ndez/", target="_blank", class="lnk-btn", "in"),
                                         HTML("&nbsp;— Universidad Católica de Chile")
                                       ),
                                       tags$li(
                                         tags$span(class="collab-name", "Guadalupe González"),
                                         tags$span(class="badge badge-collab", "Collaborator"),
                                         tags$a(href="https://www.linkedin.com/in/guadag12/", target="_blank", class="lnk-btn", "in"),
                                         HTML("&nbsp;— University of Maryland, College Park")
                                       ),
                                       tags$li(
                                         tags$span(class="collab-name", "Felipe Soto Jorquera"),
                                         tags$span(class="badge badge-collab", "Collaborator"),
                                         tags$a(href="https://www.linkedin.com/in/felipesotojorquera/", target="_blank", class="lnk-btn", "in"),
                                         HTML("&nbsp;— Hertie School, Berlin")
                                       )
                               )
                      )
                      
             )
    )
  )
}

aboutSPPServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
