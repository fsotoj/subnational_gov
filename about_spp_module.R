aboutSPPUI <- function(id) {
  ns <- NS(id)
  root_id <- ns("root")
  
  style_css <- paste0("
    #", root_id, " { background-color:#0F1D2C; color:#eef1f4; font-family: Helvetica, Arial, sans-serif; padding:24px 16px 40px; }
    #", root_id, " .spp-container { max-width:900px; margin:0 auto; }

    /* Unified card style for About panel and Team cards */
    #", root_id, " .card { 
      background:rgba(255,255,255,0.06); 
      border:1px solid rgba(255,255,255,0.08); 
      border-radius:12px; 
      padding:18px; 
      margin-bottom:28px;
      transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease;
    }
    #", root_id, " .card:hover { 
      transform: translateY(-2px); 
      box-shadow: 0 6px 14px rgba(0,0,0,0.25); 
      border-color: rgba(23,162,184,0.35);
    }

    /* Section kickers */
    #", root_id, " .kicker { color:#8fd2df; text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 8px 0; opacity:0.9; }

    /* Body text */
    #", root_id, " .spp-text { color:#dbe2ea; line-height:1.6; text-align:justify; }
    #", root_id, " .spp-meta { color:#b9c4d0; font-size:0.92em; }
    #", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(23,162,184,0), rgba(23,162,184,0.7), rgba(23,162,184,0)); margin:18px 0; }

    /* Badges for roles */
    #", root_id, " .badge { display:inline-block; padding:2px 8px; border-radius:999px; font-size:11px; font-weight:700; vertical-align:middle; margin-right:6px; }
    #", root_id, " .badge-pi { background:#17a2b8; color:#0F1D2C; }
    #", root_id, " .badge-collab { background:rgba(23,162,184,0.18); color:#9bdbe7; border:1px solid rgba(23,162,184,0.35); }

    /* Team grid & cards */
    #", root_id, " .team-grid { display:grid; grid-template-columns:repeat(auto-fit, minmax(220px, 1fr)); gap:20px; text-align:center; }
    #", root_id, " .team-card { composes: card; background:rgba(255,255,255,0.06); }
    /* Since CSS 'composes' isn't standard, we apply same look by adding both classes where used. */

    /* Avatars (B/W with hover to color) */
    #", root_id, " .team-avatar {
      width:120px; height:120px; object-fit:cover; border-radius:50%;
      margin-bottom:12px; border:2px solid rgba(23,162,184,0.6);
      filter:grayscale(100%); transition:filter .3s ease;
    }
    #", root_id, " .team-avatar:hover { filter:grayscale(0%); }

    .collab-name { color:#ffffff; font-weight:600; margin:0 0 6px 0; }
    .pi-affil { color:#b9c4d0; font-size:0.9em; margin-top:8px; }

    /* LinkedIn button */
    #", root_id, " .lnk-btn { display:inline-block; width:20px; height:20px; line-height:20px; text-align:center; border-radius:50%; background:#0A66C2; color:#fff; text-decoration:none; font-weight:700; font-family:Arial, Helvetica, sans-serif; font-size:11px; transition:filter .15s ease; vertical-align:middle; }
    #", root_id, " .lnk-btn:hover { filter:brightness(1.15); }

    /* Links */
    #", root_id, " .spp-link { color:#17a2b8; text-decoration:none; }
    #", root_id, " .spp-link:hover { text-decoration:underline; }
  ")
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(id = root_id,
             tags$div(class = "spp-container",
                      
                      # ABOUT (now uses same 'card' class as team)
                      tags$p(class="kicker", "About"),
                      tags$div(class="card",
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
                               tags$p(class="kicker", "Reference"),
                               tags$p(class="spp-meta",
                                      HTML("Giraudy, Agustina; Gonzalez, Guadalupe Andrea; Urdinez, Francisco, 2025, \"Codebook: Subnational Politics Project (SPP) (v. 1)\", "),
                                      tags$a(href="https://doi.org/doi:10.7910/DVN/IBSJO2", target="_blank", class="spp-link",
                                             "https://doi.org/doi:10.7910/DVN/IBSJO2"), "."
                               )
                      ),
                      
                      # TEAM
                      tags$p(class="kicker", "People behind the SPP"),
                      
                      # Grid: All team members in same format (each is also a 'card')
                      tags$div(class="team-grid",
                               
                               # Agustina
                               tags$div(class="team-card card",
                                        tags$a(
                                          href="https://agustinagiraudy.com", 
                                          target="_blank",
                                          class="avatar-link",
                                          tags$img(src="agustina.jpg", alt="Agustina Giraudy", class="team-avatar"),
                                          tags$span(class="tooltip-text", "Go to my webpage")
                                        ),
                                        tags$p(class="collab-name", "Agustina Giraudy"),
                                        tags$div(
                                          tags$span(class="badge badge-pi", "Principal Investigator"),
                                          tags$a(href="https://www.linkedin.com/in/agustina-giraudy-72a3b81a9/",
                                                 target="_blank", class="lnk-btn", "in")
                                        ),
                                        tags$p(class="pi-affil", "American University / Tecnológico de Monterrey")
                               ),
                               
                               # Francisco
                               tags$div(class="team-card card",
                                        tags$a(
                                          href="https://www.furdinez.com/", 
                                          target="_blank",
                                          class="avatar-link",
                                          tags$img(src="francisco.jpg", alt="Francisco Urdinez", class="team-avatar"),
                                          tags$span(class="tooltip-text", "Go to my webpage")
                                          ),
                                        tags$p(class="collab-name", "Francisco Urdinez"),
                                        tags$div(
                                          tags$span(class="badge badge-collab", "Collaborator"),
                                          tags$a(href="https://www.linkedin.com/in/francisco-urdinez-a8061813/", target="_blank", class="lnk-btn", "in")
                                        ),
                                        tags$p(class="pi-affil", "Universidad Católica de Chile")
                               ),
                               
                               # Sergio
                               tags$div(class="team-card card",
                                        tags$a(
                                          href="https://serhuertas.github.io/", 
                                          target="_blank",
                                          class="avatar-link",
                                          tags$img(src="sergio.jpg", alt="Sergio Huertas Hernández", class="team-avatar"),
                                          tags$span(class="tooltip-text", "Go to my webpage")
                                        ),
                                        tags$p(class="collab-name", "Sergio Huertas Hernández"),
                                        tags$div(
                                          tags$span(class="badge badge-collab", "Collaborator"),
                                          tags$a(href="https://www.linkedin.com/in/sergio-huertas-hern%C3%A1ndez/", target="_blank", class="lnk-btn", "in")
                                        ),
                                        tags$p(class="pi-affil", "Universidad Católica de Chile")
                               ),
                               
                               # Guadalupe
                               tags$div(class="team-card card",
                                        tags$a(
                                          href="https://guadagonzalez.com/", 
                                          target="_blank",
                                          class="avatar-link",
                                          tags$img(src="guadalupe.jpg", alt="Guadalupe González", class="team-avatar"),
                                          tags$span(class="tooltip-text", "Go to my webpage")
                                        ),
                                        tags$p(class="collab-name", "Guadalupe González"),
                                        tags$div(
                                          tags$span(class="badge badge-collab", "Collaborator"),
                                          tags$a(href="https://www.linkedin.com/in/guadag12/", target="_blank", class="lnk-btn", "in")
                                        ),
                                        tags$p(class="pi-affil", "University of Maryland, College Park")
                               ),
                               
                               # Felipe
                               tags$div(class="team-card card",
                                        tags$img(src="felipe.jpg", alt="Felipe Soto Jorquera", class="team-avatar"),
                                        tags$p(class="collab-name", "Felipe Soto Jorquera"),
                                        tags$div(
                                          tags$span(class="badge badge-collab", "Collaborator"),
                                          tags$a(href="https://www.linkedin.com/in/felipesotojorquera/", target="_blank", class="lnk-btn", "in")
                                        ),
                                        tags$p(class="pi-affil", "Hertie School, Berlin")
                               )
                      )
             )
    )
  )
}

aboutSPPServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
