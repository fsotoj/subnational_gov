library(shiny)

ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css")
  ),
  div() 
)

server <- function(input, output, session) {
  
  new_site_url <- "https://subnationalpolitics.com"
  
  showModal(modalDialog(
    title = NULL, footer = NULL, fade = TRUE, size = "m", easyClose = FALSE,
    
    div(style = "text-align: center; padding: 25px;",
        # 1. Branded Logo
        img(src = "SPP.svg", class = "spp-logo"),        
        # 2. Migration Message
        h2(style = "font-weight: 800; color: #4D4D4D;", "We've Moved!"),
        p(style = "font-size: 1.15em; color: #4D4D4D; line-height: 1.6; margin-top: 15px;",
          "The SPP Dashboard has moved to its ", tags$b("own dedicated home"), " at ",
          tags$a(href = new_site_url, "subnationalpolitics.com", 
                 style = "color: #E5007D; font-weight: 600; text-decoration: underline;"),
          " to provide you with a faster and more reliable experience."),
        
        # 3. Simple SPP Button
        tags$div(class = "button-container",
          tags$a(
            href = new_site_url, 
            class = "btn-spp", 
            "Go to our webpage"
          )
        ),
        
        # 4. Timer
        p(id = "countdown", style = "margin-top: 30px; color: #4D4D4D; opacity: 0.6; font-size: 0.9em;", 
          "Redirecting automatically in 10 seconds...")
    ),
    
    tags$script(HTML(sprintf("
      var seconds = 10;
      var timer = setInterval(function() {
        seconds--;
        var el = document.getElementById('countdown');
        if (el) el.innerHTML = 'Redirecting in ' + seconds + 's...';
        if (seconds <= 0) { 
          clearInterval(timer); 
          window.location.href = '%s'; 
        }
      }, 1000);
    ", new_site_url)))
  ))
}

shinyApp(ui, server)