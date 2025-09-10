#options(shiny.autoreload = FALSE)

source("ui.R")
source("server.R")



shinyApp(ui, server)



#rsconnect::writeManifest(appDir = ".", appPrimaryDoc = NULL, contentCategory = NULL)

# tabasco invalid votes 2017
