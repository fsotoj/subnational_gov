# UI del módulo
stateSelectorModuleUI  <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tree_ui"))
  )
}

# Server del módulo
stateSelectorModuleServer  <- function(id, data, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$tree_ui <- renderUI({
      req(active_tab() == "graph_tab")
      
      # Crear árbol jerárquico país > estado
      tree_choices <- create_tree(data %>% select(country_name, state_name))
      
      treeInput(
        inputId = ns("tree"),
        label = "Selecciona estados:",
        choices = tree_choices,
        returnValue = "text",
        closeDepth = 1,
        width = "100%"
      )
    })
    
    # Estados seleccionados (solo nombres de estados)
    selected_states <- reactive({
      req(input$tree)
      # Solo estados (excluye países)
      data$state_name[data$state_name %in% input$tree]
    })
    
    return(selected_states)
  })
}