
treeSelectorUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::dropdownButton(
      label = "Select states",
      circle = FALSE,
      status = "primary",
      icon = icon("caret-down"),
      width = "300px",
      uiOutput(ns("country_list"))
    ),
    # input oculto con la selección serializada
    textInput(ns("states_sel"), label = NULL, value = "")
  )
}

treeSelectorServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    countries <- unique(data$country_name)
    selected_states <- reactiveVal(character(0))
    expanded_country <- reactiveVal(NULL)
    
    output$country_list <- renderUI({
      tagList(
        lapply(countries, function(country) {
          btn_id <- ns(paste0("btn_", gsub(" ", "_", country)))
          
          tagList(
            actionButton(btn_id, label = country, style = "text-align:left; width:100%; font-weight:bold; margin-bottom: 3px;"),
            if (!is.null(expanded_country()) && expanded_country() == country) {
              states <- data %>% filter(country_name == country) %>% pull(state_name)
              checkboxGroupInput(
                inputId = ns(paste0("states_", gsub(" ", "_", country))),
                label = NULL,
                choices = states,
                selected = intersect(selected_states(), states),
                inline = FALSE
              )
            }
          )
        })
      )
    })
    
    observe({
      lapply(countries, function(country) {
        btn_id <- paste0("btn_", gsub(" ", "_", country))
        observeEvent(input[[btn_id]], {
          if (expanded_country() == country) {
            expanded_country(NULL)
          } else {
            expanded_country(country)
          }
        }, ignoreInit = TRUE)
      })
    })
    
    observe({
      lapply(countries, function(country) {
        input_id <- paste0("states_", gsub(" ", "_", country))
        observeEvent(input[[input_id]], {
          current <- selected_states()
          states_of_country <- data %>% filter(country_name == country) %>% pull(state_name)
          current <- setdiff(current, states_of_country)
          new_selection <- input[[input_id]]
          if (is.null(new_selection)) new_selection <- character(0)
          selected_states(c(current, new_selection))
          
          # Actualizar input oculto con cadena CSV
          updateTextInput(session, "states_sel", value = paste(selected_states(), collapse = ","))
        }, ignoreNULL = FALSE)
      })
    })
    
    # Inicializa input oculto
    observe({
      updateTextInput(session, "states_sel", value = paste(selected_states(), collapse = ","))
    })
    
    # El módulo devuelve la selección en forma reactiva (vector de strings)
    selected_states
  })
}
