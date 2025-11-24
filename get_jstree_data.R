to_title_case <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
}

get_jstree_data <- function(df) {
  tree_data <- list()
  countries <- unique(df$country_name)
  for (country in countries) {
    country_node <- list(
      id = country,
      text = country,
      children = list()
    )
    states <- unique(df$state_name[df$country_name == country &  !is.na(data$state_name)]) %>% sort()
    for (state in states) {
      # Muestra el nombre en formato de nombre propio en el árbol
      state_display_name <- to_title_case(state)
      state_node <- list(
        # El ID se mantiene en mayúsculas para el filtrado
        id = paste(country, state, sep = "-"),
        text = state_display_name
      )
      country_node$children[[length(country_node$children) + 1]] <- state_node
    }
    tree_data[[length(tree_data) + 1]] <- country_node
  }
  return(toJSON(tree_data, auto_unbox = TRUE))
}


# ------------------------------------------
# Better title-case helper (vectorized)
# ------------------------------------------
to_title_case <- function(x) {
  sapply(x, function(txt) {
    words <- strsplit(txt, " ")[[1]]
    words <- paste0(toupper(substring(words, 1, 1)),
                    tolower(substring(words, 2)))
    paste(words, collapse = " ")
  })
}

# ------------------------------------------
# Fancytree data builder (replicates jsTree)
# ------------------------------------------
get_fancytree_data_states <- function(df) {
  
  tree_data <- list()
  countries <- unique(df$country_name)
  
  for (country in countries) {
    
    # Initialize list of children
    state_nodes <- list()
    
    # Extract states for this country
    states <- df$state_name[df$country_name == country & !is.na(df$state_name)] |> sort() %>% unique()
    
    # Apply title-case to displayed names
    titles <- to_title_case(states)
    
    # Build state nodes
    if (length(states) > 0) {
      state_nodes <- lapply(seq_along(states), function(i) {
        list(
          key   = paste(country, states[i], sep = "-"),   # Key remains uppercase for filtering
          title = titles[i]                               # Pretty display
        )
      })
    }
    
    # Build country folder node
    country_node <- list(
      key = country,
      title = country,
      folder = TRUE,
      children = state_nodes
    )
    
    tree_data[[length(tree_data) + 1]] <- country_node
  }
  
  jsonlite::toJSON(tree_data, auto_unbox = TRUE, null = "null")
}
