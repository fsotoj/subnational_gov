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