# Build a JSTree where each dataset is a parent; for SLED add Lower/Upper branches
get_jstree_data_vars <- function(dict, chamber_structure = TRUE) {
  tree_data <- list()
  datasets <- unique(dict$dataset)
  for (ds in datasets) {
    ds_node <- list(
      id = ds,
      text = ds,
      children = list()
    )
    
    vars <- unique(dict$pretty_name[dict$dataset == ds])
    
    if (identical(ds, "Legislative Elections")) {
      # Build LOWER branch
    
      
      lower_children <- lapply(vars, function(v) {
        list(
          id   = paste("Legislative Elections", "Lower Chamber", v, sep = "-"),  # e.g., "SLED-Lower-Voter Turnout Percentage"
          text = v
        )
      })
      lower_node <- list(
        id = "Legislative Elections-Lower Chamber",
        text = "Lower Chamber",
        children = lower_children
      )
      
      # Build UPPER branch
      upper_children <- lapply(vars, function(v) {
        list(
          id   = paste("Legislative Elections", "Upper Chamber", v, sep = "-"),  # e.g., "SLED-Upper-Voter Turnout Percentage"
          text = v
        )
      })
      upper_node <- list(
        id = "Legislative Elections-Upper Chamber",
        text = "Upper Chamber",
        children = upper_children
      )
    
      if (chamber_structure) {
        chamber_node <- list(
          id = "Legislative Elections-Chamber Structure",
          text = "Chamber Structure"
        )
        
        ds_node$children <- append(ds_node$children,
                                   list(chamber_node,lower_node, upper_node))
      } else {
        
        ds_node$children <- append(ds_node$children,
                                   list(lower_node, upper_node))
        }
      
        
      
      
    } else {
      # Default: dataset -> variables (single level)
      for (v in vars) {
        var_node <- list(
          id   = paste(ds, v, sep = "-"),
          text = v
        )
        ds_node$children[[length(ds_node$children) + 1]] <- var_node
      }
    }
    
    tree_data[[length(tree_data) + 1]] <- ds_node
  }
  return(jsonlite::toJSON(tree_data, auto_unbox = TRUE))
}
