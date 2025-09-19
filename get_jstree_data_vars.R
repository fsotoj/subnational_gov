# Build a JSTree where each dataset is a parent and each pretty_name is a child
get_jstree_data_vars <- function(dict) {
  tree_data <- list()
  datasets <- unique(dict$dataset)
  for (ds in datasets) {
    ds_node <- list(
      id = ds,
      text = ds,
      children = list()
    )
    vars <- unique(dict$pretty_name[dict$dataset == ds])  # children per dataset
    for (v in vars) {
      var_node <- list(
        # Keep a composite id like "DATASET-PRETTY_NAME" (same pattern as your first tree)
        id = paste(ds, v, sep = "-"),
        text = v
      )
      ds_node$children[[length(ds_node$children) + 1]] <- var_node
    }
    tree_data[[length(tree_data) + 1]] <- ds_node
  }
  return(jsonlite::toJSON(tree_data, auto_unbox = TRUE))
}
