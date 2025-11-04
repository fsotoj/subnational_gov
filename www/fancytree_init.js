$(document).ready(function() {

  // Handle messages from Shiny
  Shiny.addCustomMessageHandler('fancytree_vars_data', function(message) {

    // Destroy existing tree if it exists
    const $tree = $("#fancytree_vars_demo");
    if ($tree.fancytree("instance")) {
      $tree.fancytree("destroy");
    }

    // Initialize new Fancytree
    $tree.fancytree({
      source: message.data,      // same data format as jsTree
      icons: false,
      checkbox: false,
      selectMode: 1,             // single selection
      autoScroll: true,
      clickFolderMode: 2,        // expand only, don’t select parent
      activate: function(event, data) {
        // send selected node key to Shiny
        if (data.node && !data.node.folder) {
          Shiny.setInputValue("selected_nodes_vars", JSON.stringify([data.node.key]));
        }
      }
    });

  });
});
