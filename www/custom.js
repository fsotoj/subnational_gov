$(document).ready(function () {

  // --- STATES TREE --------------------------------------------------------
  Shiny.addCustomMessageHandler('jstree_data', function (message) {
    $('#jstree_demo').jstree({
      core: {
        data: message.data,
        themes: { icons: false },
        multiple: true
      },
      checkbox: {
        three_state: false,
        cascade: 'undetermined',
        tie_selection: false,
        whole_node: false
      },
      plugins: ["checkbox"]
    });

    $('#jstree_demo')
      .on('ready.jstree', function () {
        const tree = $(this).jstree(true);
        if (message.default_selected && message.default_selected.length > 0) {
          tree.check_node(message.default_selected);
        }
        tree.close_all();
        Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
      })
      .on('check_node.jstree uncheck_node.jstree uncheck_node.jstree changed.jstree', function () {
        const tree = $(this).jstree(true);
        Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
      });
  });


  // --- VARIABLES TREE -----------------------------------------------------
// VARIABLES TREE: single selection, no checkboxes, leaves only
  Shiny.addCustomMessageHandler('jstree_vars_data', function (message) {
    var $el = $('#jstree_vars_demo');
    if ($el.data('jstree')) { $el.jstree(true).destroy(); }
  
    $el.jstree({
      core: {
        data: message.data,
        themes: { icons: false },
        multiple: false                // <-- SINGLE selection
      },
      plugins: []                      // <-- no "checkbox" plugin
    });
  
    $el
      .on('ready.jstree', function () {
        const tree = $(this).jstree(true);
  
        // If a default is provided, select it (not check_node)
        if (message.default_selected && message.default_selected.length > 0) {
          tree.select_node(message.default_selected[0]);  // pick the first
        }
  
        tree.close_all();
  
        // Send initial selection (if any)
        Shiny.setInputValue('selected_nodes_vars',
          JSON.stringify(tree.get_selected()));
      })
  
      // Keep Shiny in sync on selection changes
      .on('changed.jstree', function (e, data) {
        const tree = $(this).jstree(true);
  
        // Optional: prevent selecting parent (dataset) nodes; only allow leaves
        if (data && data.node && data.node.children && data.node.children.length) {
          tree.deselect_node(data.node);
          return;
        }
  
        Shiny.setInputValue('selected_nodes_vars',
          JSON.stringify(tree.get_selected()));
      });
  });


});
