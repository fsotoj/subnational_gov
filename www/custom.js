$(document).ready(function () {

  // --- STATES TREE --------------------------------------------------------
  Shiny.addCustomMessageHandler('jstree_data', function (message) {
    $('#jstree_demo').jstree({
      core: {
        data: message.data,
        themes: { icons: false, dots: false },
        multiple: true
      },
      checkbox: {
        three_state: false,       // no auto cascade up/down
        cascade: 'undetermined',  // don't check/uncheck descendants automatically
        tie_selection: false,
        whole_node: false
      },
      plugins: ["checkbox", "wholerow"]
    });

    $('#jstree_demo')
      .on('ready.jstree', function () {
        const tree = $(this).jstree(true);
        if (message.default_selected && message.default_selected.length > 0) {
          tree.check_node(message.default_selected); // check (states) by id
        }
        tree.close_all();
        // Push initial checked ids
        Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
      })

      // Country checkbox acts as "deselect all states" within that country
      .on('check_node.jstree', function (e, data) {
        const tree = $(this).jstree(true);
        const node = data.node;

        const isCountry = node.parent === '#'; // top-level nodes = countries
        if (isCountry) {
          // Uncheck all descendants (states) and uncheck the country itself
          if (node.children_d && node.children_d.length) {
            tree.uncheck_node(node.children_d);
          }
          tree.uncheck_node(node); // keep country unchecked after the action
        }

        // Send current checked IDs (states only, since countries end up unchecked)
        Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
      })

      // Keep Shiny in sync when states are (un)checked individually
      .on('uncheck_node.jstree', function () {
        const tree = $(this).jstree(true);
        Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
      })
      .on('changed.jstree', function () {
        // Optional: if other interactions change checks, keep Shiny updated
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
        themes: { icons: false,  dots: false   },
        multiple: false                // <-- SINGLE selection
      },
      plugins: ["wholerow"]                      // <-- no "checkbox" plugin
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
  
  
  Shiny.addCustomMessageHandler('jstree_vars_data_graph', function (message) {
    var $el = $('#jstree_vars_demo_graph');
    if ($el.data('jstree')) { $el.jstree(true).destroy(); }
  
    $el.jstree({
      core: {
        data: message.data,
        themes: { icons: false,  dots: false   },
        multiple: false                // <-- SINGLE selection
      },
      plugins: ["wholerow"]                      // <-- no "checkbox" plugin
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
        Shiny.setInputValue('selected_nodes_vars_graph',
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
  
        Shiny.setInputValue('selected_nodes_vars_graph',
          JSON.stringify(tree.get_selected()));
      });
  });


});
