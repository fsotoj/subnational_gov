$(document).ready(function () {
  

  // --- STATES TREE --------------------------------------------------------
  Shiny.addCustomMessageHandler('jstree_data', function (message) {
  $('#jstree_demo').jstree({
    core: {
      animation: 0,  
      data: message.data,
      themes: { icons: false, dots: false },
      multiple: true
    },
    checkbox: {
      three_state: false,      // no auto up-propagation
      cascade: 'down',         // parent -> children only
      tie_selection: false,
      whole_node: false
    },
    plugins: ["checkbox", "wholerow"]
  });

  $('#jstree_demo')
    .on('ready.jstree', function () {
      const tree = $(this).jstree(true);
      if (message.default_selected && message.default_selected.length > 0) {
        // will also cascade down from any parents you pass here
        tree.check_node(message.default_selected);
      }
      tree.close_all();
      Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
    })

    // Parent acts as "select all children" within that parent
    .on('check_node.jstree', function (e, data) {
      const tree = $(this).jstree(true);
      const node = data.node;
      const isTopLevel = node.parent === '#'; // treat top-level as "country"

      if (isTopLevel && node.children_d && node.children_d.length) {
        // check all descendants
        tree.check_node(node.children_d);
      }

      Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
    })

    // Parent uncheck acts as "deselect all children"
    .on('uncheck_node.jstree', function (e, data) {
      const tree = $(this).jstree(true);
      const node = data.node;
      const isTopLevel = node.parent === '#';

      if (isTopLevel && node.children_d && node.children_d.length) {
        tree.uncheck_node(node.children_d);
      }

      Shiny.setInputValue("selected_nodes", JSON.stringify(tree.get_checked()));
    })

    // Keep Shiny in sync for any other changes
    .on('changed.jstree', function () {
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
