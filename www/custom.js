$(document).ready(function () {

  Shiny.addCustomMessageHandler('jstree_data', function (message) {
    $('#jstree_demo').jstree({
      core: {
        data: message.data,
        themes: { icons: false },
        multiple: true
      },
      checkbox: {
        three_state: false,       // no auto cascade up/down
        cascade: 'undetermined',  // don't check/uncheck descendants automatically
        tie_selection: false,
        whole_node: false
      },
      plugins: ["checkbox"]
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

});
