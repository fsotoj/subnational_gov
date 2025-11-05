$(document).ready(function () {
  Shiny.addCustomMessageHandler('fancytree_vars_data', function (message) {
    const $tree = $("#fancytree_vars_demo");
    if ($tree.fancytree("instance")) $tree.fancytree("destroy");

    $tree.fancytree({
      source: message.data,
      icon: false,
      checkbox: false,
      selectMode: 1,           // single selection
      clickFolderMode: 2,      // click folders to expand, not select
      activate: function (event, data) {
        if (data.node && !data.node.folder) {
          // send a single key (no JSON array needed unless you want it)
          Shiny.setInputValue("selected_nodes_vars2", data.node.key, { priority: "event" });
        }
      }
    });

    // Select default, if provided
    const tree = $tree.fancytree("getTree");
    if (message.default_selected && message.default_selected.length) {
      const key = message.default_selected[0];
      const node = tree.getNodeByKey(key);
      if (node) {
        console.log("STEP 1: Activating node:", node.key);
      
        node.makeVisible({ scrollIntoView: true });
        node.setActive();
      
        setTimeout(() => {
  const $box = $("#fancytree_vars_demo");
  const $el  = $(node.span);

  if ($el.length === 0) return;

  const elTop = $el.position().top;
  const offset = 130;   // ≈ 2 lines

  // ✅ Scroll directly to: element top - offset
  const newScroll = elTop - offset;

  console.log("Scrolling unconditionally to", newScroll);

  $box.scrollTop(newScroll);

}, 400);

      }

    }
  });
});
