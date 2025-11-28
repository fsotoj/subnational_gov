function safeGtag() {
  if (typeof gtag === "function") {
    gtag.apply(null, arguments);
  } else {
    console.warn("gtag not ready, event skipped:", arguments);
  }
}

let lastTab = "map_tab";
let lastTabStart = Date.now();
let isInitialMap = true;  

// Initial open event (landing tab)
safeGtag("event", "tab_open", { tab_name: "map_tab_initial" });

/******************************************************
 * TAB CHANGE HANDLING
 ******************************************************/
$(document).on("shiny:inputchanged", function (e) {
  if (e.name === "tabs") {

    const now = Date.now();
    const seconds = Math.round((now - lastTabStart) / 1000);

    // Label for tab being left
    let tabLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // Browser-side GA (safe)
    safeGtag("event", "tab_duration", {
      tab_name: tabLabel,
      seconds: seconds
    });

    if (lastTab === "map_tab" && isInitialMap) {
      isInitialMap = false;
    }

    // Switch tab
    lastTab = e.value;
    lastTabStart = now;

    // Label for tab being entered
    let nextLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    safeGtag("event", "tab_open", {
      tab_name: nextLabel
    });
  }
});

/******************************************************
 * WINDOW CLOSE / REFRESH HANDLER (SECURE VIA PROXY)
 ******************************************************/
window.addEventListener("beforeunload", function () {
  const now = Date.now();
  const seconds = Math.round((now - lastTabStart) / 1000);

  let tabLabel =
    lastTab === "map_tab" && isInitialMap
      ? "map_tab_initial"
      : lastTab;

  const payload = {
    events: [
      {
        name: "tab_duration",
        params: {
          tab_name: tabLabel,
          seconds: seconds
        }
      }
    ]
  };

  // Send to Shiny proxy endpoint (no secret exposed)
  navigator.sendBeacon("ga4proxy", JSON.stringify(payload));
});
