/******************************************************
 * SECURE TAB ANALYTICS (Proxy Version)
 * -------------------------------------
 * - No API secret in browser
 * - No client_id needed in browser
 * - Unload tracked reliably via sendBeacon → Shiny proxy
 * - Initial map load labeled as "map_tab_initial"
 ******************************************************/

function safeGtag() {
  if (typeof gtag === "function") {
    gtag.apply(null, arguments);
  } else {
    console.warn("gtag not ready (skipped)", arguments);
  }
}

let lastTab = "map_tab";
let lastTabStart = Date.now();
let isInitialMap = true;

// Initial open event (landing tab)
safeGtag("event", "tab_open", { tab_name: "map_tab_initial" });

/******************************************************
 * TAB CHANGE HANDLER
 ******************************************************/
$(document).on("shiny:inputchanged", function (e) {
  if (e.name === "tabs") {
    const now = Date.now();
    const seconds = Math.round((now - lastTabStart) / 1000);

    // Determine label for tab being left
    const outgoingLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // Send tab_duration event (safe)
    safeGtag("event", "tab_duration", {
      tab_name: outgoingLabel,
      seconds: seconds
    });

    if (lastTab === "map_tab" && isInitialMap) {
      isInitialMap = false;
    }

    // Switch tab
    lastTab = e.value;
    lastTabStart = now;

    // Determine label for new tab
    const incomingLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // Send tab_open for new tab
    safeGtag("event", "tab_open", {
      tab_name: incomingLabel
    });
  }
});

/******************************************************
 * UNLOAD HANDLER → SEND TO SHINY PROXY (secure)
 ******************************************************/
window.addEventListener("beforeunload", function () {
  const now = Date.now();
  const seconds = Math.round((now - lastTabStart) / 1000);

  // Determine final label
  const finalLabel =
    lastTab === "map_tab" && isInitialMap
      ? "map_tab_initial"
      : lastTab;

  const payload = {
    events: [
      {
        name: "tab_duration",
        params: {
          tab_name: finalLabel,
          seconds: seconds
        }
      }
    ]
  };

  // Send to Shiny proxy (safe, no secret exposed)
  navigator.sendBeacon(
    "ga4proxy",                  // Shiny secure endpoint
    JSON.stringify(payload)
  );
});
