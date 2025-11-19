
function safeGtag() {
  if (typeof gtag === "function") {
    gtag.apply(null, arguments);
  } else {
    console.warn("gtag not ready, event skipped:", arguments);
  }
}

let lastTab = "map_tab";
let lastTabStart = Date.now();
let isInitialMap = true;  // <--- KEY FLAG

// 1) Send initial tab_open (as initial load)
safeGtag("event", "tab_open", { tab_name: "map_tab_initial" });

/******************************************************
 * TAB CHANGE TRACKING
 ******************************************************/
$(document).on("shiny:inputchanged", function (e) {
  if (e.name === "tabs") {

    const now = Date.now();
    const seconds = Math.round((now - lastTabStart) / 1000);

    // Determine proper label for the tab we are LEAVING
    let tabLabel;

    if (lastTab === "map_tab" && isInitialMap) {
      tabLabel = "map_tab_initial";
      isInitialMap = false;  // After first leave, switch off
    } else {
      tabLabel = lastTab;
    }

    // Send duration event
    safeGtag("event", "tab_duration", {
      tab_name: tabLabel,
      seconds: seconds
    });

    // Prepare new tab
    lastTab = e.value;
    lastTabStart = now;

    // Determine proper label for the tab we are ENTERING
    let nextLabel =
      lastTab === "map_tab" && isInitialMap
        ? "map_tab_initial"
        : lastTab;

    // Send open event
    safeGtag("event", "tab_open", {
      tab_name: nextLabel
    });
  }
});

/******************************************************
 * WINDOW CLOSE / REFRESH HANDLER
 ******************************************************/
window.addEventListener("beforeunload", function () {
  const now = Date.now();
  const seconds = Math.round((now - lastTabStart) / 1000);

  // Determine label for closing tab
  let tabLabel;

  if (lastTab === "map_tab" && isInitialMap) {
    tabLabel = "map_tab_initial";
    isInitialMap = false;
  } else {
    tabLabel = lastTab;
  }

  // Prepare Measurement Protocol payload
  const payload = {
    client_id: (window.gtagClientId || ""),
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

  const url =
    "https://www.google-analytics.com/mp/collect?measurement_id=G-2D6B3PWVGG&api_secret=ZKNkvKGbTV6504car3fmFw";

  navigator.sendBeacon(url, JSON.stringify(payload));
});
