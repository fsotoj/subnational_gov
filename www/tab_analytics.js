/******************************************************
 * USER ACTIVITY TRACKER (IDLE TIME HANDLING)
 ******************************************************/
let lastActivity = Date.now();
let idleThreshold = 60 * 1000; // 60 seconds of inactivity = IDLE

function refreshActivity() {
  lastActivity = Date.now();
}

// Register activity events
["mousemove", "keydown", "click", "scroll", "touchstart"].forEach(event => {
  window.addEventListener(event, refreshActivity, { passive: true });
});

// Returns ONLY active seconds
function getActiveSeconds() {
  const now = Date.now();
  const rawSeconds = Math.round((now - lastTabStart) / 1000);

  // Compute idle time
  const idleTime = now - lastActivity;

  if (idleTime > idleThreshold) {
    // User became idle → only count activity BEFORE going idle
    const activeMillis = idleThreshold;
    return Math.round(activeMillis / 1000);
  }

  return rawSeconds;
}








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
    const seconds = getActiveSeconds();

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
  const seconds = getActiveSeconds();

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

//  navigator.sendBeacon(    "ga",     JSON.stringify(payload)  );
  
  
navigator.sendBeacon(window.location.pathname + "ga", body)


});