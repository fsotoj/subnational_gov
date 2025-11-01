// leaflet-export.js — enhanced with text overlay on print
Shiny.addCustomMessageHandler("addExportButton", function (message) {
  const selector = "#" + message.mapId;

  // Try to find the Leaflet widget created by htmlwidgets
  const widget = HTMLWidgets.find(selector);
  if (!widget || typeof widget.getMap !== "function") {
    console.warn("EasyPrint: Leaflet widget not found for", selector);
    return;
  }

  const map = widget.getMap();
  if (!map || typeof L === "undefined") {
    console.warn("EasyPrint: Leaflet library not ready for", selector);
    return;
  }

  // Avoid duplicates
  if (map._easyPrintControl) {
    return;
  }

  // Add EasyPrint control
  map._easyPrintControl = L.easyPrint({
    title: "Download map",
    position: "bottomright",
    filename: "spp_map",
    exportOnly: true,
    hideControlContainer: false,
    sizeModes: ["A4Landscape"],
  }).addTo(map);

  // Add print event hooks to overlay text
  map.on("easyPrint-start", function () {
    
    const ctrl = map.getContainer().querySelector(".leaflet-control-easyPrint");
      if (ctrl) {
        ctrl.dataset.wasVisible = "true";
        ctrl.style.visibility = "hidden";   // visibility works better with html2canvas
        ctrl.style.opacity = "0";
      }

    // Create a watermark / label element
    const textOverlay = document.createElement("div");
    textOverlay.id = "map-print-text";
    textOverlay.textContent = message.printText || "Source: Subnational Politics Project."; // custom text
    Object.assign(textOverlay.style, {
      position: "absolute",
      bottom: "15px",
      left: "15px",
      color: "#222",
      backgroundColor: "rgba(255,255,255,0.8)",
      padding: "3px 8px",
      fontSize: "12px",
      borderRadius: "4px",
      fontFamily: "sans-serif",
      zIndex: "99999",
    });

    // Attach to the map container
    map.getContainer().appendChild(textOverlay);
  });

  map.on("easyPrint-finished", function () {
    const textOverlay = document.getElementById("map-print-text");
    if (textOverlay) textOverlay.remove();
  
    // Function to safely restore the EasyPrint control
    const restoreButton = () => {
      const ctrl = map.getContainer().querySelector(".leaflet-control-easyPrint");
      if (ctrl) {
        ctrl.style.visibility = "visible";
        ctrl.style.opacity = "1";
        ctrl.style.display = "block";
        console.log("🔁 EasyPrint button restored");
        return true;
      }
      return false;
    };
  
    // Try immediately
    if (!restoreButton()) {
      // If the map re-renders slower, retry a few times
      let attempts = 0;
      const interval = setInterval(() => {
        attempts++;
        if (restoreButton() || attempts > 10) clearInterval(interval);
      }, 200);
    }
  });

  console.log("✅ EasyPrint button added to", selector);
});
