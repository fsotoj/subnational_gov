Shiny.addCustomMessageHandler("addExportButton", function (message) {
  const selector = "#" + message.mapId;
  console.log("🔍 Looking for Leaflet widget:", selector);

  const widget = HTMLWidgets.find(selector);
  if (!widget || typeof widget.getMap !== "function") {
    console.warn("❌ Leaflet widget not found or not ready:", selector);
    return;
  }

  const map = widget.getMap();
  if (!map || typeof L === "undefined") {
    console.warn("❌ Leaflet library not ready for:", selector);
    return;
  }

  // Avoid duplicates
  if (map._easyPrintControl) {
    console.log("ℹ️ EasyPrint already exists on", selector);
    return;
  }

  // Add EasyPrint button
  map._easyPrintControl = L.easyPrint({
    title: "Download map",
    position: "topright",
    filename: "spp_map",
    exportOnly: true,
    hideControlContainer: false,
    sizeModes: ["A4Landscape"], // only reliable mode
  }).addTo(map);

  console.log("✅ EasyPrint button added to", selector);

  // ✅ Wait for the DOM element to exist, then restyle it
  const tryStyleButton = () => {
    const btn = document.querySelector(".leaflet-control-easyPrint-button");
    if (!btn) {
      setTimeout(tryStyleButton, 300); // retry until found
      return;
    }

    btn.innerHTML = '<i class="fa fa-camera"></i>';
    btn.style.background = "var(--orange)";
    btn.style.borderRadius = "50%";
    btn.style.width = "42px";
    btn.style.height = "42px";
    btn.style.boxShadow = "0 2px 6px rgba(0,0,0,0.25)";
    btn.style.color = "#111";
    btn.style.fontSize = "18px";
    btn.style.display = "flex";
    btn.style.alignItems = "center";
    btn.style.justifyContent = "center";
    btn.style.cursor = "pointer";
    btn.style.border = "none";

    btn.addEventListener("mouseenter", () => {
      btn.style.background = "var(--purple)";
      btn.style.color = "#fff";
    });
    btn.addEventListener("mouseleave", () => {
      btn.style.background = "var(--orange)";
      btn.style.color = "#111";
    });
  };

  tryStyleButton(); // trigger the styling loop
});
