Shiny.addCustomMessageHandler("captureMapmap1", function (message) {
  const mapEl = document.getElementById("map1-map");
  if (!mapEl) {
    alert("Map element not found.");
    return;
  }

  // Access the Leaflet map via global 'L' (used by leaflet.js)
  // and remove map transforms to stabilize tiles.
  if (typeof L !== "undefined" && mapEl._leaflet_id) {
    const mapInstance = L.Map.prototype._leaflet_id
      ? L.Map.prototype._leaflet_id
      : L.DomUtil.get(mapEl);
    try {
      const mapPane = mapEl.querySelector(".leaflet-map-pane");
      if (mapPane) {
        mapPane.style.transform = "none";
      }
    } catch (e) {
      console.warn("Could not reset map transform:", e);
    }
  }

  document.body.classList.add("html2canvas-active");

  // Delay to ensure transform reset takes effect
  setTimeout(() => {
    html2canvas(mapEl, {
      useCORS: true,
      allowTaint: false,
      scale: 2,
      backgroundColor: "#ffffff",
      logging: false,
      onclone: (doc) => {
        const pane = doc.querySelector(".leaflet-map-pane");
        if (pane) pane.style.transform = "none";
      },
    })
      .then((canvas) => {
        document.body.classList.remove("html2canvas-active");

        const ctx = canvas.getContext("2d");
        ctx.font = "bold 48px Arial";
        ctx.fillStyle = "rgba(255, 0, 0, 0.9)";
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.shadowColor = "rgba(255,255,255,0.8)";
        ctx.shadowBlur = 10;
        ctx.fillText("CENTER TEST", canvas.width / 2, canvas.height / 2);

        const dataURL = canvas.toDataURL("image/png");
        const link = document.createElement("a");
        link.download = message.filename || "leaflet_map.png";
        link.href = dataURL;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      })
      .catch((error) => {
        document.body.classList.remove("html2canvas-active");
        console.error("Error capturing map:", error);
        alert("Screenshot failed. Try again.");
      });
  }, 200);
});
