Shiny.addCustomMessageHandler("captureMapmap1", function(message) {
  const mapElement = document.getElementById("map1-map");
  if (!mapElement) {
    alert("Map element not found.");
    return;
  }

  // Add a temporary class to remove transforms during capture
  document.body.classList.add("html2canvas-active");

  html2canvas(mapElement, {
    useCORS: true,
    allowTaint: false,        // safer: prevents corrupt image
    scale: 2,
    backgroundColor: "#ffffff",
    logging: false,
    windowWidth: mapElement.scrollWidth,
    windowHeight: mapElement.scrollHeight
  })
  .then(function(canvas) {
    document.body.classList.remove("html2canvas-active");

    // Create PNG and trigger download only if valid
    try {
      const dataURL = canvas.toDataURL("image/png");
      if (!dataURL.startsWith("data:image/png")) {
        throw new Error("Invalid PNG data.");
      }

      const link = document.createElement("a");
      link.download = message.filename || "leaflet_map.png";
      link.href = dataURL;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    } catch (err) {
      console.error("PNG creation failed:", err);
      alert("Screenshot failed. Try again.");
    }
  })
  .catch(function(error) {
    document.body.classList.remove("html2canvas-active");
    console.error("Error capturing map:", error);
    alert("Error capturing map. Please try again.");
  });
});
