function showGraph(divId, filename) {
  var g = new OpenLayers.Map(divId, {});
  var options = {numZoomLevels: 20};
  var b = new OpenLayers.Bounds(0, 0, 1100, 50);
  var s = new OpenLayers.Size(g.getSize().w / 20, g.getSize().h / 20);
  var layer = new OpenLayers.Layer.Image('Graph', filename, b, s, options);
  g.addLayer(layer);
  g.zoomToMaxExtent();
}
