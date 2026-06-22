## TeeChart GIS Example

<img width="470" height="355" alt="image" src="https://github.com/user-attachments/assets/ddcfc7e8-8414-467b-b723-bd7001fcbd62" />

### A Series class to obtain and paint graphical tiles from web map servers:

-  [Standard OpenData OSM](https://www.google.com/search?q=https://www.openstreetmap.org/copyright)
-  [Google Maps Satellit](https://about.google/brand-resource-center/products-and-services/geo-guidelines/)
-  Google Hibrid
-  Google Terrain
-  [Esri Satellit](https://www.esri.com/legal/software-license)
-  [Carto Dark](https://www.google.com/search?q=https://carto.com/terms/)
-  Carto Positron
-  Carto Voyager

Note: Copyright attribution and license policies might apply, please check the above links with the respective servers.

### Code:

```delphi
  // Drop a normal TChart control on a Form1

  uses TeeGIS;
  var GIS : TGISRaster;

  GIS:=TGISRaster.Create(Self);
  GIS.ParentChart:=Chart1;

  // Cosmetic
  GIS.HorizAxis:=aBothHorizAxis;
  GIS.VertAxis:=aBothVertAxis;

  // Initial Latitude and Longitude
  GIS.SetBounds(41,42, 1.4,2.8);

  // Enable zoooming in-out with the mouse wheel
  Chart1.Panning.MouseWheel:=pmwNone;
  Chart1.Zoom.MouseWheel:=pmwNormal;

  // Optional: Disk cache to store map graphic tiles (to avoid http requests)

  // NOTE: This usually requires a strong license agreement with the map imagery provider.

  var tmpFolder:=TPath.Combine(TPath.GetTempPath,'TeeGISCache');
  ForceDirectories(tmpFolder);
  GIS.Path:=tmpFolder;

 

```

### Scrolling, zooming, drawing items over series, using other series on top, etc, etc, are just possible.


<img width="940" height="709" alt="image" src="https://github.com/user-attachments/assets/8c3a935f-1406-4970-ad24-929dbd460a32" />
