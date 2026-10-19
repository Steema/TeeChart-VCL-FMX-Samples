## TeeChart Quadrant Tool

Splits a chart into four rectangles, each rectangle with title and custom properties like color etc.
The two center lines (horizontal and vertical) can be configured with the CrossHair property.

### Basic Usage:

```delphi
var Quadrant:=TQuadrantTool.Create(Self);

  Quadrant.Quadrant0.Text:='Zero';
  Quadrant.Quadrant1.Text:='One';
  Quadrant.Quadrant2.Text:='Two';
  Quadrant.Quadrant3.Text:='Three';

  Quadrant.Quadrant1.Font.Size:=14;
  Quadrant.Quadrant2.Font.Size:=16;
  Quadrant.Quadrant2.Font.Color:=clRed;

  Quadrant.Quadrant0.Color:=clWebNavajoWhite;
  Quadrant.Quadrant1.Color:=clWebLavender;
  Quadrant.Quadrant2.Color:=clWebAzure;
  Quadrant.Quadrant3.Color:=clWebDarkKhaki;

  Chart1.Tools.Add(Quadrant);
```

<img width="940" height="709" alt="image" src="https://github.com/user-attachments/assets/c7daaeeb-5361-45f1-b5ad-9df75e6b9099" />
