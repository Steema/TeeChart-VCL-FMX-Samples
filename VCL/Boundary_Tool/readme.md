## TeeChart Boundary Tool

Display a polygon around a set of points.

Options:

- Convex or concave polygon
- Smooth
- Padding extra margins
- Formatting brush, border, transparency

Points can be:

- All or a portion of one or more Series
- A custom array of XY TPoint

Functionality:

- Mouse hover, click
- Obtain the calculated boundary points

Reusable code:

- TTeeCanvas.ConvexHull to calculate a convex polygon around any set of points
- TSplineCatmull to smooth a polygon
- TBoundaryTool.ResizeFromCenter to expand or shrink any polygon
- TBoundaryTool.Centroid to calculate the center xy point of a polygon
- TConcaveHull class to calculate a concave perimeter polygon
- TChartSeries.ToPoints to return an array of XY float points from a given series (all or a portion of the series points)


