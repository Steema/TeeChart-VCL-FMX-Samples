�
 TAXISOPAQUEZONE 0�	  TPF0�TAxisOpaqueZoneAxisOpaqueZonePixelsPerInch`
TextHeight �TMemoMemo1Lines.Strings2When using multiple axes and doing zoom or scroll,=series points can display outside the axes "zones" or limits. 1You can prevent this with a small code at Series -events: BeforeDrawValues and AfterDrawValues.   �TPanelPanel1 TLabelLabel1Left� TopWidthHeightCaption&Scroll:FocusControl
ScrollBar1  	TCheckBox	CheckBox1LeftTopWidthaHeightCaption&Opaque zonesChecked	State	cbCheckedTabOrder OnClickCheckBox1Click  
TScrollBar
ScrollBar1Left� TopWidth� HeightPageSize Position2TabOrderOnChangeScrollBar1Change   �TChartChart1Legend.VisibleTitle.Visible
CustomAxes
Axis.ColorclLime
Horizontal	OtherSideStartPosition       �@EndPosition       �@ 
Axis.ColorclBlue
Horizontal	OtherSideStartPosition       �@EndPosition       �@ 
Axis.ColorclRed
Horizontal	OtherSideStartPosition       �@  LeftAxis.EndPosition       �@View3DColorPalettew�� ��h �G ��� ^s� P�� �;A �� ��� ��� ��� ���   TLineSeriesSeries1SeriesColorclBlackBrush.BackColor	clDefaultPointer.InflateMargins	Pointer.StylepsRectangleXValues.NameXXValues.OrderloAscendingYValues.NameYYValues.OrderloNone  TLineSeriesSeries2Marks.Callout.Length SeriesColorclGreenVertAxisaCustomVertAxisBrush.BackColor	clDefaultClickableLinePointer.InflateMargins	Pointer.StylepsRectangleXValues.NameXXValues.OrderloAscendingYValues.NameYYValues.OrderloNoneCustomVertAxis   TLineSeriesSeries4SeriesColorclBlueVertAxisaCustomVertAxisBrush.BackColor	clDefaultPointer.InflateMargins	Pointer.StylepsRectangleXValues.NameXXValues.OrderloAscendingYValues.NameYYValues.OrderloNoneCustomVertAxis  TLineSeriesSeries3Marks.Callout.LengthVertAxisaCustomVertAxisBrush.BackColor	clDefaultPointer.InflateMargins	Pointer.StylepsRectangleXValues.NameXXValues.OrderloAscendingYValues.NameYYValues.OrderloNoneCustomVertAxis  TColorLineTool
ChartTool1	AllowDragStyle	clMinimumValue       �@AxisID TAnnotationTool Shape.AlignmenttaCenterShape.Shadow.Visible   TColorLineTool
ChartTool2	AllowDragStyle	clMinimumValue       �@AxisID TAnnotationTool Shape.AlignmenttaCenterShape.Shadow.Visible   TColorLineTool
ChartTool3	AllowDragStyle	clMinimumValue      ��@AxisID TAnnotationTool Shape.AlignmenttaCenterShape.Shadow.Visible     