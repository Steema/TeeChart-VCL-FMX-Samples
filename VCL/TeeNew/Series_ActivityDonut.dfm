inherited ActivityGaugeForm: TActivityGaugeForm
  Left = 192
  Top = 125
  Width = 612
  Height = 597
  Caption = 'Animated Activity Gauge with TeeChart'
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited BaseSplitter1: TSplitter
    Width = 596
  end
  inherited Memo1: TMemo
    Width = 596
    TabOrder = 1
  end
  inherited Panel1: TPanel
    Width = 596
    TabOrder = 2
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 81
      Height = 25
      Caption = '&Start !'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  inherited Chart1: TChart
    Width = 596
    Height = 402
    BackWall.Brush.Gradient.Direction = gdBottomTop
    BackWall.Brush.Gradient.EndColor = 7895160
    BackWall.Brush.Gradient.StartColor = 4605510
    BackWall.Brush.Gradient.Visible = True
    BackWall.Pen.Visible = False
    BackWall.Transparent = False
    Foot.Font.Color = clWhite
    Foot.Font.Name = 'Verdana'
    Gradient.Direction = gdBottomTop
    Gradient.EndColor = 4605510
    Gradient.StartColor = 4605510
    Gradient.Visible = True
    LeftWall.Color = 14745599
    Legend.Brush.Gradient.Direction = gdBottomTop
    Legend.Brush.Gradient.EndColor = 7895160
    Legend.Brush.Gradient.StartColor = 4605510
    Legend.Brush.Gradient.Visible = True
    Legend.Font.Color = clWhite
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.Transparency = 0
    Legend.Visible = False
    RightWall.Color = 14745599
    Title.Font.Color = clWhite
    Title.Font.Name = 'Verdana'
    Title.Visible = False
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = 11119017
    BottomAxis.LabelsFormat.Font.Color = clWhite
    BottomAxis.Ticks.Color = 8553090
    BottomAxis.TicksInner.Color = 11119017
    BottomAxis.Title.Font.Color = clWhite
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = 11119017
    DepthAxis.LabelsFormat.Font.Color = clWhite
    DepthAxis.Ticks.Color = 8553090
    DepthAxis.TicksInner.Color = 11119017
    DepthAxis.Title.Font.Color = clWhite
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = 11119017
    DepthTopAxis.LabelsFormat.Font.Color = clWhite
    DepthTopAxis.Ticks.Color = 8553090
    DepthTopAxis.TicksInner.Color = 11119017
    DepthTopAxis.Title.Font.Color = clWhite
    DepthTopAxis.Title.Font.Name = 'Verdana'
    Frame.Visible = False
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = 11119017
    LeftAxis.LabelsFormat.Font.Color = clWhite
    LeftAxis.Ticks.Color = 8553090
    LeftAxis.TicksInner.Color = 11119017
    LeftAxis.Title.Font.Color = clWhite
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = 11119017
    RightAxis.LabelsFormat.Font.Color = clWhite
    RightAxis.Ticks.Color = 8553090
    RightAxis.TicksInner.Color = 11119017
    RightAxis.Title.Font.Color = clWhite
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = 11119017
    TopAxis.LabelsFormat.Font.Color = clWhite
    TopAxis.Ticks.Color = 8553090
    TopAxis.TicksInner.Color = 11119017
    TopAxis.Title.Font.Color = clWhite
    TopAxis.Title.Font.Name = 'Verdana'
    View3DOptions.Elevation = 315
    View3DOptions.Orthogonal = False
    View3DOptions.Perspective = 0
    View3DOptions.Rotation = 360
    TabOrder = 0
    ColorPaletteIndex = 14
    object Series1: TDonutSeries
      HoverElement = []
      Marks.Font.Color = clWhite
      Marks.Transparent = True
      Marks.Visible = False
      Marks.Tail.Margin = 2
      SeriesColor = 14483299
      XValues.Order = loAscending
      YValues.Name = 'Pie'
      YValues.Order = loNone
      Frame.InnerBrush.Style = bsClear
      Frame.InnerBrush.BackColor = clRed
      Frame.InnerBrush.Gradient.EndColor = clGray
      Frame.InnerBrush.Gradient.MidColor = clWhite
      Frame.InnerBrush.Gradient.StartColor = clSilver
      Frame.InnerBrush.Gradient.Visible = True
      Frame.MiddleBrush.Style = bsClear
      Frame.MiddleBrush.BackColor = clYellow
      Frame.MiddleBrush.Gradient.EndColor = 8553090
      Frame.MiddleBrush.Gradient.MidColor = clWhite
      Frame.MiddleBrush.Gradient.StartColor = clGray
      Frame.MiddleBrush.Gradient.Visible = True
      Frame.OuterBrush.Style = bsClear
      Frame.OuterBrush.BackColor = clGreen
      Frame.OuterBrush.Gradient.EndColor = 4210752
      Frame.OuterBrush.Gradient.MidColor = clWhite
      Frame.OuterBrush.Gradient.StartColor = clSilver
      Frame.OuterBrush.Gradient.Visible = True
      Frame.Visible = True
      Frame.Width = 3
      Shadow.Visible = False
      Bevel.Bright = 29
      ColorEachPoint = False
      Gradient.EndColor = 9896191
      Gradient.StartColor = 9896191
      MultiPie = mpDisabled
      OtherSlice.Legend.Visible = False
      PiePen.Color = 4144959
      PiePen.Width = 4
      Transparency = 1
      DonutPercent = 47
    end
    object Series2: TDonutSeries
      HoverElement = []
      Marks.Visible = False
      Marks.Tail.Margin = 2
      SeriesColor = 65456
      XValues.Order = loAscending
      YValues.Name = 'Pie'
      YValues.Order = loNone
      Frame.InnerBrush.Style = bsClear
      Frame.InnerBrush.BackColor = clRed
      Frame.InnerBrush.Gradient.EndColor = clGray
      Frame.InnerBrush.Gradient.MidColor = clWhite
      Frame.InnerBrush.Gradient.StartColor = 4210752
      Frame.InnerBrush.Gradient.Visible = True
      Frame.MiddleBrush.BackColor = clYellow
      Frame.MiddleBrush.Gradient.EndColor = 8553090
      Frame.MiddleBrush.Gradient.MidColor = clWhite
      Frame.MiddleBrush.Gradient.StartColor = clGray
      Frame.MiddleBrush.Gradient.Visible = True
      Frame.OuterBrush.BackColor = clGreen
      Frame.OuterBrush.Gradient.EndColor = 4210752
      Frame.OuterBrush.Gradient.MidColor = clWhite
      Frame.OuterBrush.Gradient.StartColor = clSilver
      Frame.OuterBrush.Gradient.Visible = True
      Frame.Width = 4
      ColorEachPoint = False
      Gradient.Direction = gdTopBottom
      Gradient.EndColor = 753908
      Gradient.MidColor = 16059031
      Gradient.StartColor = 14540754
      MultiPie = mpDisabled
      OtherSlice.Legend.Visible = False
      PiePen.Color = 4868682
      PiePen.Width = 5
      DonutPercent = 64
    end
    object Series3: TDonutSeries
      HoverElement = []
      Marks.Visible = False
      Marks.Tail.Margin = 2
      SeriesColor = 10158300
      XValues.Order = loAscending
      YValues.Name = 'Pie'
      YValues.Order = loNone
      Frame.InnerBrush.Style = bsClear
      Frame.InnerBrush.BackColor = clRed
      Frame.InnerBrush.Gradient.EndColor = clGray
      Frame.InnerBrush.Gradient.MidColor = clWhite
      Frame.InnerBrush.Gradient.StartColor = 4210752
      Frame.InnerBrush.Gradient.Visible = True
      Frame.MiddleBrush.BackColor = clYellow
      Frame.MiddleBrush.Gradient.EndColor = 8553090
      Frame.MiddleBrush.Gradient.MidColor = clWhite
      Frame.MiddleBrush.Gradient.StartColor = clGray
      Frame.MiddleBrush.Gradient.Visible = True
      Frame.OuterBrush.BackColor = clGreen
      Frame.OuterBrush.Gradient.EndColor = 4210752
      Frame.OuterBrush.Gradient.MidColor = clWhite
      Frame.OuterBrush.Gradient.StartColor = clSilver
      Frame.OuterBrush.Gradient.Visible = True
      Frame.Width = 4
      ColorEachPoint = False
      Gradient.Direction = gdTopBottom
      Gradient.EndColor = 753908
      Gradient.MidColor = 16059031
      Gradient.StartColor = 14540754
      MultiPie = mpDisabled
      OtherSlice.Legend.Visible = False
      PiePen.Color = 4868682
      PiePen.Width = 5
      DonutPercent = 82
    end
    object ChartTool1: TAnnotationTool
      Position = ppCenter
      Shape.Alignment = taCenter
      Shape.Font.Color = clSilver
      Shape.Font.Height = -36
      Shape.Font.Name = 'Yu Gothic Light'
      Shape.Font.Quality = fqClearType
      Shape.Font.Shadow.HorizSize = 2
      Shape.Font.Shadow.VertSize = 2
      Shape.Font.Shadow.Visible = False
      Shape.Text = '33 %'
      Shape.Transparent = True
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 328
    Top = 16
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer2Timer
    Left = 336
    Top = 80
  end
  object Timer3: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer3Timer
    Left = 324
    Top = 152
  end
end