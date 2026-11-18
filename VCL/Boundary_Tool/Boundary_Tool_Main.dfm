object FormBoundary: TFormBoundary
  Left = 0
  Top = 0
  Caption = 'TeeChart Boundary Tool Example'
  ClientHeight = 576
  ClientWidth = 897
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 576
    Align = alLeft
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 11
      Width = 47
      Height = 15
      Caption = '&Padding:'
      FocusControl = TBPadding
    end
    object TBPadding: TTrackBar
      Left = 8
      Top = 32
      Width = 150
      Height = 45
      LineSize = 5
      Max = 50
      Frequency = 5
      Position = 15
      TabOrder = 0
      OnChange = TBPaddingChange
    end
    object CBSmooth: TCheckBox
      Left = 8
      Top = 83
      Width = 97
      Height = 17
      Caption = '&Smooth'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBSmoothClick
    end
    object CBStyle: TComboBox
      Left = 8
      Top = 168
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Convex'
      OnChange = CBStyleChange
      Items.Strings = (
        'Convex'
        'Concave')
    end
    object TBSmoothSteps: TTrackBar
      Left = 8
      Top = 106
      Width = 150
      Height = 45
      LineSize = 5
      Max = 50
      Frequency = 5
      Position = 15
      TabOrder = 3
      OnChange = TBSmoothStepsChange
    end
  end
  object Chart1: TChart
    Left = 185
    Top = 0
    Width = 712
    Height = 576
    BackWall.Pen.Visible = False
    BottomWall.Brush.Gradient.EndColor = clSilver
    BottomWall.Brush.Gradient.StartColor = clGray
    BottomWall.Brush.Gradient.Visible = True
    BottomWall.Pen.Color = clGray
    BottomWall.Transparent = True
    Gradient.Direction = gdFromTopLeft
    Gradient.EndColor = clWhite
    Gradient.StartColor = clSilver
    LeftWall.Brush.Gradient.EndColor = clSilver
    LeftWall.Brush.Gradient.StartColor = clGray
    LeftWall.Brush.Gradient.Visible = True
    LeftWall.Color = clWhite
    LeftWall.Pen.Color = clGray
    LeftWall.Transparent = True
    Legend.Font.Height = -19
    Legend.Font.Name = 'Roboto Th'
    Legend.Frame.Visible = False
    Legend.Shadow.Visible = False
    Legend.Symbol.Pen.Visible = False
    Legend.Visible = False
    RightWall.Transparent = True
    Title.Alignment = taLeftJustify
    Title.Font.Color = clBlack
    Title.Font.Height = -19
    Title.Font.Name = 'Roboto Th'
    Title.Font.Style = [fsBold]
    Title.Font.Shadow.Color = clGray
    Title.Font.Shadow.HorizSize = 1
    Title.Font.Shadow.SmoothBlur = 2
    Title.Font.Shadow.VertSize = 1
    Title.Text.Strings = (
      'Drag points with the mouse')
    BottomAxis.Axis.Width = 0
    BottomAxis.Axis.Visible = False
    BottomAxis.Grid.Color = 14540253
    BottomAxis.Grid.Visible = False
    BottomAxis.LabelsFormat.Font.Color = clGray
    BottomAxis.LabelsFormat.Font.Height = -24
    BottomAxis.LabelsFormat.Font.Name = 'Roboto Th'
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.Ticks.Color = -8355712
    BottomAxis.TicksInner.Visible = False
    BottomAxis.Title.Font.Height = -13
    BottomAxis.Title.Font.Name = 'Roboto Th'
    DepthAxis.Title.Font.Height = -13
    DepthAxis.Title.Font.Name = 'Roboto Th'
    DepthTopAxis.Title.Font.Height = -13
    DepthTopAxis.Title.Font.Name = 'Roboto Th'
    Frame.Visible = False
    LeftAxis.Axis.Width = 0
    LeftAxis.Axis.Visible = False
    LeftAxis.Grid.Color = 14540253
    LeftAxis.LabelsFormat.Font.Color = clGray
    LeftAxis.LabelsFormat.Font.Height = -24
    LeftAxis.LabelsFormat.Font.Name = 'Roboto Th'
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.TickLength = 7
    LeftAxis.Ticks.Color = -8355712
    LeftAxis.TicksInner.Visible = False
    LeftAxis.Title.Font.Height = -13
    LeftAxis.Title.Font.Name = 'Roboto Th'
    RightAxis.Axis.Width = 0
    RightAxis.Grid.Visible = False
    RightAxis.LabelsFormat.Font.Color = clGray
    RightAxis.LabelsFormat.Font.Height = -31
    RightAxis.LabelsFormat.Font.Name = 'Roboto Th'
    RightAxis.MinorTicks.Visible = False
    RightAxis.Ticks.Visible = False
    RightAxis.Title.Font.Height = -13
    RightAxis.Title.Font.Name = 'Roboto Th'
    TopAxis.Title.Font.Height = -13
    TopAxis.Title.Font.Name = 'Roboto Th'
    View3D = False
    View3DOptions.Elevation = 350
    View3DOptions.Perspective = 55
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 2
    Color = clWhite
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 19
    object Series1: TPointSeries
      Marks.Font.Height = -24
      Marks.Font.Name = 'Roboto Th'
      Marks.Transparent = True
      Marks.Arrow.Visible = False
      Marks.Callout.Arrow.Visible = False
      Marks.Callout.ArrowHead = ahLine
      Marks.Callout.Distance = 8
      Marks.OnTop = True
      SeriesColor = 6724095
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TPointSeries
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series3: TPointSeries
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end
