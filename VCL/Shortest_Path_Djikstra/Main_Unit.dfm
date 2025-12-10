object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeChart Shortest Path Dijkstra Algorithm '
  ClientHeight = 514
  ClientWidth = 1265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 641
    Top = 41
    Height = 473
    ExplicitLeft = 488
    ExplicitTop = 240
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1265
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 120
      Top = 13
      Width = 34
      Height = 15
      Caption = 'Label1'
    end
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Run !'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 41
    Width = 641
    Height = 473
    Legend.Visible = False
    Title.Text.Strings = (
      'XY ')
    View3D = False
    Align = alLeft
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series2: TLineSeries
      SeriesColor = clBlue
      Brush.BackColor = clDefault
      LinePen.Width = 4
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series1: TPointSeries
      Marks.Frame.Visible = False
      Marks.Transparent = True
      Marks.Visible = True
      Marks.AutoPosition = False
      Marks.Callout.Distance = -5
      ClickableLine = False
      Pointer.HorizSize = 10
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 10
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object StartFinish: TPointSeries
      ClickableLine = False
      Pointer.Brush.Style = bsClear
      Pointer.HorizSize = 9
      Pointer.InflateMargins = True
      Pointer.Pen.Color = clRed
      Pointer.Pen.Width = 3
      Pointer.Style = psCircle
      Pointer.VertSize = 9
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Chart2: TChart
    Left = 644
    Top = 41
    Width = 621
    Height = 473
    Title.Text.Strings = (
      'TChart')
    Chart3DPercent = 78
    Align = alClient
    TabOrder = 2
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
end
