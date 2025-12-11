object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeChart Shortest Path Dijkstra Algorithm '
  ClientHeight = 514
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 729
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
    object CBEdges: TCheckBox
      Left = 320
      Top = 15
      Width = 129
      Height = 17
      Caption = 'Use &edges (roads)'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBEdgesClick
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 41
    Width = 729
    Height = 473
    Legend.Visible = False
    Title.Text.Strings = (
      'XY ')
    View3D = False
    Align = alClient
    TabOrder = 1
    OnMouseUp = Chart1MouseUp
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
      BeforeDrawValues = Series1BeforeDrawValues
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
      Pointer.HorizSize = 12
      Pointer.InflateMargins = True
      Pointer.Pen.Color = clRed
      Pointer.Pen.Width = 5
      Pointer.Style = psCircle
      Pointer.VertSize = 12
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end
