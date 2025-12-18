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
    Height = 64
    Align = alTop
    TabOrder = 0
    object LPathInfo: TLabel
      Left = 120
      Top = 13
      Width = 51
      Height = 15
      Caption = 'LPathInfo'
    end
    object Label2: TLabel
      Left = 341
      Top = 36
      Width = 76
      Height = 15
      Caption = '&Max Distance: '
      FocusControl = TrackBar1
    end
    object LDistance: TLabel
      Left = 583
      Top = 36
      Width = 18
      Height = 15
      Caption = '200'
    end
    object LTotalLength: TLabel
      Left = 120
      Top = 34
      Width = 68
      Height = 15
      Caption = 'LTotalLength'
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
      Left = 341
      Top = 13
      Width = 118
      Height = 17
      Caption = 'Use &edges (roads)'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBEdgesClick
    end
    object CBWeights: TCheckBox
      Left = 558
      Top = 13
      Width = 122
      Height = 17
      Caption = 'Use weights (cost)'
      TabOrder = 2
      OnClick = CBWeightsClick
    end
    object CBDirection: TCheckBox
      Left = 465
      Top = 13
      Width = 87
      Height = 17
      Caption = 'Both Ways'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBDirectionClick
    end
    object TrackBar1: TTrackBar
      Left = 423
      Top = 36
      Width = 154
      Height = 22
      Max = 2000
      PageSize = 250
      Frequency = 75
      Position = 200
      TabOrder = 4
      ThumbLength = 12
      OnChange = TrackBar1Change
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 64
    Width = 729
    Height = 450
    Legend.Visible = False
    Title.Text.Strings = (
      'XY ')
    View3D = False
    Align = alClient
    TabOrder = 1
    OnMouseUp = Chart1MouseUp
    ExplicitTop = 41
    ExplicitHeight = 473
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object SeriesPath: TLineSeries
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
    object Series1: TPointSeries
      ColorEachPoint = True
      Marks.Frame.Visible = False
      Marks.Transparent = True
      BeforeDrawValues = Series1BeforeDrawValues
      ClickableLine = False
      Pointer.HorizSize = 16
      Pointer.InflateMargins = True
      Pointer.Style = psCircle
      Pointer.VertSize = 16
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      OnGetPointerStyle = Series1GetPointerStyle
    end
  end
end
