object frmAnalytics: TfrmAnalytics
  Left = 0
  Top = 0
  Caption = 'SmartCore AI and TeeChart'
  ClientHeight = 760
  ClientWidth = 1120
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
    Width = 1120
    Height = 118
    Align = alTop
    TabOrder = 0
    ExplicitTop = -6
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 47
      Height = 15
      Caption = 'Provider:'
    end
    object Label2: TLabel
      Left = 112
      Top = 16
      Width = 37
      Height = 15
      Caption = 'Model:'
    end
    object Label3: TLabel
      Left = 240
      Top = 16
      Width = 43
      Height = 15
      Caption = 'API Key:'
    end
    object BSample: TButton
      Left = 384
      Top = 36
      Width = 97
      Height = 25
      Caption = 'Load Sample'
      TabOrder = 0
      OnClick = BSampleClick
    end
    object BAsk: TButton
      Left = 511
      Top = 36
      Width = 75
      Height = 25
      Caption = '&Ask AI'
      TabOrder = 1
      OnClick = BAskClick
    end
    object BCancel: TButton
      Left = 592
      Top = 36
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Enabled = False
      TabOrder = 2
      OnClick = BCancelClick
    end
    object Provider: TComboBox
      Left = 8
      Top = 37
      Width = 95
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Gemini'
      OnChange = ProviderChange
      Items.Strings = (
        'Gemini'
        'OpenAI'
        'Claude'
        'Ollama')
    end
    object Prompt: TEdit
      Left = 8
      Top = 74
      Width = 1057
      Height = 23
      TabOrder = 4
    end
    object Model: TEdit
      Left = 112
      Top = 37
      Width = 121
      Height = 23
      TabOrder = 5
      Text = 'gemini-3.1-flash-lite'
    end
    object APIKey: TEdit
      Left = 240
      Top = 37
      Width = 121
      Height = 23
      PasswordChar = '*'
      TabOrder = 6
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 671
    Width = 1120
    Height = 89
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Chart1: TChart
    Left = 0
    Top = 118
    Width = 1120
    Height = 306
    BackWall.Brush.Gradient.Direction = gdBottomTop
    BackWall.Brush.Gradient.EndColor = clWhite
    BackWall.Brush.Gradient.StartColor = 15395562
    BackWall.Brush.Gradient.Visible = True
    BackWall.Transparent = False
    Foot.Font.Color = clBlue
    Foot.Font.Name = 'Verdana'
    Gradient.Direction = gdBottomTop
    Gradient.EndColor = clWhite
    Gradient.MidColor = 15395562
    Gradient.StartColor = 15395562
    Gradient.Visible = True
    LeftWall.Color = clLightyellow
    Legend.Font.Name = 'Verdana'
    Legend.Shadow.Transparency = 0
    RightWall.Color = clLightyellow
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'TChart')
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = clDarkgray
    BottomAxis.LabelsFormat.Font.Name = 'Verdana'
    BottomAxis.TicksInner.Color = clDarkgray
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = clDarkgray
    DepthAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthAxis.TicksInner.Color = clDarkgray
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = clDarkgray
    DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthTopAxis.TicksInner.Color = clDarkgray
    DepthTopAxis.Title.Font.Name = 'Verdana'
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = clDarkgray
    LeftAxis.LabelsFormat.Font.Name = 'Verdana'
    LeftAxis.TicksInner.Color = clDarkgray
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = clDarkgray
    RightAxis.LabelsFormat.Font.Name = 'Verdana'
    RightAxis.TicksInner.Color = clDarkgray
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = clDarkgray
    TopAxis.LabelsFormat.Font.Name = 'Verdana'
    TopAxis.TicksInner.Color = clDarkgray
    TopAxis.Title.Font.Name = 'Verdana'
    Align = alClient
    TabOrder = 2
    ExplicitTop = 124
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TBarSeries
      Marks.OnTop = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 424
    Width = 1120
    Height = 247
    Align = alBottom
    TabOrder = 3
  end
  object AIConnection1: TAIConnection
    Left = 256
    Top = 504
  end
  object AIChatAgent1: TAIChatAgent
    Connection = AIConnection1
    Tools.Functions = <>
    Tools.Agents = <>
    Tools.MCPServers = <>
    Skills = <>
    OnError = AIChatAgent1Error
    OnCancelled = AIChatAgent1Cancelled
    OnStarted = AIChatAgent1Started
    OnPartial = AIChatAgent1Partial
    OnComplete = AIChatAgent1Complete
    Left = 368
    Top = 504
  end
end
