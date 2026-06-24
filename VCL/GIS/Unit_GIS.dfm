object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeChart GIS Example'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 89
      Height = 15
      Caption = '&Map Web Server:'
      FocusControl = CBMapServer
    end
    object CBMapServer: TComboBox
      Left = 115
      Top = 10
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'Esri Satellit'
      OnChange = CBMapServerChange
      Items.Strings = (
        'Standard OSM'
        'Esri Satellit'
        'Carto Dark'
        'Carto Positron'
        'Carto Voyager')
    end
    object Button1: TButton
      Left = 336
      Top = 9
      Width = 105
      Height = 25
      Caption = '&Disk cache...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 464
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 41
    Width = 624
    Height = 400
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
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
end
