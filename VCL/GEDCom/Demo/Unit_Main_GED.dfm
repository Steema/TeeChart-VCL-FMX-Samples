object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeChart and GEDCom Genealogy Data'
  ClientHeight = 441
  ClientWidth = 942
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
    Width = 942
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 232
    ExplicitTop = 224
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 53
      Height = 15
      Caption = '&GEDCOM:'
      FocusControl = CBFile
    end
    object CBFile: TComboBox
      Left = 79
      Top = 10
      Width = 518
      Height = 23
      TabOrder = 0
      OnChange = CBFileChange
      Items.Strings = (
        '..\..\Samples\Royal92.ged'
        '..\..\Samples\Pres2020.ged'
        
          'https://raw.githubusercontent.com/arbre-app/public-gedcoms/refs/' +
          'heads/master/files/royal92.ged'
        
          'https://raw.githubusercontent.com/arbre-app/public-gedcoms/refs/' +
          'heads/master/files/pres2020.ged')
    end
    object BLoad: TButton
      Left = 609
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Load'
      Enabled = False
      TabOrder = 1
      OnClick = BLoadClick
    end
  end
  object Chart1: TChart
    Left = 185
    Top = 41
    Width = 503
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
    View3D = False
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 160
    ExplicitTop = 112
    ExplicitWidth = 400
    ExplicitHeight = 250
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 185
    Height = 400
    Align = alLeft
    TabOrder = 2
    ExplicitLeft = 264
    ExplicitTop = 224
    ExplicitHeight = 41
    object MemoInfo: TMemo
      Left = 1
      Top = 214
      Width = 183
      Height = 185
      Align = alBottom
      TabOrder = 0
      ExplicitLeft = -4
      ExplicitTop = 220
    end
    object LBCharts: TListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 213
      Align = alClient
      Enabled = False
      ItemHeight = 15
      Items.Strings = (
        'By Gender'
        'Dead or Alive'
        'Number of Children')
      TabOrder = 1
      OnClick = LBChartsClick
      ExplicitLeft = -4
      ExplicitTop = 6
    end
  end
  object LBIndividuals: TListBox
    Left = 688
    Top = 41
    Width = 254
    Height = 400
    Align = alRight
    ItemHeight = 15
    TabOrder = 3
  end
end
