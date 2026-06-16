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
  object Splitter1: TSplitter
    Left = 613
    Top = 41
    Height = 400
    Align = alRight
    ExplicitLeft = 480
    ExplicitTop = 192
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 942
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitTop = -5
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 53
      Height = 15
      Caption = '&GEDCOM:'
      FocusControl = CBFile
    end
    object Label2: TLabel
      Left = 700
      Top = 13
      Width = 29
      Height = 15
      Caption = 'Filter:'
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
    object EFilter: TEdit
      Left = 744
      Top = 10
      Width = 121
      Height = 23
      TabOrder = 2
      OnChange = EFilterChange
    end
  end
  object Chart1: TChart
    Left = 185
    Top = 41
    Width = 428
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
    View3D = False
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 190
    ExplicitTop = 39
    ExplicitWidth = 232
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
    object MemoInfo: TMemo
      Left = 1
      Top = 214
      Width = 183
      Height = 185
      Align = alBottom
      TabOrder = 0
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
    end
  end
  object Panel3: TPanel
    Left = 616
    Top = 41
    Width = 326
    Height = 400
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object LBSurname: TListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 214
      Align = alLeft
      ItemHeight = 15
      TabOrder = 0
      OnClick = LBSurnameClick
      ExplicitLeft = -6
      ExplicitTop = -1
    end
    object LBIndividuals: TListBox
      Left = 121
      Top = 0
      Width = 205
      Height = 214
      Align = alClient
      ItemHeight = 15
      PopupMenu = PopupMenu1
      TabOrder = 1
      OnClick = LBIndividualsClick
      ExplicitLeft = 127
      ExplicitTop = 6
    end
    object MemoPerson: TMemo
      Left = 0
      Top = 214
      Width = 326
      Height = 186
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
      ExplicitLeft = 6
      ExplicitWidth = 525
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 464
    Top = 224
    object Sort1: TMenuItem
      Caption = '&Sort'
      OnClick = Sort1Click
    end
  end
end
