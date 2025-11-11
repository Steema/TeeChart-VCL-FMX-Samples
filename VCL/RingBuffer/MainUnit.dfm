object FormRingBuffer: TFormRingBuffer
  Left = 0
  Top = 0
  Caption = 'Tee Ring Buffer XY Series'
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
  TextHeight = 15
  object Chart1: TChart
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    View3D = False
    Align = alClient
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 33
      Height = 15
      Caption = 'Series:'
      OnClick = Label1Click
    end
    object Label2: TLabel
      Left = 152
      Top = 13
      Width = 36
      Height = 15
      Caption = 'Points:'
      OnMouseDown = Label2MouseDown
    end
    object CBSeries: TComboBox
      Left = 58
      Top = 10
      Width = 71
      Height = 23
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = '10'
      OnCloseUp = CBSeriesChange
      Items.Strings = (
        '1'
        '5'
        '10'
        '20'
        '50'
        '100'
        '200')
    end
    object CBPoints: TComboBox
      Left = 194
      Top = 10
      Width = 71
      Height = 23
      Style = csDropDownList
      DropDownCount = 16
      ItemIndex = 1
      TabOrder = 1
      Text = '100'
      OnCloseUp = CBPointsChange
      Items.Strings = (
        '10'
        '100'
        '500'
        '1000'
        '2000'
        '5000'
        '10000'
        '20000'
        '50000')
    end
    object CBRun: TCheckBox
      Left = 296
      Top = 13
      Width = 65
      Height = 17
      Caption = 'Run'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBRunClick
    end
    object CBAntialias: TCheckBox
      Left = 352
      Top = 13
      Width = 73
      Height = 17
      Caption = 'Antialias'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBAntialiasClick
    end
    object CBStyle: TComboBox
      Left = 440
      Top = 10
      Width = 65
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Lines'
      OnCloseUp = CBStyleCloseUp
      Items.Strings = (
        'Lines'
        'Dots')
    end
    object CBGrids: TCheckBox
      Left = 527
      Top = 13
      Width = 60
      Height = 18
      Caption = 'Grids'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CBGridsClick
    end
  end
end
