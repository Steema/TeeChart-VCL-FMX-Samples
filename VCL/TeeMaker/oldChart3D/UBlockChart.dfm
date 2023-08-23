object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Presentation TeeChart BlockCharts'
  ClientHeight = 552
  ClientWidth = 914
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TeeCommander1: TTeeCommander
    Left = 0
    Top = 0
    Width = 914
    Height = 33
    Panel = Maker1
    Align = alTop
    ParentShowHint = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 33
    Width = 722
    Height = 519
    Align = alClient
    TabOrder = 1
    object Maker1: TMaker
      Left = 1
      Top = 1
      Width = 720
      Height = 517
      Gradient.EndColor = 13556735
      Gradient.MidColor = 14739177
      Gradient.StartColor = 16774122
      Gradient.Visible = True
      OnAfterDraw = Maker1AfterDraw
      View3DOptions.Orthogonal = False
      Align = alClient
      TabOrder = 0
      Options.Floor.Format.Border.Color = 5460819
      Options.Floor.Format.Border.Visible = True
      Options.Floor.Format.Texture.PictureLink = '$(TEEMaker)\Basic\parket.bmp'
      Options.Floor.Rotation.Y = 90.000000000000000000
      Options.Floor.Size.X = 1300.000000000000000000
      Options.Floor.Size.Y = 1300.000000000000000000
      Options.Floor.Size.Z = 1300.000000000000000000
      Options.Floor.Tile.X = 10.000000000000000000
      Options.Floor.Tile.Y = 10.000000000000000000
    end
  end
  object Panel2: TPanel
    Left = 722
    Top = 33
    Width = 192
    Height = 519
    Align = alRight
    TabOrder = 2
    object Label1: TLabel
      Left = 77
      Top = 61
      Width = 61
      Height = 13
      Caption = 'Series Types'
    end
    object ComboBox1: TComboBox
      Left = 32
      Top = 80
      Width = 106
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'Bar'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Area'
        'Bar'
        'Donut'
        'Line'
        'Pie')
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 341
      Width = 190
      Height = 177
      Align = alBottom
      Caption = 'Custom content'
      TabOrder = 1
      object BitBtn1: TBitBtn
        Left = 32
        Top = 96
        Width = 106
        Height = 25
        Caption = 'Add Extruded Block'
        TabOrder = 0
        OnClick = BitBtn1Click
      end
      object CBAfterDraw: TCheckBox
        Left = 32
        Top = 40
        Width = 97
        Height = 25
        Caption = 'Mark Second  Area Point zone'
        TabOrder = 1
        WordWrap = True
        OnClick = CBAfterDrawClick
      end
    end
    object Button1: TButton
      Left = 32
      Top = 120
      Width = 105
      Height = 25
      Caption = 'Refill Series'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 21
      Top = 168
      Width = 156
      Height = 137
      BevelInner = bvNone
      BevelOuter = bvSpace
      Ctl3D = False
      Lines.Strings = (
        'TeeChart is used here in block '
        'format, being added internally '
        'to a TChart TBlockCanvas as '
        'individual, complete elements '
        '(Chart base, Axes, Series '
        'elements). The approach is '
        'likely to be used as a '
        'rendering technique for '
        'Canvases other than OpenGL '
        'in the future.')
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 3
    end
  end
end
