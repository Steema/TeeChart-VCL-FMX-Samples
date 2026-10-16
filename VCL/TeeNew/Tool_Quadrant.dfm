inherited QuadrantToolForm: TQuadrantToolForm
  Caption = 'Quadrant Tool'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    object CBActive: TCheckBox
      Left = 16
      Top = 12
      Width = 81
      Height = 17
      Caption = '&Active'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBActiveClick
    end
    object CBCrossHair: TCheckBox
      Left = 112
      Top = 12
      Width = 113
      Height = 17
      Caption = '&Crosshair'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CBCrossHairClick
    end
  end
end
