inherited AxesIsometric: TAxesIsometric
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited Memo1: TMemo
    Lines.Strings = (
      'Axes.Isometric keeps one X unit and one Y unit the same size in 2D.'
      'None: normal axes. Rescale: expand ranges. Fit: fit the plot rectangle.'
      'Resize the window or zoom: the circle and square keep their shape.'
      ''
      'Chart1.Axes.Isometric.Mode := iaRescale; // or iaNone, iaFit')
  end
  inherited Chart1: TChart
    Legend.Visible = False
    Title.Text.Strings = (
      'Isometric axes: circle and square')
    BottomAxis.Grid.Visible = True
    BottomAxis.Increment = 20.000000000000000000
    BottomAxis.Title.Caption = 'X'
    LeftAxis.Axis.Visible = True
    LeftAxis.Increment = 20.000000000000000000
    LeftAxis.Title.Caption = 'Y'
    View3D = False
    object Series1: TLineSeries
      SeriesColor = clRed
      Title = 'Circle'
      LinePen.Width = 2
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TLineSeries
      SeriesColor = clBlue
      Title = 'Square'
      LinePen.Width = 2
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loNone
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  inherited Panel1: TPanel
    object Label1: TLabel
      Left = 8
      Top = 13
      Width = 30
      Height = 13
      Caption = '&Mode:'
      FocusControl = CBMode
    end
    object CBMode: TComboBox
      Left = 48
      Top = 10
      Width = 225
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CBModeChange
      Items.Strings = (
        'None (iaNone)'
        'Rescale ranges (iaRescale)'
        'Fit plot rectangle (iaFit)')
    end
    object BReset: TButton
      Left = 288
      Top = 8
      Width = 97
      Height = 25
      Caption = '&Reset axes'
      TabOrder = 1
      OnClick = BResetClick
    end
  end
end
