unit UMultiPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, VCLTee.TeEngine,
  VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeComma, Math, VCLTee.Series,
  VCLTee.Bar3D, VCLTee.TeeSurfa, VCLTee.TeeNumericGauge, VCLTee.TeeLinearGauge,
  VCLTee.TeeCircularGauge, VCLTee.TeeKnobGauge, VCLTee.TeeDonut, VCLTee.ErrorBar,
  Vcl.StdCtrls, VCLTee.StatChar, VCLTee.TeeTools;

type
    XArray = Array of Double;

type
  TForm7 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    TeeCommander1: TTeeCommander;
    Chart1: TChart;
    Chart2: TChart;
    Chart3: TChart;
    Chart4: TChart;
    Chart5: TChart;
    Chart6: TChart;
    Chart7: TChart;
    Chart8: TChart;
    Chart9: TChart;
    line1: TLineSeries;
    line2: TLineSeries;
    area1: TAreaSeries;
    bar3D1: TBar3DSeries;
    surface: TSurfaceSeries;
    harmonic: TFastLineSeries;
    donut1: TDonutSeries;
    donut2: TDonutSeries;
    donut3: TDonutSeries;
    donut4: TDonutSeries;
    circularGauge1: TKnobGauge;
    highlow1: THighLowSeries;
    line3: TLineSeries;
    line4: TLineSeries;
    Timer1: TTimer;
    Button1: TButton;
    TeeFunction1: TStdDeviationFunction;
    TeeFunction2: TRMSFunction;
    Annotation1: TAnnotationTool;
    Annotation2: TAnnotationTool;
    ColorBand1: TColorBandTool;
    Colorband2: TColorBandTool;
    CursorTool1: TCursorTool;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Chart2AfterDraw(Sender: TObject);
    procedure Chart3AfterDraw(Sender: TObject);
    procedure Chart4AfterDraw(Sender: TObject);
    procedure Chart9AfterDraw(Sender: TObject);
    procedure Chart6BeforeDrawSeries(Sender: TObject);
    procedure CursorTool1Change(Sender: TCursorTool; x, y: Integer;
      const XValue, YValue: Double; Series: TChartSeries; ValueIndex: Integer);
    procedure Chart7AfterDraw(Sender: TObject);
  private
    { Private declarations }
    procedure ConfigGauge1;
    procedure ConfigGauge2;
    procedure ConfigGauge3;
    procedure ConfigGauge4;
    procedure ConfigGauge5;
    procedure FillHarmonic;
    procedure ConfigHarmonics;
    procedure ConfigMultiSeries;
    procedure ConfigHighLow;
    procedure AddHighLow;
    procedure RotateSurface;
    procedure AddToMultiSeries;
    procedure DoTimerTick;
    procedure ModGauges;
    procedure ConfigSurface;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;
  p2, freq, aFactor: double;
  N1, m, tickCount, Delta, rotateBrake, OneYesOneNo: Integer;
  cursorX,cursorY : Integer;
  x : XArray;
  deltaM : Boolean;
  fms : TFormatSettings;
  cursorText : String;


implementation

{$R *.dfm}

procedure TForm7.AddHighLow;
var
  NewHigh, NewLow: Double;
begin
  NewHigh := HighLow1.HighValues.Last + (100.0 - (Random * 50));
  NewLow  := HighLow1.LowValues.Last  + (100.0 - (Random * 50));

  if NewHigh > 999 then
    NewHigh := 1000 - (Random * 200);

  if NewLow > 999 then
    NewLow := 1000 - (Random * 200);

  HighLow1.AddHighLow(
    HighLow1.XValues.Last + (1 / 86400.0 / 2.0),
    NewHigh,
    NewLow
  );

  if HighLow1.Count > 100 then
  begin
    HighLow1.Delete(0);
    Chart6.Axes.Bottom.SetMinMax(
      HighLow1.XValues.Value[0],
      HighLow1.XValues.Last
    );
  end;

  Line3.CheckDataSource;
  Line4.CheckDataSource;
end;

procedure TForm7.AddToMultiSeries;
var
  Delta1, Delta2, Delta3: Double;
  NewAreaVal, NewLine1Val, NewLine2Val: Double;
  TimeStamp: Double;
  LastAreaX: Double;
begin
  Delta1 := Random * 350;
  Delta2 := Random * 350; // (kept although unused, like original)
  Delta3 := (Random * 350) + 10;

  NewAreaVal  := Area1.YValues.Last + IfThen(Area1.YValues.Last > 449, -Delta1, Delta1);
  NewLine1Val := Line1.YValues.Last + IfThen(Line1.YValues.Last > 449, -Delta1, Delta1);
  NewLine2Val := Line2.YValues.Last + IfThen(Line2.YValues.Last > 449, -Delta3, Delta3);

  if NewAreaVal < 100 then NewAreaVal := 100;
  if NewLine1Val < 100 then NewLine1Val := 100;
  if NewLine2Val < 100 then NewLine2Val := 100;

  LastAreaX := Area1.XValues.Last;

  TimeStamp := LastAreaX + (1 / 86400.0 / 2.0);

  Area1.AddXY(TimeStamp, NewAreaVal);
  Area1.XValues.Modified := True;

  Line1.AddXY(TimeStamp, NewLine1Val);
  Line2.AddXY(TimeStamp, NewLine2Val);

  if Area1.Count = 102 then
    OneYesOneNo := 0;

  Inc(OneYesOneNo);

  if OneYesOneNo = 1 then
  begin
    Bar3D1.AddBar(TimeStamp, 400 - Random(400), 400 + Random(400));
    OneYesOneNo := -1;
  end;

  if Area1.Count > 110 then
  begin
    Area1.Delete(0);
    Area1.XValues.Modified := True;

    Line1.Delete(0);
    Line1.XValues.Modified := True;

    Line2.Delete(0);
    Line2.XValues.Modified := True;

    if Bar3D1.Count > 70 then
    begin
      Bar3D1.Delete(0);
      Bar3D1.XValues.Modified := True;
    end;

    Chart7.Axes.Bottom.SetMinMax(
      Area1.XValues.Value[0],
      Area1.XValues.Last
    );
  end;
end;

procedure TForm7.Button1Click(Sender: TObject);
Var lRange, bRange : Double;
begin
  timer1.Enabled := not timer1.Enabled;

  cursorTool1.Active := not timer1.Enabled;
  if (cursorTool1.Active) then
  Begin
    lRange := Chart7.Axes.Left.Maximum - Chart7.Axes.Left.Minimum;
    bRange := Chart7.Axes.Bottom.Maximum - Chart7.Axes.Bottom.Minimum;
    cursorTool1.XValue := Chart7.Axes.Bottom.Minimum + (bRange / 2);
    cursorTool1.YValue := Chart7.Axes.Left.Minimum + (lRange / 2);
  end;
end;

procedure TForm7.Chart1AfterDraw(Sender: TObject);
Var x,y : Integer;
begin
    x := Chart1.Canvas.XCenter;
    y := Chart1.Canvas.YCenter + 35;

    if (x > -1) then
    With Chart1.Canvas do
    begin
      Font.Color := clWhite;
      Font.Style := [fsBold];
      TextOut((x - Round((TextWidth(FormatFloat('#.00', circularGauge1.Value)) / 2))), y, FormatFloat('#.00', circularGauge1.Value));
      TextOut((x - Round((TextWidth('ºC') / 2))), y + 19, 'ºC');
    end;
end;

procedure TForm7.Chart2AfterDraw(Sender: TObject);
Var x,y : Integer;
begin
    x := Chart2.Canvas.XCenter;
    y := Chart2.Canvas.YCenter - (Chart2.Canvas.TextHeight('H') div 2);

    if (x > -1) then
    With Chart2.Canvas do
    begin
      Font.Color := clWhite;
      Font.Size := 16;
      Font.Style := [fsBold];
      TextOut((x - Round((TextWidth(FormatFloat('#.00', donut1.YValues[1])) / 2))), y, FormatFloat('#.00', donut1.YValues[1]));
      TextOut((x - Round((TextWidth('ºC') / 2))), y + 28, 'ºC');
    end;
end;

procedure TForm7.Chart3AfterDraw(Sender: TObject);
Var x,y : Integer;
begin
    x := Chart3.Canvas.XCenter;
    y := Chart3.Canvas.YCenter - (Chart3.Canvas.TextHeight('H') div 2);

    if (x > -1) then
      With Chart3.Canvas do
      begin
        Font.Color := clWhite;
        Font.Size := 16;
        Font.Style := [fsBold];
        TextOut((x - Round((TextWidth(FormatFloat('#.00', donut2.YValues[1])) / 2))), y, FormatFloat('#.00', donut2.YValues[1]));
        TextOut((x - Round((TextWidth('ºC') / 2))), y + 28, 'ºC');
      end;

end;

procedure TForm7.Chart4AfterDraw(Sender: TObject);
Var x,y : Integer;
begin
    x := Chart4.Canvas.XCenter;
    y := Chart4.Canvas.YCenter - (Chart4.Canvas.TextHeight('H') div 2);

    if (x > -1) then
      With Chart4.Canvas do
      begin
        Font.Color := clWhite;
        Font.Size := 16;
        Font.Style := [fsBold];
        TextOut((x - Round((TextWidth(FormatFloat('#.00', donut3.YValues[1])) / 2))), y, FormatFloat('#.00', donut3.YValues[1]));
        TextOut((x - Round((TextWidth('ºC') / 2))), y + 28, 'ºC');
      end;

end;

procedure TForm7.Chart6BeforeDrawSeries(Sender: TObject);
Var pRight : Integer;
begin
      pRight := Chart6.Axes.Bottom.CalcPosValue(Chart6.Axes.Bottom.Maximum);

      With Chart6.Canvas do
      Begin
        annotation1.Left := (pRight - TextWidth(annotation1.Text));
        annotation2.Left := (pRight - TextWidth(annotation2.Text));

        annotation1.Top := (Chart6.CustomAxes[0].CalcPosValue(colorBand1.EndValue) - TextHeight('H'));
        annotation2.Top := Chart6.CustomAxes[0].CalcPosValue(colorBand1.StartValue);
      End;
end;

procedure TForm7.Chart7AfterDraw(Sender: TObject);
begin
      if (not timer1.Enabled) then
      Begin
        With Chart7.Canvas do
        Begin
          if (cursorX <> -1) then
          Begin
            Font.Color := clWhite;
            Font.Size := 12;
            TextOut(cursorX + 3, (cursorY - TextHeight('H') - 2), 'Freq. Hz.: ' + cursorText);
          end;
        End;
      end;
end;

procedure TForm7.Chart9AfterDraw(Sender: TObject);
Var x,y : Integer;
begin
    x := Chart9.Canvas.XCenter;
    y := Chart9.Canvas.YCenter - (Chart9.Canvas.TextHeight('H') div 2);

    if (x > -1) then
    With Chart9.Canvas do
    begin
      Font.Color := clWhite;
      Font.Size := 16;
      Font.Style := [fsBold];
      TextOut((x - Round((TextWidth(FormatFloat('#.00', donut4.YValues[1])) / 2))), y, FormatFloat('#.00', donut4.YValues[1]));
      TextOut((x - Round((TextWidth('ºC') / 2))), y + 28, 'ºC');
    end;

end;

procedure TForm7.ConfigGauge1;
begin
  CircularGauge1.GreenLine.Pen.Visible := False;
  CircularGauge1.GreenLine.Gradient.Visible := False;
  CircularGauge1.GreenLine.Brush.Color := RGB(255, 207, 104); // Light orange/yellow

  CircularGauge1.RedLine.StartValue := 75;
  CircularGauge1.RedLine.Pen.Visible := False;
  CircularGauge1.RedLine.Gradient.Visible := False;
  CircularGauge1.RedLine.Brush.Color := clRed;

  //CircularGauge1.FaceBrush.Visible := False;

  CircularGauge1.Value := Random * 70;

  //CircularGauge1.Axis.OnGetAxisDrawLabel := Axis_GetAxisDrawLabel;

end;

procedure TForm7.ConfigGauge2;
Var dValue : Double;
begin
  Donut1.Clear;

  dValue := Random * 1000;
  Donut1.Add(1000 - dValue);
  Donut1.Add(dValue);

  Donut1.AngleSize := 237;
  Donut1.DonutPercent := 60;
  Donut1.RotationAngle := 332;
  Donut1.Frame.Circled := True;

  Donut1.Pen.Width := 4;
  Donut1.Pen.Color := TChart(Donut1.ParentChart).Gradient.StartColor;
end;

procedure TForm7.ConfigGauge3;
Var dValue : Double;
begin
  // donut2
  Donut2.AngleSize := 237;
  Donut2.Brush.Color := RGB(255, 243, 144);
  Donut2.Color := RGB(119, 153, 214);
  Donut2.DonutPercent := 60;

  Donut2.Frame.Circled := True;

  Donut2.MultiPie := mpAutomatic;

  Donut2.RotationAngle := 332;
  Donut2.Title := 'donut2';

  Donut2.UniqueCustomRadius := True;
  Donut2.XValues.Order := loAscending;

  Donut2.Clear;
  dValue := Random * 1000;
  Donut2.Add(1000 - dValue);
  Donut2.Add(dValue);

  Donut2.Pen.Width := 4;
  Donut2.Pen.Color := TChart(Donut1.ParentChart).Gradient.StartColor; // clBlack
end;

procedure TForm7.ConfigGauge4;
Var dValue : Double;
begin
  Donut3.Clear;
  dValue := Random * 1000;
  Donut3.Add(1000 - dValue);
  Donut3.Add(dValue);

  Donut3.AngleSize := 237;
  Donut3.DonutPercent := 60;
  Donut3.RotationAngle := 332;
  Donut3.Frame.Circled := True;

  Donut3.Pen.Width := 4;
  Donut3.Pen.Color := TChart(Donut1.ParentChart).Gradient.StartColor;
end;

procedure TForm7.ConfigGauge5;
Var dValue : Double;
begin
  Donut4.Clear;
  dValue := Random * 1000;
  Donut4.Add(1000 - dValue);
  Donut4.Add(dValue);

  Donut4.AngleSize := 237;
  Donut4.DonutPercent := 60;
  Donut4.RotationAngle := 332;
  Donut4.Frame.Circled := True;

  Donut4.Pen.Width := 4;
  Donut4.Pen.Color := TChart(Donut1.ParentChart).Gradient.StartColor;
end;

procedure TForm7.ConfigHarmonics;
begin
  Chart8.Axes.Left.SetMinMax(-1.2, 1.21);

  Chart8.AddSeries(TFastLineSeries.Create(Self));

  Chart8.Series[0].Color := RGB(255, 255, 128);

  TFastLineSeries(Chart8.Series[0]).LinePen.Style := TPenStyle.psSolid; //   .UseStyling := False;

  FillHarmonic;
end;

procedure TForm7.ConfigHighLow;
var
  I: Integer;
begin
//  Annotation1.PositionUnits := puPixels;
//  Annotation2.PositionUnits := puPixels;
//
//  Annotation1.Position := apCustom;
//  Annotation2.Position := apCustom;

  Chart6.Axes.Left.Maximum := 1000;

  //HighLow1.HighBrush.Visible := True;
  HighLow1.HighBrush.Gradient.StartColor := RGB(192, 192, 255);
  HighLow1.HighBrush.Gradient.EndColor := RGB(128, 128, 255);

  HighLow1.LowBrush.Gradient.StartColor := RGB(128, 128, 255);
  HighLow1.LowBrush.Gradient.EndColor := RGB(192, 192, 255);

  HighLow1.HighBrush.Gradient.Visible := True;
  HighLow1.HighBrush.Transparency := 50;

  //HighLow1.LowBrush.Visible := True;
  HighLow1.LowBrush.Gradient.Visible := True;
  HighLow1.LowBrush.Transparency := 50;

  for I := 0 to Line1.Count - 1 do
  begin
    HighLow1.AddHighLow(
      Line1.XValues.Value[I],
      Line1.YValues.Value[I],
      Line2.YValues.Value[I]
    );
  end;

  Line3.CheckDataSource;
  Line4.CheckDataSource;
end;

procedure TForm7.ConfigMultiSeries;
var
  StartDT: Double;
  I: Integer;
begin
  //Chart7.OnAfterDraw := TChart7AfterDraw;

  Area1.FillSampleValues(100);
  Line1.FillSampleValues(100);
  Line2.FillSampleValues(100);

  StartDT := Now; // Delphi TDateTime is equivalent to OLE Automation date

  for I := 0 to Area1.XValues.Count - 1 do
  begin
    Area1.XValues.Value[I] := StartDT + ((1.0 / 86400) * I / 2);

    Line1.XValues.Value[I] := Area1.XValues.Value[I];
    Line2.XValues.Value[I] := Area1.XValues.Value[I];

    if Line1.YValues.Value[I] > 999 then
      Line1.YValues.Value[I] := 1000 - (Random * 200);

    if Line2.YValues.Value[I] > 999 then
      Line2.YValues.Value[I] := 1000 - (Random * 200);

    if (I mod 2) = 0 then
      Bar3D1.AddBar(
        Area1.XValues.Value[I],
        400 - Random(400),
        400 + Random(400)
      );
  end;

  ConfigHighLow;
end;


procedure TForm7.ConfigSurface;
var
  XVal: array[0..9] of Double;
  ZVal: array[0..9] of Double;
  X, Z: Integer;
  Y: Double;
begin
  Chart5.BackColor := RGB(70, 70, 70);

  Surface.Clear;

  // Irregular grid sample values
  XVal[0] := 0.1; XVal[1] := 0.2; XVal[2] := 0.3; XVal[3] := 0.5; XVal[4] := 0.8;
  XVal[5] := 1.1; XVal[6] := 1.5; XVal[7] := 2.0; XVal[8] := 2.2; XVal[9] := 3.0;

  ZVal[0] := 0.5; ZVal[1] := 0.6; ZVal[2] := 0.7; ZVal[3] := 0.75; ZVal[4] := 0.8;
  ZVal[5] := 1.1; ZVal[6] := 1.5; ZVal[7] := 2.0; ZVal[8] := 2.2; ZVal[9] := 5.6;

  Surface.IrregularGrid := True; // VERY IMPORTANT

  // Surface dimensions
  Surface.NumXValues := 10;
  Surface.NumZValues := 10;

  // Fill data (10x10 grid)
  for X := 0 to 9 do
    for Z := 0 to 9 do
    begin
      // Example Y value
      Y := 700 + Sin(Z * Pi / 10.0) * Cos(X * Pi / 5.0);

      Surface.AddXYZ(XVal[X], Y, ZVal[Z]);
    end;
end;

procedure TForm7.CursorTool1Change(Sender: TCursorTool; x, y: Integer;
  const XValue, YValue: Double; Series: TChartSeries; ValueIndex: Integer);
begin
  cursorText := FormatFloat('#.00',line2.YScreenToValue(y));
  cursorX := x;
  cursorY := y;
end;

procedure TForm7.DoTimerTick;
begin
  if deltaM then
  begin
    m := m + 10;
    if m = 200 then
      deltaM := False;
  end
  else
  begin
    m := m - 10;
    if m = 10 then
      deltaM := True;
  end;

  Inc(tickCount);

  FillHarmonic;
  AddHighLow;
  RotateSurface;
  AddToMultiSeries;

  if tickCount = 70 then
  begin
    ModGauges;
    tickCount := 0;
  end;
end;

procedure TForm7.FormResize(Sender: TObject);
begin
  Randomize;

  Panel2.Width := self.ClientWidth div 7 * 3;
  Panel4.Height := self.ClientHeight div 2;
  Panel6.Height := self.ClientHeight div 2;
  Panel2.Left := Panel1.Left+Panel1.Width;
  Panel3.Left := Panel1.Left+Panel1.Width + Panel2.Left+Panel2.Width;

  Panel8.Height := self.ClientHeight div 5;
  Panel9.Height := self.ClientHeight div 5;
  Panel10.Height := self.ClientHeight div 5;
  Panel11.Height := self.ClientHeight div 5;
  Panel12.Height := self.ClientHeight div 5;

  Chart5.Foot.Top := Chart5.BoundsRect.Height - 32;
end;

procedure TForm7.FormShow(Sender: TObject);
begin
  SetLength(x,3000);
  N1 := 3000;
  p2 := System.Pi * 2;
  m := 0;
  freq := 3;
  aFactor := 1.0;

  deltaM := true;
  tickCount := 0;
  Delta := 1;
  rotateBrake := 0;
  OneYesOneNo := 0;

  cursorText := '';
  cursorX := -1;
  cursorY := -1;

  fms := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);

  area1.FillSampleValues(10);
  line1.FillSampleValues(10);
  line2.FillSampleValues(10);
  bar3D1.FillSampleValues(10);

//  donut1.FillSampleValues(2);
//  donut2.FillSampleValues(2);
//  donut3.FillSampleValues(2);
//  donut4.FillSampleValues(2);
//
//  surface.FillSampleValues(10);
//
//  harmonic.FillSampleValues(20);

  ConfigGauge1;
  ConfigGauge2;
  ConfigGauge3;
  ConfigGauge4;
  ConfigGauge5;
  ConfigSurface;
  ConfigMultiSeries;
  ConfigHarmonics;
end;

procedure TForm7.ModGauges;

  procedure ModDonut(ADonut: TDonutSeries);
  var
    Delta1: Double;
  begin
    Delta1 := Random * 5;

    if ADonut.YValues[1] > 910 then
      ADonut.YValues[1] := ADonut.YValues[1] - Delta1
    else if ADonut.YValues[1] < 90 then
      ADonut.YValues[1] := ADonut.YValues[1] + Delta1
    else
      ADonut.YValues[1] := ADonut.YValues[1] + (1 - Delta1);

    ADonut.YValues[0] := 1000 - ADonut.YValues[1];

    ADonut.Repaint;
  end;

begin
  ModDonut(Donut1);
  ModDonut(Donut2);
  ModDonut(Donut3);
  ModDonut(Donut4);

  if CircularGauge1.Value > 65 then
    CircularGauge1.Value := CircularGauge1.Value - (Random * 2)
  else if CircularGauge1.Value < 10 then
    CircularGauge1.Value := CircularGauge1.Value + (Random * 2)
  else
    CircularGauge1.Value := CircularGauge1.Value + (2 - Random * 3);
end;

procedure TForm7.RotateSurface;
begin
  if (rotateBrake mod 40) = 0 then
  begin
    rotateBrake := 0;

    if Chart5.Aspect.Rotation = 360 then
      Delta := -1
    else if Chart5.Aspect.Rotation = 0 then
      Delta := 1;

    if (Chart5.Aspect.Rotation <= 360) and (Delta = 1) then
      Chart5.Aspect.Rotation := Chart5.Aspect.Rotation + 1
    else if (Chart5.Aspect.Rotation >= 0) and (Delta = -1) then
      Chart5.Aspect.Rotation := Chart5.Aspect.Rotation - 1;
  end
  else
    Inc(rotateBrake);
end;

procedure TForm7.Timer1Timer(Sender: TObject);
begin
  DoTimerTick;
end;

procedure TForm7.FillHarmonic;
var
  a: Double;
  n, k: Integer;
begin
  // m := 40; // 0 -> 500

  // Square
  for n := 0 to N1 - 1 do
  begin
    x[n] := 0;

    for k := 1 to m do
    begin
      a := aFactor / k;

      if ((k - 1) mod 2) = 0 then
        x[n] := x[n] + (4.0 / Pi) * a * Sin(p2 * freq * k * n / N1);
    end;
  end;

  {
  // Sawtooth
  for n := 0 to N1 - 1 do
  begin
    x[n] := 0;
    s := -1;

    for k := 1 to m do
    begin
      s := -s;
      a := s / k;
      x[n] := x[n] + (2 / Pi) * a * Sin(p2 * 3 * k * n / N1);
    end;
  end;
  }

  Chart8[0].Clear;

  for n := 0 to N1 - 1 do
    Chart8[0].AddXY(n, x[n]);
end;

end.
