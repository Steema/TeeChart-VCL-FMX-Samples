unit SeriesType_Slope;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
  Dialogs,
  Base, TeeProcs, TeEngine, Chart, Series, TeeSlopeSeries;

type
  TSlopeSeriesForm = class(TBaseForm)
    Button1: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Series1 : TSlopeSeries;

    procedure Series1ClickPointer(Sender: TCustomSeries; ValueIndex, X,
      Y: Integer);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSlopeSeriesForm.FormCreate(Sender: TObject);
begin
  inherited;

  Series1:=TSlopeSeries.Create(Self);

  Chart1.AddSeries(Series1);

  Chart1.Color:=clWhite;

  Chart1.Legend.TextStyle:=ltsPlain;
  
  Chart1.Axes.Right.Grid.Hide;

  Chart1.Title.Caption:='Slope Chart';
  Chart1.Foot.Caption:='Made with TeeChart';

  RandSeed:=123456;

  Series1.Add([25,100,60],'A');
  Series1.Add([50,10,90],'B');
  Series1.Add([15,80,70],'C');
  Series1.Add([70,30,110],'D');
  Series1.Add([40,10,20],'E');

  Series1.AddTitles(['X','Y']);

  Series1.OnClickPointer:=Series1ClickPointer;
end;

procedure TSlopeSeriesForm.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues;
end;

procedure TSlopeSeriesForm.Series1ClickPointer(Sender: TCustomSeries;
                       ValueIndex, X, Y: Integer);
begin
  Label1.Caption:='Clicked: '+IntToStr(ValueIndex);
end;

initialization
  RegisterClass(TSlopeSeriesForm);
end.
