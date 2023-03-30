unit SeriesType_Slope;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
  Dialogs,
  Base, TeeProcs, TeEngine, Chart, TeeSlopeSeries;

type
  TSlopeSeriesForm = class(TBaseForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Series1 : TSlopeSeries;
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

  Series1.Add(25,100,'A');  // or array: Series1.Add( [25,100] , 'A')
  Series1.Add(50,10,'B');
  Series1.Add(15,80,'C');
  Series1.Add(70,30,'D');
  Series1.Add(40,10,'E');

  Series1.AddTitles(['X','Y']);
end;

procedure TSlopeSeriesForm.Button1Click(Sender: TObject);
begin
  Series1.FillSampleValues;
end;

initialization
  RegisterClass(TSlopeSeriesForm);
end.
