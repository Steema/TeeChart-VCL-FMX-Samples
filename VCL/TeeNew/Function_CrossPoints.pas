unit Function_CrossPoints;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, StatChar, Series;

type
  TCrossPointsDemo = class(TBaseForm)
    TeeFunction1: TCrossPointsFunction;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TCrossPointsDemo.CheckBox1Click(Sender: TObject);
begin
  Series3.Visible:=CheckBox1.Checked;
end;

procedure TCrossPointsDemo.FormCreate(Sender: TObject);
begin
  inherited;
  Chart1.SeriesList.FillSampleValues(25);
  Series3.CheckDataSource;
end;

initialization
  RegisterClass(TCrossPointsDemo);
end.
