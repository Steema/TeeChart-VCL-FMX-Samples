unit Pages_NextPreviousMethods;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas;

type
  TPagesNextPreviousMethods = class(TBaseForm)
    Series1: TAreaSeries;
    bPrevious: TButton;
    bNext: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bPreviousClick(Sender: TObject);
    procedure bNextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TPagesNextPreviousMethods.FormCreate(Sender: TObject);
begin
  inherited;

  Chart1.MaxPointsPerPage:=15;
end;

procedure TPagesNextPreviousMethods.bPreviousClick(Sender: TObject);
begin
  Chart1.PreviousPage;
end;

procedure TPagesNextPreviousMethods.bNextClick(Sender: TObject);
begin
  Chart1.NextPage;
end;

initialization
  RegisterClass(TPagesNextPreviousMethods);
end.

