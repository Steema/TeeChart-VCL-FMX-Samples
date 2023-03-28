unit Tool_MarksTipHide;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages, 
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeCanvas, TeeProcs, TeEngine, Chart, TeeTools, Series;

type
  TMarksTipHideForm = class(TBaseForm)
    Series1: TBarSeries;
    Series2: TBarSeries;
    ChartTool1: TMarksTipTool;
    Label1: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksTipHideForm.Edit1Change(Sender: TObject);
begin
  ChartTool1.HideDelay:=UpDown1.Position;
end;

initialization
  RegisterClass(TMarksTipHideForm);
end.
