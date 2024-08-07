unit Tool_MarksTipCancelHint;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools;

type
  TMarksCancelHintForm = class(TBaseForm)
    Series1: THorizBarSeries;
    ChartTool1: TMarksTipTool;
    Label1: TLabel;
    procedure ChartTool1CancelHint(Sender: TObject);
    procedure ChartTool1GetText(Sender: TMarksTipTool; var Text: String);
  private
    { Private declarations }
    HintText : string;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TMarksCancelHintForm.ChartTool1CancelHint(Sender: TObject);
begin
  Label1.Caption:='Cancelled Hint : ' + HintText;
end;

procedure TMarksCancelHintForm.ChartTool1GetText(Sender: TMarksTipTool;
  var Text: String);
begin
  HintText:=Text;
end;

initialization
  RegisterClass(TMarksCancelHintForm);
end.
