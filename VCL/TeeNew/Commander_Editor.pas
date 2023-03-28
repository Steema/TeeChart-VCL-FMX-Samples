unit Commander_Editor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeComma, TeeProcs, TeEngine, Chart;

type
  TCommanderEditorExample = class(TBaseForm)
    TeeCommander1: TTeeCommander;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeeCommanderEditor;

procedure TCommanderEditorExample.Button1Click(Sender: TObject);
begin
  TeeCommanderEdit(Self,TeeCommander1);
end;

initialization
  RegisterClass(TCommanderEditorExample);
end.
