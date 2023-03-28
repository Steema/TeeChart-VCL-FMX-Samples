unit Editor_Strings;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeTools, BubbleCh;

type
  TEditorStrings = class(TBaseForm)
    Series1: TBubbleSeries;
    ChartTool1: TAnnotationTool;
    bEdit: TButton;
    procedure bEditClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeStringsEditor;

procedure TEditorStrings.bEditClick(Sender: TObject);
var tmp : string;
begin
  tmp:=ChartTool1.Text;

  if TStringsEditor.Edit(Self,tmp) then
     ChartTool1.Text:=tmp;
end;

initialization
  RegisterClass(TEditorStrings);
end.
