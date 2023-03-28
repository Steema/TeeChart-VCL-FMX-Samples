unit Print_Preview;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, TeePreviewPanel;

type
  TPrintPreviewForm = class(TBaseForm)
    TeePreviewPanel1: TTeePreviewPanel;
    Splitter1: TSplitter;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

Uses TeePreviewPanelEditor;

procedure TPrintPreviewForm.Button1Click(Sender: TObject);
begin
  With TFormPreviewPanelEditor.CreatePanel(Self,TeePreviewPanel1) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

initialization
  RegisterClass(TPrintPreviewForm);
end.
