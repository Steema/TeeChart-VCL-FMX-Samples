unit Tools_Gallery;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, TeeTools,
  TeeBrushDlg, TeeAbout, TeeEditPro, TeeEditTools, TeeGanttTool, TeeAnnToolEdit,
  TeeToolsGallery;

type
  TToolsGallery = class(TBaseForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TToolsGallery.Button1Click(Sender: TObject);
begin
  with TTeeToolsGallery.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

initialization
  RegisterClass(TToolsGallery);
end.
