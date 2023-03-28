unit Editor_FontsTab;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, StatChar;

type
  TEditorFontsTab = class(TBaseForm)
    Button1: TButton;
    Series1: THistogramSeries;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeEdiFont;

procedure TEditorFontsTab.Button1Click(Sender: TObject);
begin
  TTeeFontEditor.Edit(Self,Chart1.Axes.Left.LabelsFont);
end;

initialization
  RegisterClass(TEditorFontsTab);
end.
