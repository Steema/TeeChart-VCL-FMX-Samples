unit ImageFiltered;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Base, TeeProcs, TeEngine, Chart, Series, TeCanvas, GanttCh,

  jpeg,

  TeeFilters, TeeFiltersEditor;

type
  TImageFilteredDemo = class(TBaseForm)
    ImageFiltered1: TImageFiltered;
    Button1: TButton;
    Series1: TGanttSeries;
    bApply: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TImageFilteredDemo.Button1Click(Sender: TObject);
begin
  // Show the filters editor dialog, and if user click "OK"
  // invalidate the Image (force repaint)

  if TFiltersEditor.ShowEditor(Self, ImageFiltered1.Picture.Graphic,
                       ImageFiltered1.Filters) then
  begin
    ImageFiltered1.Invalidate;
  end;
end;

procedure TImageFilteredDemo.bApplyClick(Sender: TObject);
var tmp : TBitmap;
begin
  // Obtain "filtered" image  (apply filters to picture)

  Chart1.Walls.Visible := True;
  Chart1.Walls.Back.Visible := True;

  bApply.Enabled := False;

  tmp:=ImageFiltered1.Filtered;
  try
    // Assign filtered image to Back Wall of Chart1

    Chart1.Walls.Back.Picture.Graphic:=tmp;
  finally
    //tmp.Free;
  end;
end;

initialization
  RegisterClass(TImageFilteredDemo);
end.
