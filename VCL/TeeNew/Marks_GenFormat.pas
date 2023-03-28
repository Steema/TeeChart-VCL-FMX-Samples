unit Marks_GenFormat;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  Base, TeeProcs, TeEngine, Chart, Series;

type
  TMarksGenFormat = class(TBaseForm)
    PageControl1: TPageControl;
    Series1: TPointSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

Uses TeeCustomShapeEditor;

procedure TMarksGenFormat.FormCreate(Sender: TObject);
begin
  inherited;
  Series1.FillSampleValues(4);
end;

procedure TMarksGenFormat.FormShow(Sender: TObject);
begin
  inherited;
  TFormTeeShape.InsertObjectForm(PageControl1,Series1.Marks).RefreshControls(Series1.Marks);
end;

initialization
  RegisterClass(TMarksGenFormat);
end.
