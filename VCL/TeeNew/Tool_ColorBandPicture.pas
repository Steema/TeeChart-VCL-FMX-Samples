unit Tool_ColorBandPicture;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  jpeg,
  Base, TeEngine, Series, TeeProcs, Chart, TeeTools, Buttons, CandleCh,
  TeeFiltersEditor, TeeFilters;


type
  TColorBandPictureForm = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    bLoad: TButton;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Series1: TVolumeSeries;
    ChartTool1: TColorBandTool;
    bFilters: TButton;
    bEdit: TButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure bFiltersClick(Sender: TObject);
    procedure bEditClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses EditChar;

{$R *.dfm}

procedure TColorBandPictureForm.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Edit1.Text:=OpenDialog1.FileName;
    bLoad.Enabled:=true;
  end;
end;

procedure TColorBandPictureForm.bLoadClick(Sender: TObject);
begin
  ChartTool1.Picture.LoadFromFile(edit1.Text);
  bLoad.Enabled:=false;
end;

procedure TColorBandPictureForm.bFiltersClick(Sender: TObject);
begin
  TFiltersEditor.ShowEditor(Self,Charttool1.Picture);
end;

procedure TColorBandPictureForm.bEditClick(Sender: TObject);
begin
  EditChartTool(Self,ChartTool1);
end;

initialization
  RegisterClass(TColorBandPictureForm);
end.
