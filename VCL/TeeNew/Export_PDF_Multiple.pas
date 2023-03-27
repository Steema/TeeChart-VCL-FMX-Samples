unit Export_PDF_Multiple;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Base, TeeProcs, TeEngine, Chart, ExtCtrls, StdCtrls,
  Series;

type
  TPDFMultipleForm = class(TBaseForm)
    Button1: TButton;
    SavePDFDialog: TSaveDialog;
    Chart2: TChart;
    Series1: TPieSeries;
    Series2: TAreaSeries;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeePDFCanvas;

procedure TPDFMultipleForm.Button1Click(Sender: TObject);
var PDF : TPDFExportFormat;
    tmp : TPDFCanvas;
begin
  if SavePDFDialog.Execute then
  begin
    PDF:=TPDFExportFormat.Create;
    try
      PDF.Orientation:=poPortrait;
      PDF.Height:=1024;
      PDF.Width:=800;

      // Page 1
      PDF.Document.NewPage;
      tmp:=TPDFCanvas.CreatePage(PDF.Document);
      PDF.Draw(tmp,Chart1,Rect(50,250,600,600));
      tmp.TextOut(50,100,'Page 1');
      tmp.Free;

      // Page 2
      PDF.Document.NewPage;
      tmp:=TPDFCanvas.CreatePage(PDF.Document);
      PDF.Draw(tmp,Chart2,Rect(50,250,600,600));
      tmp.TextOut(50,100,'Page 2');
      tmp.Free;

      // Page 3
      // ...

      // Skip checking PDF.Panel property
      PDF.SaveToFile(SavePDFDialog.FileName,nil,False);
    finally
      PDF.Free;
    end;

    // Try to launch Adobe Reader or the default PDF preview
    TeeGotoURL(Handle, 'file:///'+SavePDFDialog.FileName);
  end;
end;

initialization
  RegisterClass(TPDFMultipleForm);
end.
