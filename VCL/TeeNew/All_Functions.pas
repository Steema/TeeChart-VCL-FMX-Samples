unit All_Functions;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TAllFunctionsForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeePenDlg, Chart, TeeGally;

procedure TAllFunctionsForm.FormCreate(Sender: TObject);
var Dummy : TChart;
    tmp : TTeeGallery;
begin
  Dummy:=TChart.Create(Self);

  tmp:=TTeeGallery.AddFunctionGallery(Self,Dummy,nil);

  tmp.P1.Visible:=False;
  
  tmp.Align:=alClient;
  
  TTeeVCL.AddFormTo(tmp,Self);
end;

initialization
  RegisterClass(TAllFunctionsForm);
end.
