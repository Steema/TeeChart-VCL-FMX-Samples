unit Series_XMLSourceWeb;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  TeeXML, Base, TeeProcs, TeEngine, Chart;

type
  TXMLSourceWeb = class(TBaseForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure LoadXML;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TXMLSourceWeb.LoadXML;
begin
  {$IFNDEF LINUX}
  // Force Windows to use "," (comma) as decimal separator
  SetLocaleInfo(SysLocale.DefaultLCID,LOCALE_SDECIMAL,',');
  {$ENDIF}

  with TTeeXMLSource.Create(Self) do
  try
    FileName:=Edit1.Text; // <-- set Web URL address
    Chart:=Chart1;        // <-- set Chart
    Load;                 // <-- retrieve data and create series
  finally
    Free;
  end;
end;

procedure TXMLSourceWeb.Button1Click(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    LoadXML;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

initialization
  RegisterClass(TXMLSourceWeb);
end.
