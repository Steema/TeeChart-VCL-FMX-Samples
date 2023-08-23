unit TeeLoadError;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, 
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TAskLoadError = class(TForm)
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    class function AskContinueLoad(AOwner:TComponent; const Error:String):Boolean;
  end;

var
  GlobalContinueLoad : Boolean=False;

implementation

{$R *.dfm}

class function TAskLoadError.AskContinueLoad(AOwner:TComponent; const Error:String):Boolean;
begin
  with TAskLoadError.Create(AOwner) do
  try
    CheckBox1.Checked:=GlobalContinueLoad;
    Memo1.Lines.Text:=Error;

    if ShowModal=mrOk then
    begin
      GlobalContinueLoad:=CheckBox1.Checked;
      result:=True;
    end
    else
      result:=False;
  finally
    Free;
  end;
end;

end.
