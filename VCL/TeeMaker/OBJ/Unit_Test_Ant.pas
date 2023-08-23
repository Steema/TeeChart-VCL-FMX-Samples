unit Unit_Test_Ant;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeeMakerControl, TeeObjFormat, TeePenDlg, ExtCtrls, TeeJPEG;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    M : TMaker;
    Ant : TObjBlock;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var Ed : TObjBlockEditor;
begin
  M:=TMaker.Create(Self);
  M.Align:=alClient;
  M.Parent:=Self;

  Ant:=TObjBlock.Create(Self);
  Ant.LinkFile:='Samples\formica rufa.obj';
  Ant.Format.Texture.PictureLink:='Samples\texture.jpg';

  M.Blocks.Add(Ant);

  Ed:=TObjBlockEditor.Create(Self);
  Ed.Align:=alClient;

  TTeeVCL.AddFormTo(Ed,Panel1,Ant);
end;

end.
