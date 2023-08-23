unit TeeBlockReplacer;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  TeeMakerControl;

type
  TBlockReplacer = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BOk: TButton;
    Button2: TButton;
    Panel4: TPanel;
    PanelOld: TPanel;
    PanelNew: TPanel;
    Splitter1: TSplitter;
    Button3: TButton;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure ShowGallery(AMaker:TMaker);
  public
    { Public declarations }
    MakerOld,
    MakerNew : TMaker;
  end;

implementation

{$R *.dfm}

uses
  TeeBlocks, TeeBlockGallery;

procedure TBlockReplacer.FormCreate(Sender: TObject);
begin
  MakerOld:=TMaker.Create(Self);
  MakerOld.Align:=alClient;
  MakerOld.Parent:=PanelOld;
  Button3.Parent:=MakerOld;

  MakerNew:=TMaker.Create(Self);
  MakerNew.Align:=alClient;
  MakerNew.Parent:=PanelNew;
  Button4.Parent:=MakerNew;
end;

procedure TBlockReplacer.ShowGallery(AMaker:TMaker);
var tmp : TCustomBlock;
begin
  if AMaker.Blocks.Count=0 then
     tmp:=TBlockGallery.ModalShow(Self,Self,nil,True)
  else
     tmp:=TBlockGallery.ModalShow(Self,Self,TBlockClass(AMaker.Blocks[0].ClassType),True);

  if Assigned(tmp) then
  begin
    if AMaker.Blocks.Count>0 then
       AMaker.Blocks[0].Free;

    tmp.Parent:=AMaker.Blocks;
  end;
end;

procedure TBlockReplacer.Button3Click(Sender: TObject);
begin
  ShowGallery(MakerOld);
  BOk.Enabled:=(MakerOld.Blocks.Count>0) and (MakerNew.Blocks.Count>0);
end;

procedure TBlockReplacer.Button4Click(Sender: TObject);
begin
  ShowGallery(MakerNew);
  BOk.Enabled:=(MakerOld.Blocks.Count>0) and (MakerNew.Blocks.Count>0);
end;

end.
