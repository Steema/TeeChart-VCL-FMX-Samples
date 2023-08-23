unit TeeChartBlock3DEditor;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls,
  {$ENDIF}
  TeeBlockEditor, TeeProcs, TeeGLEditor, TeeEdi3D;

type
  TChartBlock3DEditor = class(TForm)
    PageControl1: TPageControl;
    TabObject: TTabSheet;
    TabOpenGL: TTabSheet;
    Tab3D: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    I3DEdit : TFormTee3D;
    IEdit   : TBlockEditor;
    IGLEdit : TFormTeeGLEditor;
    IPanel  : TCustomTeePanel;
  public
    { Public declarations }
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeMakerControl, TeePenDlg, TeeChartBlock, TeeBlocks;

type
  TCustomBlockChartAccess=class(TCustomBlockChart);

procedure TChartBlock3DEditor.FormShow(Sender: TObject);
var tmpMaker : TMaker;
begin
  IPanel:=TCustomTeePanel(Tag);

  if Assigned(IPanel) and (IPanel is TCustomBlockChart) then
  begin
    {IEdit.TabBlock.TabVisible:=False;
    IEdit.TabFormat.TabVisible:=False;}
    IEdit.PageControl1.ActivePage:=IEdit.TabRotation;

    IEdit.RefreshBlock(TCustomBlockChartAccess(IPanel).IBlock);

    tmpMaker:=TMaker(TCustomBlockChartAccess(IPanel).IBlock.Parent.Parent);

    IGLEdit.RefreshOpenGL(tmpMaker.Render);
    IGLEdit.Panel1.Visible:=False;
    IGLEdit.CBActive.Visible:=False;

    I3DEdit.ThePanel:=tmpMaker;
    I3DEdit.CheckControls;
  end;
end;

procedure TChartBlock3DEditor.FormCreate(Sender: TObject);
begin
  I3DEdit:=TFormTee3D.Create(nil);
  I3DEdit.Align:=alClient;

  TTeeVCL.AddFormTo(I3DEdit,Tab3D);

  IEdit:=TBlockEditor.Create(nil);
  IEdit.Align:=alClient;
  IEdit.PanelButtons.Visible:=False;
  IEdit.TabObject.TabVisible:=False;
  IEdit.TabCustom.TabVisible:=False;

  TTeeVCL.AddFormTo(IEdit,TabObject);

  IGLEdit:=TFormTeeGLEditor.Create(nil);
  IGLEdit.Align:=alClient;

  TTeeVCL.AddFormTo(IGLEdit,TabOpenGL);
end;

procedure TChartBlock3DEditor.FormDestroy(Sender: TObject);
begin
  IGLEdit.Free;
  IEdit.Free;
  I3DEdit.Free;
end;

end.
