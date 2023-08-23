unit TeeActionAnimation;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  TeeAnimate, TeeAnimationEditor, TeeBlocks, TeCanvas, TeeProcs;

type
  TBlockActionAnimation=class(TTeeAnimation)
  private
    FBlock       : TCustomBlock;
    FBlockAction : String;
    procedure SetBlock(const Value: TCustomBlock);
  public
    function EditorClass:String; override;
    Procedure Play; override;
  published
    property Block:TCustomBlock read FBlock write SetBlock;
    property BlockAction:String read FBlockAction write FBlockAction;
  end;

  TActionAnimationEditor = class(TForm)
    PageControl1: TPageControl;
    TabGeneral: TTabSheet;
    TabAction: TTabSheet;
    Button1: TButton;
    LabelAction: TLabel;
    Label1: TLabel;
    ComboBlocks: TComboFlat;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    IBasic    : TAnimationEditor;
  public
    { Public declarations }
    Animation : TBlockActionAnimation;
  end;

implementation

{$R *.dfm}

uses
  TeePenDlg, TeeActionGallery, TeeMakerControl;

procedure TActionAnimationEditor.Button1Click(Sender: TObject);
var ActionText : String;
begin
  ActionText:=LabelAction.Caption;

  if TActionGallery.ModalShow(Self,Animation.Block,Animation.Animate.Panel as TMaker,ActionText,ActionText) then
  begin
    Animation.BlockAction:=ActionText;
    LabelAction.Caption:=ActionText;
  end;
end;

{ TBlockActionAnimation }

function TBlockActionAnimation.EditorClass: String;
begin
  result:='TActionAnimationEditor';
end;

procedure TBlockActionAnimation.Play;
begin
  if FBlockAction<>'' then
     (Animate.Panel as TMaker).DoSingleAction(FBlock,FBlockAction);
end;

procedure TBlockActionAnimation.SetBlock(const Value: TCustomBlock);
begin
  // FreeNotification
  FBlock := Value;
end;

procedure TActionAnimationEditor.FormShow(Sender: TObject);
begin
  Animation:=TBlockActionAnimation(Tag);

  if Assigned(Animation) then
  begin
    if not Assigned(IBasic) then
    begin
      IBasic:=TAnimationEditor.Create(Self);
      IBasic.Align:=alClient;
      TeeTranslateControl(IBasic);
      TTeeVCL.AddFormTo(IBasic,TabGeneral,Animation);
    end;

    IBasic.RefreshAnimation(Animation);

    // Block combo
    LabelAction.Caption:=Animation.BlockAction;
  end;
end;

initialization
  TeeRegisterAnimation(TBlockActionAnimation);
  RegisterClass(TActionAnimationEditor);
end.
