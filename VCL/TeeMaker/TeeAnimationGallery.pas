unit TeeAnimationGallery;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs, TeeAnimate, TeeSelectProperty, TeePenDlg;

type
  TPropertySelectorClass=class of TPropertySelector;

  TAnimationGallery = class(TForm)
    Panel1: TPanel;
    BOK: TButton;
    BCancel: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    ComboClasses: TComboFlat;
    procedure ComboClassesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    ISelect : TPropertySelector;

    procedure CheckSelect;
    function SelectedClass:TTeeAnimationClass;
    procedure TreeObjectsClick(Sender: TObject);
    procedure TreePropsClick(Sender: TObject);
  public
    { Public declarations }
    Animate   : TTeeAnimate;
    ParentAnimation : TTeeAnimation;

    procedure CreateSelector(const AClass:TPropertySelectorClass);
    function GetAnimation(const AOwner:TComponent):TTeeAnimation;

    property Selector:TPropertySelector read ISelect;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

type
  TPropertyAnimationAccess=class(TPropertyAnimation);

function TAnimationGallery.GetAnimation(const AOwner:TComponent):TTeeAnimation;
var tmp : TObject;
    tmpSt   : String;
begin
  result:=SelectedClass.Create(AOwner);

  if result is TPropertyAnimation then
  begin
    tmp:=ISelect.SelectedProperty(tmpSt);
    TPropertyAnimation(result).Instance:=tmp as TComponent;
    TPropertyAnimationAccess(result).PropertyName:=tmpSt;
  end;
end;

function TAnimationGallery.SelectedClass:TTeeAnimationClass;
begin
  if ComboClasses.ItemIndex=-1 then
     result:=nil
  else
     result:=TTeeAnimationClass(ComboClasses.Items.Objects[ComboClasses.ItemIndex]);
end;

procedure TAnimationGallery.ComboClassesChange(Sender: TObject);
var tmp : TTeeAnimationClass;
    tmpO : TObject;
begin
  tmp:=SelectedClass;

  tmpO:=tmp.Create(nil);
  ISelect.Visible:=tmpO is TPropertyAnimation;
  tmpO.Free;

  if ISelect.Visible then
     TreePropsClick(Self)
  else
     BOK.Enabled:=True;
end;

procedure TAnimationGallery.TreeObjectsClick(Sender: TObject);
begin
  with ISelect.TreeObjects do
  if Selected=nil then
     BOK.Enabled:=False
  else
     BOK.Enabled:=(SelectedClass<>nil) and
                  SelectedClass.IsValidSource(TObject(Selected.Data),True) and
                  (ISelect.TreeProps.Selected=nil);
end;

procedure TAnimationGallery.TreePropsClick(Sender: TObject);
begin
  with ISelect.TreeProps do
  if Selected=nil then
     BOK.Enabled:=False
  else
     BOK.Enabled:=(SelectedClass<>nil) and
               SelectedClass.IsValidSource(TObject(Selected.Data),Selected.Count>0);
end;

procedure TAnimationGallery.CheckSelect;
begin
  if not Assigned(ISelect) then
     CreateSelector(TPropertySelector);
end;

type
  TAnimationAccess=class(TTeeAnimation);

procedure TAnimationGallery.FormShow(Sender: TObject);
var t : Integer;
    tmpClass : TTeeAnimationClass;
begin
  ComboClasses.Items.Clear;

  for t:=0 to TeeAnimationClasses.Count-1 do
  begin
    tmpClass:=TTeeAnimationClass(GetClass(TeeAnimationClasses[t]));

    if (not Assigned(ParentAnimation)) or
       TAnimationAccess(ParentAnimation).IsValidChild(tmpClass) then
          ComboClasses.Items.AddObject(tmpClass.Description,TObject(tmpClass));
  end;

  with ComboClasses do
  begin
    ItemIndex:=Items.IndexOfObject(TObject(TNumberAnimation));

    if ItemIndex=-1 then
       ItemIndex:=0;
  end;

  CheckSelect;
end;

procedure TAnimationGallery.CreateSelector(const AClass: TPropertySelectorClass);
begin
  ISelect:=AClass.Create(Self);
  ISelect.LabelClass.Parent:=Panel1;
  ISelect.PanelButtons.Visible:=False;
  ISelect.Align:=alClient;
  TTeeVCL.AddFormTo(ISelect,Self);

  ISelect.TreeProps.OnClick:=TreePropsClick;
  ISelect.TreeObjects.OnClick:=TreeObjectsClick;
end;

end.
