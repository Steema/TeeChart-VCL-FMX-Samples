unit TeeActionGallery;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls, QExtCtrls,
  QButtons,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  {$ENDIF}
  TeCanvas, TeeBlocks, TeeLoadBlock, TeeBlockEditor, TeeMakerControl,
  TeeAnimate, TeeProcs;

type
  TActionGallery = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabLoad: TTabSheet;
    TabBrowse: TTabSheet;
    TabPlay: TTabSheet;
    Label1: TLabel;
    CBURL: TComboFlat;
    Label2: TLabel;
    CBObject: TComboFlat;
    Label3: TLabel;
    CBAnimation: TComboFlat;
    BTestAnimation: TButton;
    TabAction: TTabSheet;
    Label4: TLabel;
    CBObjectAction: TComboFlat;
    TabEvents: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    CBObjectEvents: TComboFlat;
    CBEvents: TComboFlat;
    Label7: TLabel;
    TabSetProperty: TTabSheet;
    Button1: TButton;
    LabelProperty: TLabel;
    GroupRight: TGroupBox;
    RBConstant: TRadioButton;
    RBProperty: TRadioButton;
    BSelectRight: TButton;
    EConstant: TEdit;
    CBNot: TCheckBox;
    LabelRight: TLabel;
    TabDrag: TTabSheet;
    LabelDrag: TLabel;
    BDrag: TButton;
    CBDragInvert: TCheckBox;
    CBDragMinMax: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    EDragMin: TEdit;
    EDragMax: TEdit;
    SBConstant: TSpeedButton;
    RBObjectEvent: TRadioButton;
    RBDelphiEvent: TRadioButton;
    TabSound: TTabSheet;
    Label10: TLabel;
    CBSoundFile: TComboFlat;
    SpeedButton1: TSpeedButton;
    TabDelay: TTabSheet;
    Label11: TLabel;
    EDelay: TEdit;
    Panel2: TPanel;
    BOK: TButton;
    Button2: TButton;
    GroupAnimation: TGroupBox;
    CBAnimated: TCheckBox;
    Label12: TLabel;
    TBDuration: TTrackBar;
    LabelDuration: TLabel;
    ListActions: TListBox;
    BlockEvents: TTreeView;
    BSoundPreview: TButton;
    Splitter1: TSplitter;
    CBIncrement: TCheckBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Button3: TButton;
    LDragAction: TLabel;
    TabRepaint: TTabSheet;
    TabTimer: TTabSheet;
    Label13: TLabel;
    ETimer: TEdit;
    UDTimer: TUpDown;
    CBTimerRepeat: TCheckBox;
    Button4: TButton;
    LTimerAction: TLabel;
    SpeedButton4: TSpeedButton;
    LTimerInterval: TLabel;
    SpeedButton5: TSpeedButton;
    LDelay: TLabel;
    UDDelay: TUpDown;
    procedure CBObjectChange(Sender: TObject);
    procedure CBAnimationChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BTestAnimationClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure CBObjectActionChange(Sender: TObject);
    procedure CBURLChange(Sender: TObject);
    procedure CBObjectEventsChange(Sender: TObject);
    procedure CBEventsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RBConstantClick(Sender: TObject);
    procedure RBPropertyClick(Sender: TObject);
    procedure BSelectRightClick(Sender: TObject);
    procedure EConstantChange(Sender: TObject);
    procedure BDragClick(Sender: TObject);
    procedure CBDragMinMaxClick(Sender: TObject);
    procedure SBConstantClick(Sender: TObject);
    procedure RBObjectEventClick(Sender: TObject);
    procedure RBDelphiEventClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CBSoundFileChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TBDurationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListActionsClick(Sender: TObject);
    procedure BlockEventsChange(Sender: TObject; Node: TTreeNode);
    procedure BSoundPreviewClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure ETimerChange(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure EDelayChange(Sender: TObject);
  private
    { Private declarations }

    ILoadDialog : TLoadBlockDialog;

    DragComp,
    LeftComp,
    RightComp : TObject;

    DragProp,
    LeftProp,
    RightProp : String;

    procedure ChangeEvents(AObjects,AEvents:TComboBox);
    class function CheckObjectName(AObject:TCustomBlock): String;
    procedure FillObjectsAction;
    procedure FillObjectsEvents(AObjects,AEvents:TComboBox);
    function ObjectAction(AComp:TObject; const AProp:String):String;
    function ObjectEvent(AObjects,AEvents:TComboBox):String;
    function SelectProperty(var AObject:TObject; var AName:String):Boolean;
  public
    { Public declarations }

    Blocks  : TBlocks;
    Current : TCustomBlock;
    Maker   : TMaker;

    function ActionText:String;
    class procedure AddBasicEvents(Tree:TTreeView);
    class procedure AddTreeEvent(Tree:TTreeView; Text:String);
    class function IndexToEvent(Node:TTreeNode):String;
    class function ModalShow(AOwner:TComponent; ABlock:TCustomBlock;
                             AMaker:TMaker;
                             out ResultAction:String; const AAction:String=''):Boolean;

    class function ObjectPathName(AObject:TCustomBlock): String;

    class function PropertyText(const AObject:TObject; const AName:String):String; overload;
    class function PropertyText(Link:TPropertyLink):String; overload;

    procedure SetActionText(const Value:String);
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeePenDlg, TeeMakerConst, TeeSoundSelector, Registry, TeePlayMP3;

{ TActionGallery }

class function TActionGallery.ModalShow(AOwner:TComponent; ABlock:TCustomBlock;
                                        AMaker:TMaker;
                                        out ResultAction:String;
                                        const AAction:String=''):Boolean;
var
  tmp: TActionGallery;
begin
  tmp := TActionGallery.Create(AOwner);
  with tmp do
  try
    Current:=ABlock;
    Maker:=AMaker;

    if Assigned(Current) and Assigned(Current.Parent) then
       Blocks:=Current.Parent.DrawBlocks
    else
    if Assigned(Maker) then
       Blocks:=Maker.Blocks
    else
       Blocks:=nil;

    if AAction<>'' then
       SetActionText(AAction);

    TeeTranslateControl(tmp);

    result:=ShowModal=mrOk;

    if result then
       ResultAction:=ActionText;
  finally
    Free;
  end;
end;

type
  TBlocksAccess=class(TBlocks);

class function TActionGallery.CheckObjectName(AObject:TCustomBlock): String;
begin
  result:=AObject.Name;

  if (result='') and Assigned(AObject.Parent) then
     result:=IntToStr(AObject.Index);
end;

class function TActionGallery.ObjectPathName(AObject:TCustomBlock): String;
begin
  result:=CheckObjectName(AObject);

  while Assigned(TBlocksAccess(AObject.Parent).IObject) do
  begin
    result:=CheckObjectName(TBlocksAccess(AObject.Parent).IObject)+'.'+result;
    AObject:=TBlocksAccess(AObject.Parent).IObject;
  end;
end;

class function TActionGallery.IndexToEvent(Node:TTreeNode):String;
begin
  if not Assigned(Node) then
     result:=''
  else
  begin
    result:=Node.Text;

    while Assigned(Node.Parent) do
    begin
      Node:=Node.Parent;
      result:=Node.Text+'.'+result;
    end;

    result:=MakerEvents.PathToID(result);
  end;
end;

function CheckAddQuotes(const AText:String):String;
begin
  if (Pos(' ',AText)>0) or (Pos('.',AText)>0) or (Pos(':',AText)>0) then
     result:='"'+AText+'"'
  else
     result:=AText;
end;

procedure TActionGallery.ETimerChange(Sender: TObject);
begin
  LTimerInterval.Caption:=IntToStr(UDTimer.Position);
end;

function TActionGallery.ObjectAction(AComp:TObject; const AProp:String):String;
begin
  if AComp is TCustomBlock then
     if AComp=Current then
        result:=AProp
     else
        result:=ObjectPathName(TCustomBlock(AComp))+'.'+AProp
  else
  if AComp is TComponent then
     result:=TComponent(AComp).Name+'.'+AProp
  else
     result:=AProp;
end;

function TActionGallery.ActionText: String;

  function IsPositive(const AText:String):String;
  begin
    if StrToFloat(AText)>0 then result:='+'
                           else result:='';
  end;

var tmp : TCustomObjectBlock;
    tmpTarget : TCustomBlock;
begin
  if PageControl1.ActivePage=TabLoad then
     result:=ILoadDialog.GetSelectedText
  else
  if PageControl1.ActivePage=TabBrowse then
     result:=CBURL.Text
  else
  if PageControl1.ActivePage=TabAction then
  begin
    result:='Action:';

    if CBObjectAction.ItemIndex<>-1 then
    begin
      tmpTarget:=TCustomBlock(CBObjectAction.Items.Objects[CBObjectAction.ItemIndex]);

      result:=result+ObjectPathName(tmpTarget);

      if Assigned(BlockEvents.Selected) then
         result:=result+'.'+IndexToEvent(BlockEvents.Selected);
    end;
  end
  else
  if PageControl1.ActivePage=TabPlay then
  begin
    result:='Animation:';

    with CBObject do
    if ItemIndex<>-1 then
    begin
      tmp:=TCustomObjectBlock(Items.Objects[ItemIndex]);

      if Assigned(tmp) then
         result:=result+ObjectPathName(tmp);
    end;

    result:=result+':';

    with CBAnimation do
    if ItemIndex<>-1 then
       result:=result+CheckAddQuotes(Items[ItemIndex]);
  end
  else
  if PageControl1.ActivePage=TabSetProperty then
  begin
    result:='Set: '+ObjectAction(LeftComp,LeftProp)+' = ';

    if RBConstant.Checked then
    begin
      if CBIncrement.Checked then
         result:=result+'+='+EConstant.Text
      else
         result:=result+'='+EConstant.Text
    end
    else
    if Assigned(RightComp) then
    begin
      if CBNot.Checked then
         result:=result+' not ';

      if RightComp is TCustomBlock then
         result:=result+ObjectPathName(TCustomBlock(RightComp))+'.'+RightProp
      else
      if RightComp is TComponent then
         result:=result+TComponent(RightComp).Name+'.'+RightProp
      else
         result:=result+RightProp;
    end
    else
       result:=result+RightProp;

    if CBAnimated.Checked then
       result:=result+',Animated,'+LabelDuration.Caption;
  end
  else
  if PageControl1.ActivePage=TabDrag then
  begin
    result:='Drag: '+ObjectAction(DragComp,DragProp);

    if CBDragMinMax.Checked then
       result:=result+',MinMax:'+EDragMin.Text+';'+EDragMax.Text;

    if CBDragInvert.Checked then
       result:=result+',Invert';

    if LDragAction.Caption<>'' then
       result:=result+',Action:'+LDragAction.Caption;
  end
  else
  if PageControl1.ActivePage=TabSound then
     result:='Sound:'+CBSoundFile.Text
  else
  if PageControl1.ActivePage=TabDelay then
     result:='Delay:'+IntToStr(StrToInt(EDelay.Text)) // <-- Verify integer
  else
  if PageControl1.ActivePage=TabEvents then
  begin
    result:='Event:';

    if RBObjectEvent.Checked then
       result:=result+ObjectEvent(CBObjectEvents,CBEvents);
  end
  else
  if PageControl1.ActivePage=TabRepaint then
     result:='Repaint:'
  else
  if PageControl1.ActivePage=TabTimer then
  begin
    result:='Timer:'+LTimerInterval.Caption;

    if CBTimerRepeat.Checked then
       result:=result+',Repeat';

    if LTimerAction.Caption<>'' then
       result:=result+',Action:'+LTimerAction.Caption;
  end
  else
    result:='';
end;

function TActionGallery.ObjectEvent(AObjects,AEvents:TComboBox):String;
var tmp : TCustomObjectBlock;
begin
  result:='';

  with AObjects do
  if ItemIndex<>-1 then
  begin
    tmp:=TCustomObjectBlock(TBlocksAccess(Items.Objects[ItemIndex]).IObject);

    if Assigned(tmp) then
       result:=result+ObjectPathName(tmp)+':';
  end;

  with AEvents do
  if ItemIndex<>-1 then
     result:=result+CheckAddQuotes(Items[ItemIndex]);
end;

procedure TActionGallery.CBObjectChange(Sender: TObject);
var t : Integer;
    tmp : TCustomObjectBlock;
    tmpBlocks : TBlocks;
begin
  CBAnimation.Clear;

  if CBObject.ItemIndex<>-1 then
  begin
    tmp:=TCustomObjectBlock(CBObject.Items.Objects[CBObject.ItemIndex]);

    if Assigned(tmp) then
       tmpBlocks:=tmp.Items
    else
       tmpBlocks:=Blocks;

    if tmpBlocks.HasAnimations then
    with tmpBlocks.Animates do
    for t:=0 to Count-1 do
        if Item[t].Animations.Count>0 then
           CBAnimation.Items.AddObject(Item[t].Description,Item[t]);
  end;

  CBAnimationChange(Self);
end;

procedure TActionGallery.CBAnimationChange(Sender: TObject);
begin
  BTestAnimation.Enabled:=CBAnimation.ItemIndex<>-1;
  BOK.Enabled:=BTestAnimation.Enabled;
end;

procedure TActionGallery.FormShow(Sender: TObject);

  procedure LoadOptions;
  begin
    with TRegistry.Create do
    try
      if OpenKeyReadOnly(TeeMakerKey+'\ActionGallery') then
      begin
        Self.Height:=ReadInteger('Height');
        Self.Width:=ReadInteger('Width');

        CloseKey;
      end;
    finally
      Free;
    end;
  end;

begin
  LoadOptions;
end;

procedure TActionGallery.BTestAnimationClick(Sender: TObject);
begin
  TAnimateItem(CBAnimation.Items.Objects[CBAnimation.ItemIndex]).Animate.Play;
end;

procedure TActionGallery.PageControl1Change(Sender: TObject);

  procedure AddObjects(AObject:TCustomObjectBlock; const AName:String);
  var tmpBlocks : TBlocks;
      t : Integer;
      tmpName : String;
  begin
    if Assigned(AObject) then
       tmpBlocks:=AObject.Items
    else
       tmpBlocks:=Blocks;

    with tmpBlocks do
    begin
      if HasAnimations then
         CBObject.Items.AddObject(AName,AObject);

      for t:=0 to Count-1 do
      if Block[t] is TCustomObjectBlock then
      begin
        tmpName:=ObjectPathName(TCustomObjectBlock(Block[t]));

        if TCustomObjectBlock(Block[t]).LinkFile<>'' then
           tmpName:=tmpName+' ('+ExtractFileName(TCustomObjectBlock(Block[t]).LinkFile)+')';

        AddObjects(TCustomObjectBlock(Block[t]),tmpName);
      end;
    end;
  end;

begin
  BOK.Enabled:=False;

  if PageControl1.ActivePage=Tabplay then
  begin
    if CBObject.Items.Count=0 then
    begin
      CBObject.Clear;
      AddObjects(nil,'');

      if CBObject.Items.Count>0 then
         CBObject.ItemIndex:=0;

      CBObjectChange(Self);
    end;
  end
  else
  if PageControl1.ActivePage=TabLoad then
  begin
    if TabLoad.ControlCount=0 then
    begin
      ILoadDialog:=TLoadBlockDialog.Create(Self);
      ILoadDialog.PanelButtons.Visible:=False;
      TeeTranslateControl(ILoadDialog);
      TTeeVCL.AddFormTo(ILoadDialog,TabLoad);
    end;
  end
  else
  if PageControl1.ActivePage=TabAction then
  begin
    if CBObjectAction.Items.Count=0 then
       FillObjectsAction;
  end
  else
  if PageControl1.ActivePage=TabEvents then
  begin
    if CBObjectEvents.Items.Count=0 then
       FillObjectsEvents(CBObjectEvents,CBEvents);
  end
  else
  if PageControl1.ActivePage=TabDelay then
     BOK.Enabled:=True
  else
  if PageControl1.ActivePage=TabRepaint then
     BOK.Enabled:=True
end;

class procedure TActionGallery.AddBasicEvents(Tree:TTreeView);
var t : Integer;
begin
  for t:=0 to MakerEvents.Count-1 do
      AddTreeEvent(Tree,TActionEvent(MakerEvents[t]).Path);
end;

class procedure TActionGallery.AddTreeEvent(Tree:TTreeView; Text:String);

  function FindNode(Parent:TTreeNode; const AText:String):TTreeNode;
  var t : Integer;
  begin
    result:=nil;

    if Assigned(Parent) then
    begin
      for t:=0 to Parent.Count-1 do
      if UpperCase(Parent[t].Text)=AText then
      begin
        result:=Parent[t];
        break;
      end;
    end
    else
      for t:=0 to Tree.Items.Count-1 do
      if UpperCase(Tree.Items[t].Text)=AText then
      begin
        result:=Tree.Items[t];
        break;
      end;
  end;

var Node : TTreeNode;
    i    : Integer;
    tmp  : String;
    tmpNode : TTreeNode;
begin
  Node:=nil;

  i:=Pos('.',Text);
  while i>0 do
  begin
    tmp:=Copy(Text,1,i-1);
    Delete(Text,1,i);

    tmpNode:=FindNode(Node,UpperCase(tmp));

    if Assigned(tmpNode) then
       Node:=tmpNode
    else
       Node:=Tree.Items.AddChild(Node,tmp);

    i:=Pos('.',Text);
  end;

  Tree.Items.AddChild(Node,Text);
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TActionGallery.CBObjectActionChange(Sender: TObject);
var t   : Integer;
    tmp : TCustomBlock;
begin
  BlockEvents.Items.Clear;

  if CBObjectAction.ItemIndex<>-1 then
  begin
    tmp:=TCustomBlock(CBObjectAction.Items.Objects[CBObjectAction.ItemIndex]);

    if TBlockAccess(tmp).HasActions then
    begin
      for t:=0 to tmp.Actions.Count-1 do
          AddTreeEvent(BlockEvents,MakerEvents.IDToPath(tmp.Actions[t].Trigger));

      BlockEvents.Items[0].Expanded:=True;
    end;

    BOK.Enabled:=Assigned(BlockEvents.Selected) and (BlockEvents.Selected.Count=0);
  end;
end;

procedure TActionGallery.CBURLChange(Sender: TObject);
begin
  BOK.Enabled:=Trim(CBURL.Text)<>'';
end;

type
  TCustomObjectBlockAccess=class(TCustomObjectBlock);

procedure TActionGallery.FillObjectsAction;

  procedure AddObjectsAction(ABlocks:TBlocks; const AName:String);
  var t       : Integer;
      tmpName : String;
      tmp     : TCustomBlock;
  begin
    with ABlocks do
    for t:=0 to Count-1 do
    begin
      tmp:=Block[t];

      if TBlockAccess(tmp).HasActions then
         if tmp<>Current then
            CBObjectAction.Items.AddObject(AName+TBlockAccess(tmp).TitleOrName,tmp);

      if tmp is TCustomObjectBlock then
      if TCustomObjectBlockAccess(tmp).ItemsReady then
      begin
        tmpName:=ObjectPathName(TCustomObjectBlock(tmp));

        if TCustomObjectBlock(tmp).LinkFile<>'' then
           tmpName:=tmpName+
             ' ('+RemoveFileExtension(ExtractFileName(TCustomObjectBlock(tmp).LinkFile))+') ';

        AddObjectsAction(TCustomObjectBlock(tmp).Items,tmpName);
      end;
    end;
  end;

begin
  CBObjectAction.Clear;
  AddObjectsAction(Blocks,'');

  if CBObjectAction.Items.Count>0 then
     CBObjectAction.ItemIndex:=0;

  CBObjectActionChange(Self);
end;

procedure TActionGallery.FillObjectsEvents(AObjects,AEvents:TComboBox);

  procedure AddObjectsEvents(ABlocks:TBlocks; const AName:String);
  var t       : Integer;
      tmpName : String;
      tmp     : TCustomBlock;
  begin
    if TBlocksAccess(ABlocks).HasEvents then
       AObjects.Items.AddObject(AName,ABlocks);

    with ABlocks do
    for t:=0 to Count-1 do
    begin
      tmp:=Block[t];

      if tmp is TCustomObjectBlock then
      if TCustomObjectBlockAccess(tmp).ItemsReady then
      begin
        tmpName:=ObjectPathName(TCustomObjectBlock(tmp));

        if TCustomObjectBlock(tmp).LinkFile<>'' then
           tmpName:=tmpName+' ('+ExtractFileName(TCustomObjectBlock(tmp).LinkFile)+')';

        AddObjectsEvents(TCustomObjectBlock(tmp).Items,tmpName);
      end;
    end;
  end;

begin
  AObjects.Clear;
  AddObjectsEvents(Blocks,'');

  if AObjects.Items.Count>0 then
     AObjects.ItemIndex:=0;

  AObjects.OnChange(Self);
end;

procedure TActionGallery.ChangeEvents(AObjects,AEvents:TComboBox);
var tmpBlocks : TBlocks;
begin
  AEvents.Clear;

  if AObjects.ItemIndex<>-1 then
  begin
    tmpBlocks:=TBlocks(AObjects.Items.Objects[AObjects.ItemIndex]);

    if TBlocksAccess(tmpBlocks).HasEvents then
       AEvents.Items.AddStrings(tmpBlocks.Events);
  end;

  if Assigned(AEvents.OnChange) then
     AEvents.OnChange(Self);
end;

procedure TActionGallery.CBObjectEventsChange(Sender: TObject);
begin
  ChangeEvents(CBObjectEvents,CBEvents);
end;

procedure TActionGallery.CBEventsChange(Sender: TObject);
begin
  BOK.Enabled:=CBEvents.ItemIndex<>-1;
end;

function TActionGallery.SelectProperty(var AObject:TObject; var AName:String):Boolean;
begin
  result:=TMakerPropertySelector.ModalShow(Self,Blocks,AObject,AName);
end;

procedure TActionGallery.BDragClick(Sender: TObject);
begin
  if SelectProperty(DragComp,DragProp) then
  begin
    LabelDrag.Caption:=PropertyText(DragComp,DragProp);
    BOK.Enabled:=True;
  end;
end;

procedure TActionGallery.Button1Click(Sender: TObject);
var tmpObj  : TObject;
    tmpName : String;
begin
  if SelectProperty(LeftComp,LeftProp) then
  begin
    LabelProperty.Caption:=PropertyText(LeftComp,LeftProp);

    GroupRight.Enabled:=True;
    GroupAnimation.Enabled:=True;

    tmpObj:=LeftComp;
    tmpName:=LeftProp;

    TPropertyAnimation.Fixup(tmpObj,tmpName);

    SBConstant.Visible:=IsColorProperty(tmpObj,tmpName);

    if RBConstant.Checked then
       EConstant.SetFocus;
  end;
end;

procedure TActionGallery.SetActionText(const Value:String);
var tmp : String;
begin
  ListActions.Visible:=False;
  Splitter1.Visible:=False;

  tmp:=UpperCase(Trim(Value));

  case TBlockActions.ActionToIndex(tmp) of
    0: PageControl1.ActivePage:=TabPlay; //(Copy(tmp,11,Length(tmp)));
    1: PageControl1.ActivePage:=TabAction; //(Copy(tmp,8,Length(tmp)));
    2: PageControl1.ActivePage:=TabEvents; //(Copy(tmp,8,Length(tmp)));
    3: PageControl1.ActivePage:=TabSetProperty; //Copy(tmp,5,Length(tmp)));
    4: PageControl1.ActivePage:=TabDrag; //(Copy(tmp,6,Length(tmp)));
    5: PageControl1.ActivePage:=TabSound; //(Copy(tmp,7,Length(tmp)));
    6: PageControl1.ActivePage:=TabDelay; //(Copy(tmp,7,Length(tmp)));
    7: PageControl1.ActivePage:=TabRepaint;
    8: PageControl1.ActivePage:=TabTimer;
  else
  if TeeIsURL(tmp) then
  begin
    PageControl1.ActivePage:=TabBrowse;
    CBURL.Text:=Value;
  end
  else
  begin
    PageControl1.ActivePage:=TabLoad;
  end;
  end;
end;

class function TActionGallery.PropertyText(Link:TPropertyLink):String;
begin
  result:=PropertyText(Link.Instance,Link.PropertyName);
end;

class function TActionGallery.PropertyText(const AObject:TObject; const AName:String):String;
begin
  if AObject is TCustomBlock then
     result:=TCustomBlock(AObject).Title+' ('+ObjectPathName(TCustomBlock(AObject))+') '+AName
  else
  if AObject is TComponent then
     result:=TComponent(AObject).Name+' '+AName
  else
     result:=AName;
end;

procedure TActionGallery.RBConstantClick(Sender: TObject);
begin
  RBProperty.Checked:=False;
  CBNot.Enabled:=False;
  BSelectRight.Enabled:=False;
  EConstant.Enabled:=True;
  EConstant.SetFocus;
end;

procedure TActionGallery.RBPropertyClick(Sender: TObject);
begin
  RBConstant.Checked:=False;
  EConstant.Enabled:=False;
  CBNot.Enabled:=True;
  BSelectRight.Enabled:=True;
end;

procedure TActionGallery.BSelectRightClick(Sender: TObject);
begin
  if SelectProperty(RightComp,RightProp) then
  begin
    LabelRight.Caption:=PropertyText(RightComp,RightProp);
    BOK.Enabled:=True;
  end;
end;

procedure TActionGallery.EConstantChange(Sender: TObject);
begin
  BOK.Enabled:=True;
end;

procedure TActionGallery.CBDragMinMaxClick(Sender: TObject);
begin
  EDragMin.Enabled:=CBDragMinMax.Checked;
  EDragMax.Enabled:=EDragMin.Enabled;
end;

procedure TActionGallery.SBConstantClick(Sender: TObject);
var tmp : TColor;
begin
  if EConstant.Text<>'' then
     tmp:=StringToColor(EConstant.Text)
  else
     tmp:=clWhite;

  EConstant.Text:=TeeStr(TButtonColor.Edit(Self,tmp));
end;

procedure TActionGallery.RBObjectEventClick(Sender: TObject);
begin
  RBDelphiEvent.Checked:=False;
  CBObjectEvents.SetFocus;
  CBEventsChange(Self);
end;

procedure TActionGallery.RBDelphiEventClick(Sender: TObject);
begin
  RBObjectEvent.Checked:=False;
  BOK.Enabled:=True;
end;

procedure TActionGallery.SpeedButton1Click(Sender: TObject);
var tmp : String;
begin
  tmp:=TSoundSelector.ModalShow(Self,Blocks.LibraryPath);

  if tmp<>'' then
  begin
    with CBSoundFile.Items do
    if IndexOf(tmp)=-1 then
       Add(tmp);

    CBSoundFile.ItemIndex:=CBSoundFile.Items.IndexOf(tmp);
    CBSoundFileChange(Self);
  end;
end;

procedure TActionGallery.CBSoundFileChange(Sender: TObject);
begin
  BOK.Enabled:=CBSoundFile.Text<>'';
  BSoundPreview.Enabled:=BOK.Enabled;
end;

procedure TActionGallery.FormDestroy(Sender: TObject);
begin
  with TRegistry.Create do
  try
    if OpenKey(TeeMakerKey+'\ActionGallery',True) then
    begin
      WriteInteger('Height',Self.Height);
      WriteInteger('Width',Self.Width);

      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TActionGallery.TBDurationChange(Sender: TObject);
begin
  LabelDuration.Caption:=TeeStr(TBDuration.Position);
end;

procedure TActionGallery.FormCreate(Sender: TObject);
var t : Integer;
begin
  for t:=0 to PageControl1.PageCount-1 do
      ListActions.Items.AddObject(PageControl1.Pages[t].Caption,TObject(t));

  ListActions.Sorted:=True;

  for t:=0 to PageControl1.PageCount-1 do
      PageControl1.Pages[t].TabVisible:=False;

  PageControl1.ActivePage:=TabSetProperty;
  ListActions.ItemIndex:=TabSetProperty.TabIndex;
end;

procedure TActionGallery.ListActionsClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=Integer(ListActions.Items.Objects[ListActions.ItemIndex]);

  PageControl1.ActivePage:=PageControl1.Pages[tmp];
  PageControl1Change(Self);
end;

procedure TActionGallery.BlockEventsChange(Sender: TObject;
  Node: TTreeNode);
begin
  BOK.Enabled:=Node.Count=0;
end;

procedure TActionGallery.BSoundPreviewClick(Sender: TObject);
begin
  TPlayMP3Sound.PlayFile(CBSoundFile.Text);
end;

procedure TActionGallery.SpeedButton2Click(Sender: TObject);
var tmpObj  : TObject;
    tmpProp : String;
begin
  tmpObj:=nil;
  tmpProp:='';

  if SelectProperty(tmpObj,tmpProp) then
     EDragMin.Text:=ObjectAction(tmpObj,tmpProp);
end;

procedure TActionGallery.SpeedButton3Click(Sender: TObject);
var tmpObj : TObject;
    tmpProp : String;
begin
  tmpObj:=nil;
  tmpProp:='';

  if SelectProperty(tmpObj,tmpProp) then
     EDragMax.Text:=ObjectAction(tmpObj,tmpProp);
end;

procedure TActionGallery.Button3Click(Sender: TObject);
var ActionText : String;
begin
  if TActionGallery.ModalShow(Self,Current,Maker,ActionText) then
     LDragAction.Caption:=ActionText;
end;

procedure TActionGallery.Button4Click(Sender: TObject);
var ActionText : String;
begin
  if TActionGallery.ModalShow(Self,Current,Maker,ActionText) then
  begin
    LTimerAction.Caption:=ActionText;
    BOK.Enabled:=ActionText<>'';
  end;
end;

procedure TActionGallery.SpeedButton4Click(Sender: TObject);
var TimerComp : TObject;
    TimerProp : String;
begin
  TimerComp:=nil;
  TimerProp:='';

  if SelectProperty(TimerComp,TimerProp) then
     LTimerInterval.Caption:=PropertyText(TimerComp,TimerProp);
end;

procedure TActionGallery.SpeedButton5Click(Sender: TObject);
var DelayComp : TObject;
    DelayProp : String;
begin
  DelayComp:=nil;
  DelayProp:='';
  
  if SelectProperty(DelayComp,DelayProp) then
     LDelay.Caption:=PropertyText(DelayComp,DelayProp);
end;

procedure TActionGallery.EDelayChange(Sender: TObject);
begin
  LDelay.Caption:=IntToStr(UDDelay.Position);
end;

end.

