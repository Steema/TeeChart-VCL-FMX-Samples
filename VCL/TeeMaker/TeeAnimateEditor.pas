{**********************************************}
{   TTeeAnimate Editor Dialog                  }
{   Copyright (c) 2001-2023 by Steema Software }
{**********************************************}
unit TeeAnimateEditor;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons, ComCtrls, Menus,
  StdCtrls, Grids,
  {$IFDEF D9}
  Types,
  {$ENDIF}

  TeCanvas, TeeAnimate, TeeInspector, ImgList, TeePenDlg;

type
  TAnimateEditorEvent=procedure(Sender:TObject; out Animation:TTeeAnimation) of object;
  TAnimateGalleryEvent=procedure(Sender:TObject; out Animation:TTeeAnimation; const AParent:TTeeAnimation) of object;
  TAnimateGetName=procedure(const Animation:TTeeAnimation; out S:String) of object;

  TClickedItem=(ciNone,ciBody,ciLeftSide,ciRightSide);

  TTeeAnimateEditor = class(TForm)
    Panel1: TPanel;
    Panel4: TPanel;
    SBPlay: TSpeedButton;
    SBPause: TSpeedButton;
    SBStop: TSpeedButton;
    SBFrame: TSpeedButton;
    TBFramesPerSecond: TTrackBar;
    CBLoop: TCheckBox;
    Splitter1: TSplitter;
    PanelAnim: TPanel;
    PanelAnimTop: TPanel;
    PanelAnimTree: TPanel;
    Splitter2: TSplitter;
    Panel2: TPanel;
    UpScale: TUpDown;
    TeeInspector1: TTeeInspector;
    Panel3: TPanel;
    SBClose: TSpeedButton;
    StatusBar1: TStatusBar;
    PanelEditor: TPanel;
    UpHeight: TUpDown;
    PopupMenu1: TPopupMenu;
    Insert1: TMenuItem;
    Delete1: TMenuItem;
    Duplicate1: TMenuItem;
    PanelGroups: TPanel;
    ComboGroups: TComboFlat;
    SBAddGroup: TSpeedButton;
    SBRemoveGroup: TSpeedButton;
    SBCloseTree: TSpeedButton;
    ListAnim: TTreeView;
    LFramesPerSecond: TLabel;
    N1: TMenuItem;
    View1: TMenuItem;
    Flat1: TMenuItem;
    ree1: TMenuItem;
    Images: TImageList;
    PageControl1: TPageControl;
    TabAnimEditor: TTabSheet;
    TabSheet2: TTabSheet;
    SBAdd: TSpeedButton;
    SBRemove: TSpeedButton;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure TreeMapScroll(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBPlayClick(Sender: TObject);
    procedure TreeMapAfterDraw(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SBPauseClick(Sender: TObject);
    procedure TBFramesPerSecondChange(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure CBLoopClick(Sender: TObject);
    procedure SBFrameClick(Sender: TObject);

    {
    procedure TreeMapSelectShape(Sender: TTreeNodeShape);
    procedure Tree1SelectShape(Sender: TTreeNodeShape);
    }

    procedure TreeMapDeletedShapes(Sender: TObject);

    {
    procedure TreeMapDeletingShapes(Sender: TSelectedShapeList;
      var AllowDelete: Boolean);
    procedure TreeMapMovingShape(Sender: TTreeNodeShape; var DeltaX,
      DeltaY: Integer);
    procedure TreeMapResizingShape(Sender: TTreeNodeShape;
      ACorner: TTreeShapeHandle; var DeltaX, DeltaY: Integer);
    }

    procedure SBRemoveClick(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure Tree1DeletedShapes(Sender: TObject);
    procedure TeeInspector1Items2Change(Sender: TObject);
    procedure TeeInspector1Items3Change(Sender: TObject);
    procedure TeeInspector1Items1Change(Sender: TObject);
    procedure TeeInspector1Items4Change(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure PanelAnimResize(Sender: TObject);
    procedure UpHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure Delete1Click(Sender: TObject);
    procedure Insert1Click(Sender: TObject);
    procedure Duplicate1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SBCloseTreeClick(Sender: TObject);
    procedure ListAnimChange(Sender: TObject; Node: TTreeNode);
    procedure Flat1Click(Sender: TObject);
    procedure ree1Click(Sender: TObject);
    procedure ListAnimGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ListAnimGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure ListAnimDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListAnimDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListAnimMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListAnimCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListAnimEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FAnimate    : TTeeAnimate;
    FOnGallery  : TAnimateGalleryEvent;
    FOnGetAnimationName : TAnimateGetName;
    FOnModified : TNotifyEvent;
    FOnSelectedAnimation : TAnimateEditorEvent;
    FEvents     : TTeeAnimateEvents;

    Changing    : Boolean;

    //IHorizScale : Integer;  // pixels (width) equalling one frame
    //IVertScale  : Integer;

    ITranslated : Boolean;

    IOldX,
    IDragging : Integer;
    IDraggingMode : TClickedItem;

    procedure AddNewAnimation(const Animation:TTeeAnimation);
    procedure AnimateDeletedShapes(Sender: TObject);
    procedure AnimateClear(Sender: TObject);
    procedure AnimateContinue(Sender: TObject);
    procedure AnimateFrame(Sender: TObject);
    procedure AnimatePause(Sender: TObject);
    procedure AnimatePlay(Sender: TObject);
    procedure AnimateStop(Sender: TObject);
    function ClickedItem(Index,X,Y:Integer):TClickedItem;
    Procedure FillNodes;
    function NodeIndex(const Node:TTreeNode):Integer;
    procedure SelectAnimation(const Animation:TTeeAnimation);
    //Procedure SetActionBarColor;
    procedure SetAnimate(const Value: TTeeAnimate);
    procedure SetScales;
    procedure TeeModified;
    //Function TheActionBar:TTreeNodeShape;

  protected
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
  public
    { Public declarations }
    class Procedure ModalShow(const AOwner:TComponent; const TeeAnimate:TTeeAnimate;
                              const AGallery:TAnimateGalleryEvent;
                              const ASelected:TAnimateEditorEvent);
    procedure RefreshAnimate;
    procedure RestoreAnimate;
    function Selected:TTeeAnimation;

    property Animate:TTeeAnimate read FAnimate write SetAnimate;

    property OnGetAnimationName:TAnimateGetName read FOnGetAnimationName
                                                write FOnGetAnimationName;
    property OnModified:TNotifyEvent read FOnModified write FOnModified;
    property OnSelectedAnimation:TAnimateEditorEvent read FOnSelectedAnimation
                                                      write FOnSelectedAnimation;
    property OnShowGallery:TAnimateGalleryEvent read FOnGallery write FOnGallery;
  end;

implementation

uses
  TeeProcs;

{$IFNDEF LCL}
{$R *.DFM}
{$ELSE}
{$R *.lfm}
{$ENDIF}

class Procedure TTeeAnimateEditor.ModalShow(const AOwner:TComponent;
                                            const TeeAnimate:TTeeAnimate;
                                            const AGallery:TAnimateGalleryEvent;
                                            const ASelected:TAnimateEditorEvent);
begin
  with TTeeAnimateEditor.Create(AOwner) do
  try
    FAnimate:=TeeAnimate;
    FOnGallery:=AGallery;
    FOnSelectedAnimation:=ASelected;

    ShowModal;
  finally
    Free;
  end;
end;

{
type
  TNodeAnimate=class(TTreeNodeShape)
  protected
    procedure DoDrawHandle( Handle:TTreeShapeHandle;
                            var x,y:Integer; var Draw:Boolean); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Procedure MoveRelative(OfsX,OfsY:Integer; MoveChilds:Boolean); override;
  end;

procedure TNodeAnimate.DoDrawHandle( Handle:TTreeShapeHandle;
                                     var x,y:Integer; var Draw:Boolean);
begin
  with Tree.Canvas do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=clNavy;
    Pen.Style:=psSolid;
    Pen.Color:=clBlue;
    Pen.Width:=1;
    Pen.Mode:=pmNotXor;
  end;

  Tree.Selected.HandleSize:=3;
  Draw:=(Handle=rcLeft) or (Handle=rcRight);
end;
}

procedure TTeeAnimateEditor.FormCreate(Sender: TObject);
begin
  Changing:=True;

  PageControl1.ActivePage:=TabAnimEditor;

  (*
  // Default settings for map shapes
  with TreeMap.GlobalFormat do
  begin
    NodeClass:=TNodeAnimate;
    ImageIndex:=tiNone;
    Cursor:=crHandPoint;
  end;

  TreeMap.Designing:=True;

  IHorizScale:=12;  // pixels (width) equalling one frame
  IVertScale:=12;  // pixels (height)
  *)
end;

{
procedure TTeeAnimateEditor.RulerGetUnit( Sender:TTreeRuler; Pixel:Integer;
                                          var Text:String);
begin
  Text:=IntToStr(Pixel div IHorizScale);
end;
}

procedure TTeeAnimateEditor.TreeMapScroll(Sender: TObject);
begin
  // ListAnim.ScrollBy(); ...

  (*
  // Synchronize trees
  if Assigned(Tree1) and Assigned(TreeMap) then
     Tree1.View3DOptions.VertOffset:=TreeMap.View3DOptions.VertOffset;
  *)
end;

(*
{ TNodeAnimate }

const
  DefaultActionColor=clBlack;

Constructor TNodeAnimate.Create(AOwner: TComponent);
begin
  inherited;
  Color:=DefaultActionColor;
  Border.Color:=clSilver;
  ImageIndex:=tiNone;
  Cursor:=crHandPoint;
end;

procedure TNodeAnimate.MoveRelative(OfsX, OfsY: Integer;
  MoveChilds: Boolean);
begin
  OfsY:=0;

  if OfsX<0 then
     if Left < -OfsX then OfsX:=0;

  inherited;
end;
*)

type
  TTeeAnimateAccess=class(TTeeAnimate);

procedure TTeeAnimateEditor.RefreshAnimate;
begin
  if not Assigned(FAnimate) then
    Exit;

  if Assigned(TeeInspector1) then
     TeeInspector1.Enabled:=False;

  Changing:=True;

  with TTeeAnimateAccess(Animate).IEditor do
  begin
    {
    if HorizScale<>0 then
       IHorizScale:=HorizScale;

    if VertScale<>0 then
       IVertScale:=VertScale;
    }

    if Size.X<>0 then
       Self.Width:=Size.X;

    if Size.Y<>0 then
       Self.Height:=Size.Y;

    if NodeListX<>0 then
       ListAnim.Width:=NodeListX;
       //Tree1.Width:=NodeListX;
  end;

  {
  Tree1.AllowPanning:=pmNone;
  Tree1.TextEditor.Enabled:=False;

  TreeMap.AllowPanning:=pmHorizontal;
  TreeMap.TextEditor.Enabled:=False;

  with TreeMap.Page do
  begin
    Border.Visible:=False;
    UsePrinter:=False;
    Width:=IHorizScale*Animate.EndFrame;
    Height:=TreeMap.ClientHeight;
  end;
  }

  SetScales;

  FillNodes;

  CBLoop.Checked:=Animate.Loop;

  TBFramesPerSecond.Position:=Animate.Speed;
  LFramesPerSecond.Caption:=IntToStr(Animate.Speed)+' fps';

  // Replace Onxxx events...
  with TTeeAnimateAccess(Animate) do
  begin
    OnClear:=AnimateClear;
    OnContinue:=AnimateContinue;
    OnDeleteShapes:=AnimateDeletedShapes;
    OnFrame:=AnimateFrame;
    OnPause:=AnimatePause;
    OnPlay:=AnimatePlay;
    OnStop:=AnimateStop;
  end;

  SBPlay.Enabled:=Animate.State=asStopped;

  SBPause.Enabled:=Animate.State<>asStopped;
  SBPause.Down:=Animate.State=asPaused;

  SBStop.Enabled:=SBPause.Enabled;

  {
  if Tree1.Roots.Count>0 then
  begin
     Tree1.Roots[0].Selected:=True;
     TreeMap.View3DOptions.VertOffset:=0;
     TreeMapScroll(Self);
  end;
  }

  if ListAnim.Items.Count>0 then
  begin
    ListAnim.Items[0].Selected:=True;
    //ListAnimChange(Self);
  end;

  Changing:=False;
end;

procedure TTeeAnimateEditor.FormShow(Sender: TObject);
begin
  Animate:=TTeeAnimate(Tag);

  UpScale.Visible:=False;
  UpHeight.Visible:=False;

  if not ITranslated then
  begin
    TeeTranslateControl(Self);
    ITranslated:=True;
  end;

  ListAnim.Images:=Images;

  {$IFNDEF LCL}
  TabAnimEditor.UpdateControlState;
  {$ENDIF}
end;

Procedure TTeeAnimateEditor.FillNodes;

  function TreeNode(const A:TTeeAnimation):TTreeNode;
  var t : Integer;
  begin
    result:=nil;

    with ListAnim.Items do
    for t:=0 to Count-1 do
    if Item[t].Data=A then
    begin
      result:=Item[t];
      exit;
    end;
  end;

  function AddFlatItem(const A:TTeeAnimation; const AParent:TTreeNode):TTreeNode;
  var tmp : String;
  begin
    tmp:=A.EditorName;

    if Assigned(FOnGetAnimationName) then
       FOnGetAnimationName(A,tmp);

    if Assigned(AParent) then
       result:=ListAnim.Items.AddChildObject(AParent,tmp,A)
    else
       result:=ListAnim.Items.AddObject(nil,tmp,A);
  end;

  function AddItem(const A:TTeeAnimation):TTreeNode;
  var tmpNode : TTreeNode;
  begin
    result:=TreeNode(A);

    if not Assigned(result) then
    begin
      if Assigned(A.Parent) then
      begin
        tmpNode:=TreeNode(A.Parent);

        if not Assigned(tmpNode) then
           tmpNode:=AddItem(A.Parent);
      end
      else
        tmpNode:=nil;

      result:=AddFlatItem(A,tmpNode);
    end;
  end;

var t : Integer;
begin
  (*
  // Add map shapes
  TreeMap.Clear;

  with Animate do
  for t:=0 to Animations.Count-1 do
  with TreeMap.AddRootObject('',Animations[t]) do
  begin
    X0:=IHorizScale*Animations[t].StartFrame;
    Width:=IHorizScale*Animations[t].Duration;

    Y0:=1+(IVertScale*t);
    Height:=6+((IVertScale-10) div 2);

    if not Animations[t].IsEnabled then
       Color:=clDkGray;
  end;
  *)

  // Add left Tree nodes
  //Tree1.Clear;

  ListAnim.Items.Clear;

  with Animate do
  for t:=0 to Animations.Count-1 do
  if Flat1.Checked then
     AddFlatItem(Animations[t],nil)
  else
     AddItem(Animations[t]);

  {
  with Tree1.AddRootObject(Animations[t].EditorName,Animations[t]) do
  begin
    Border.Visible:=False;
    Font.Size:=8+((IVertScale-10) div 4);
    Y0:=1+(IVertScale*t);
    Transparent:=True;
    ImageIndex:=tiNone;
  end;
  }
end;

// When starting playing:
procedure TTeeAnimateEditor.AnimatePlay(Sender: TObject);
begin
  if Sender=FAnimate then
  begin
    SBPlay.Enabled:=False;
    SBPause.Enabled:=True;
    SBPause.Down:=False;
    SBStop.Enabled:=True;
  end;
end;

// When Paused, disable buttons and repaint map
procedure TTeeAnimateEditor.AnimatePause(Sender: TObject);
begin
  if Sender=FAnimate then
  begin
    SBPause.Down:=True;
    SBFrame.Enabled:=True;

    //TreeMap.Invalidate;
  end;
end;

// When animation is stopped:
procedure TTeeAnimateEditor.AnimateStop(Sender: TObject);
begin
  if Sender=FAnimate then
  begin
    if not (csDestroying in ComponentState) then
    begin
      SBPlay.Enabled:=True;
      SBPause.Enabled:=False;
      SBPause.Down:=False;
      SBStop.Enabled:=False;
      SBFrame.Enabled:=False;

      //TreeMap.Invalidate;
    end;
  end;
end;

procedure TTeeAnimateEditor.AnimateFrame(Sender: TObject);
//var P : TPoint;
begin
  if Sender=FAnimate then
  if Showing then
  begin
    (*
    P:=TreeMap.Canvas.Calculate3DPosition(IHorizScale*Animate.CurrentFrame,0,TeeTreeZ);

    // if current frame is outside Tree, scroll Tree
    if P.x>TreeMap.ChartWidth then
    begin
      with TreeMap.View3DOptions do HorizOffset:=HorizOffset-20;
    end
    else
    if P.x<0 then
      with TreeMap.View3DOptions do HorizOffset:=HorizOffset-P.x;

    // Repaint tree
    TreeMap.Invalidate;
    *)

    StatusBar1.SimpleText:=IntToStr(Animate.CurrentFrame)+'/'+IntToStr(Animate.EndFrame);
  end;
end;

procedure TTeeAnimateEditor.SBPlayClick(Sender: TObject);
//var t : Integer;
begin
  (*
  // Set actions
  for t:=0 to TreeMap.Shapes.Count-1 do
  with TTeeAnimation(TreeMap[t].Data) do
  begin
    StartFrame:=TreeMap[t].Left div IHorizScale;
    Duration:=TreeMap[t].Width div IHorizScale;
  end;
  *)

  Animate.Play;
end;

procedure TTeeAnimateEditor.TreeMapAfterDraw(Sender: TObject);
//var tmp : Integer;
begin
  (*
  // Paint animation markers (start, end and current frame)
  if Assigned(Animate) then
  with TreeMap.Canvas do
  begin
    Pen.Color:=clBlack;
    Pen.Style:=psSolid;
    Pen.Width:=1;

    if Animate.EndFrame>0 then
       VertLine3D(1+(Succ(Animate.EndFrame)*IHorizScale),0,TreeMap.Height,TeeTreeZ);

    VertLine3D(-1,0,TreeMap.Height,TeeTreeZ);

    if Animate.State=asPlaying then Pen.Color:=clBlue
    else
    if Animate.State=asPaused then
       Pen.Color:=clRed
    else
       Exit;

    // Paint CurrentFrame
    tmp:=Animate.CurrentFrame;
    if tmp=-1 then tmp:=Succ(Animate.EndFrame);

    VertLine3D(tmp*IHorizScale,0,TreeMap.Height,TeeTreeZ);
  end;
  *)
end;

procedure TTeeAnimateEditor.RestoreAnimate;
begin
  if Assigned(Animate) then
  with TTeeAnimateAccess(Animate) do
  begin
    //Stop;

    RestoreEvents(FEvents);

    with IEditor do
    begin
      //VertScale:=IVertScale;
      //HorizScale:=IHorizScale;

      Size.X:=Self.Width;
      Size.Y:=Self.Height;
      NodeListX:=ListAnim.Width; // Tree1.Width;
    end;
  end;
end;

// Stop the animation and restore old events:
procedure TTeeAnimateEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  RestoreAnimate;
end;

// Pause or Continue playing:
procedure TTeeAnimateEditor.SBPauseClick(Sender: TObject);
begin
  if SBPause.Down then Animate.Pause
                  else Animate.Continue;
end;

// Change speed:
procedure TTeeAnimateEditor.TBFramesPerSecondChange(Sender: TObject);
begin
  if not Changing then
  begin
    Animate.Speed:=TBFramesPerSecond.Position;
    LFramesPerSecond.Caption:=IntToStr(Animate.Speed)+' fps';
    TeeModified;
  end;
end;

// Stop animation:
procedure TTeeAnimateEditor.SBStopClick(Sender: TObject);
begin
  Animate.Stop;
end;

// Set Loop:
procedure TTeeAnimateEditor.CBLoopClick(Sender: TObject);
begin
  if not Changing then
  begin
    Animate.Loop:=CBLoop.Checked;
    TeeModified;
  end;
end;

// Next frame:
procedure TTeeAnimateEditor.SBFrameClick(Sender: TObject);
begin
  Animate.NextFrame;
end;

{
// Select node in Tree when a bar is selected:
procedure TTeeAnimateEditor.TreeMapSelectShape(Sender: TTreeNodeShape);
begin
  //Tree1.Selected.Clear;
  //Tree1.Roots[TTeeAnimation(Sender.Data).Index].Selected:=True;
end;

// Select a bar in map when a node in Tree is selected:
procedure TTeeAnimateEditor.Tree1SelectShape(Sender: TTreeNodeShape);
begin
  SelectAnimation(TTeeAnimation(Sender.Data));
end;
}

procedure TTeeAnimateEditor.SelectAnimation(const Animation:TTeeAnimation);

  procedure CheckEditor;

    procedure RemoveEditor;
    begin
      RemoveControls(TabAnimEditor);
    end;

    (*
    function AnimInspector:TObjectInspector;
    begin
      result:=nil;

      with IAnimEditor.TabAnimEditor do
      if ControlCount>0 then
         if Controls[0] is TObjectInspector then
            result:=TObjectInspector(Controls[0]);
    end;

    procedure InspectAnimation;

      function CreateInspector:TObjectInspector;
      begin
        result:=TObjectInspector.Create(Sender as TComponent);

        with result do
        begin
          Align:=alClient;
          Header.Visible:=False;
          Parent:=(Sender as TTeeAnimateEditor).TabAnimEditor;

          OnFilter:=FilterInspector;
          OnChange:=InspectorChanged;

          Options:=result.Options+[goColSizing];
          ColWidths[0]:=100;
        end;
      end;

    var tmp : TObjectInspector;
    begin
      tmp:=AnimInspector;

      if not Assigned(tmp) then
      begin
        RemoveEditor;
        tmp:=CreateInspector;
      end;

      tmp.Inspect(Animation);

      tmp.Invalidate;
    end;
    *)

    procedure AddEditor;

      function CurrentEditor:TForm;
      begin
        with TabAnimEditor do
        if (ControlCount=0) or (not (Controls[0] is TForm)) then
           result:=nil
        else
           result:=Controls[0] as TForm;
      end;

    var tmpFormClass : TFormClass;
        tmpForm      : TForm;
    begin
      tmpFormClass:=TFormClass(TeeGetClass(Animation.EditorClass));

      if Assigned(tmpFormClass) then
      begin
        //if AnimInspector<>nil then
        //   AnimInspector.Hide;

        if (CurrentEditor=nil) or (tmpFormClass<>CurrentEditor.ClassType) then
        begin
          RemoveEditor;

          tmpForm:=tmpFormClass.Create(Self);
          tmpForm.Align:=alClient;
          TeeTranslateControl(tmpForm);
          TTeeVCL.AddFormTo(tmpForm,TabAnimEditor,Animation);
        end
        else
        begin
          CurrentEditor.Tag:=ObjectToTag(Animation);
          TForm(CurrentEditor).OnShow(Self);
        end;
      end
      //else
       // InspectAnimation;
    end;

  begin
    if Assigned(Animation) then
       AddEditor
    else
       RemoveEditor;
  end;

  Procedure DoSelection;
  var tmp : Boolean;
      tmpAnimation : TTeeAnimation;
  begin
    {
    TreeMap.OnSelectShape:=nil;
    TreeMap.Selected.Clear;
    }

    tmp:=Assigned(Animation);

    {
    if tmp then
       TreeMap.Roots[Animation.Index].Selected:=True;

    TreeMap.OnSelectShape:=TreeMapSelectShape;
    }

    TeeInspector1.Enabled:=tmp;
    SBRemove.Enabled:=tmp;

    if tmp then
    with Animation do
    begin
      TeeInspector1.Items[0].Value:=Enabled;
      TeeInspector1.Items[1].Value:=StartFrame;
      TeeInspector1.Items[2].Value:=Duration;
      TeeInspector1.Items[3].Value:=Loop;

      if Title='' then
         StatusBar1.SimpleText:=Name
      else
         StatusBar1.SimpleText:=Title;
    end
    else
    begin
      RemoveControls(PanelEditor);
      StatusBar1.SimpleText:='';
    end;

    CheckEditor;

    if Assigned(FOnSelectedAnimation) then
    begin
      tmpAnimation:=Animation;
      FOnSelectedAnimation(Self,tmpAnimation);
    end;
  end;

begin
  Changing:=True;
  try
    DoSelection;
  finally
    Changing:=False;
  end;
end;

procedure TTeeAnimateEditor.AnimateDeletedShapes(Sender: TObject);
begin
  //ListAnimChange(Self);

  {
  if Assigned(Tree1) then
     Tree1SelectShape(Tree1.Selected.First);
  }
end;

procedure TTeeAnimateEditor.AnimateClear(Sender: TObject);
begin
  FillNodes;
end;

procedure TTeeAnimateEditor.TreeMapDeletedShapes(Sender: TObject);
begin
  SBRemoveClick(Sender);
end;

{
procedure TTeeAnimateEditor.TreeMapDeletingShapes(
  Sender: TSelectedShapeList; var AllowDelete: Boolean);
begin
  TTeeAnimation(Sender.First.Data).Free;
  Tree1.Roots[TreeMap.Roots.IndexOf(Sender.First)].Free;
end;

Function TTeeAnimateEditor.TheActionBar:TTreeNodeShape;
begin
  result:=TreeMap.Roots[Tree1.Roots.IndexOf(Tree1.Selected.First)];
end;
}

Function TTeeAnimateEditor.Selected:TTeeAnimation;
begin
  with ListAnim do
  if Selected=nil then
     result:=nil
  else
     result:=TTeeAnimation(Selected.Data);

  {
  if Tree1.Selected.First<>nil then
     result:=TTeeAnimation(Tree1.Selected.First.Data)
  else
     result:=nil;
  }
end;

{
procedure TTeeAnimateEditor.TreeMapMovingShape(Sender: TTreeNodeShape;
  var DeltaX, DeltaY: Integer);
var tmp : Integer;
begin
  DeltaY:=0; // prevent moving vertically

  tmp:=(DeltaX div IHorizScale);

  with TTeeAnimation(Sender.Data) do
  begin
    if (StartFrame+tmp)>0 then StartFrame:=StartFrame+tmp
                          else StartFrame:=0;

    Sender.X0:=StartFrame*IHorizScale;
    DeltaX:=0;

    TeeInspector1.Items[1].Value:=StartFrame;
  end;
end;

procedure TTeeAnimateEditor.TreeMapResizingShape(Sender: TTreeNodeShape;
  ACorner: TTreeShapeHandle; var DeltaX, DeltaY: Integer);
var tmp : Integer;
begin
  DeltaY:=0; // prevent resizing vertically

  tmp:=(DeltaX div IHorizScale);

  with TTeeAnimation(Sender.Data) do
  begin
    if ACorner=rcLeft then
    begin
      if (Duration-tmp)>0 then
      begin
        Duration:=Duration-tmp;
        StartFrame:=StartFrame+tmp;
        Sender.Left:=StartFrame*IHorizScale;
        TeeInspector1.Items[1].Value:=StartFrame;
      end;
    end
    else
    begin
      if (Duration+tmp)>1 then Duration:=Duration+tmp
                          else Duration:=1;
    end;

    Sender.Width:=Duration*IHorizScale;
    TeeInspector1.Items[2].Value:=Duration;
    DeltaX:=0;
  end;
end;
}

procedure TTeeAnimateEditor.SBRemoveClick(Sender: TObject);
begin
  Selected.Free;
  FillNodes;

  SBRemove.Enabled:=Animate.Animations.Count>0;

  if not SBRemove.Enabled then
     SelectAnimation(nil);

  //ListAnimChange(Self);
  //Tree1SelectShape(Tree1.Selected.First);

  TeeModified;
end;

type
  TAnimationAccess=class(TTeeAnimation);

procedure TTeeAnimateEditor.AddNewAnimation(const Animation:TTeeAnimation);
var tmp : String;
begin
  if Animation.Name='' then 
  begin
    tmp:=Animation.ClassName;
    if Copy(tmp,1,1)='T' then
       Delete(tmp,1,1);

    Animation.Name:=TeeGetUniqueName(Animation.Owner,tmp);
  end;

  Animate.Animations.Add(Animation);

  if ListAnim.Selected<>nil then
     if TAnimationAccess(Selected).IsFolder then
        Animation.Parent:=Selected
     else
        Animation.Parent:=Selected.Parent;

  FillNodes;

  ListAnim.Items[ListAnim.Items.Count-1].Selected:=True;
  //ListAnimClick(Self);
  
  //TreeMap.Roots.Last.Selected:=True;

  TeeModified;
end;

procedure TTeeAnimateEditor.SBAddClick(Sender: TObject);
var tmp : TTeeAnimation;
begin
  if Assigned(FOnGallery) then
  begin
    tmp:=nil;
    FOnGallery(Self,tmp,Selected);

    if Assigned(tmp) then
       AddNewAnimation(tmp);
  end;
end;

procedure TTeeAnimateEditor.TeeModified;
begin
  if Assigned(FOnModified) then
     FOnModified(Self);
end;

procedure TTeeAnimateEditor.AnimateContinue(Sender: TObject);
begin
  SBPause.Down:=False;
  SBFrame.Enabled:=False;
end;

procedure TTeeAnimateEditor.Tree1DeletedShapes(Sender: TObject);
begin
  SBRemoveClick(Sender);
end;

procedure TTeeAnimateEditor.Notification( AComponent: TComponent;
                            Operation: TOperation);
begin
  inherited;

  if (AComponent=FAnimate) and (Operation=opRemove) then 
     Animate:=nil;
end;

procedure TTeeAnimateEditor.SetAnimate(const Value: TTeeAnimate);
begin
  if FAnimate<>Value then
  begin
    if Assigned(FAnimate) then
    begin
      TTeeAnimateAccess(Animate).RestoreEvents(FEvents);

      FAnimate.RemoveFreeNotification(Self);
    end;

    FAnimate:=Value;

    if Assigned(FAnimate) then
    begin
      FAnimate.FreeNotification(Self);
      TTeeAnimateAccess(FAnimate).SaveEvents(FEvents);
    end;

    RefreshAnimate;
  end;
end;

procedure TTeeAnimateEditor.TeeInspector1Items2Change(Sender: TObject);
begin
  Selected.StartFrame:=TInspectorItem(Sender).Value;
  TeeModified;
end;

procedure TTeeAnimateEditor.TeeInspector1Items3Change(Sender: TObject);
var tmp : Integer;
begin
  tmp:=TInspectorItem(Sender).Value;

  if Selected.Duration<>tmp then
  begin
    Selected.Duration:=tmp;
    TeeModified;
  end;
end;

procedure TTeeAnimateEditor.TeeInspector1Items1Change(Sender: TObject);
begin
  if Selected.Enabled<>TInspectorItem(Sender).Value then
  begin
    Selected.Enabled:=TInspectorItem(Sender).Value;
    TeeModified;
  end;
end;

procedure TTeeAnimateEditor.TeeInspector1Items4Change(Sender: TObject);
begin
  if Selected.Loop<>TInspectorItem(Sender).Value then
  begin
    Selected.Loop:=TInspectorItem(Sender).Value;
    TeeModified;
  end;
end;

{
Procedure TTeeAnimateEditor.SetActionBarColor;
begin
  if TheAction.IsEnabled then
     if TheAction.Loop then TheActionBar.Color:=clBlue
                       else TheActionBar.Color:=DefaultActionColor
  else
     TheActionBar.Color:=clDkGray;
end;
}

procedure TTeeAnimateEditor.SBCloseClick(Sender: TObject);
begin
  PanelAnim.Hide;
  Splitter2.Hide;
end;

procedure TTeeAnimateEditor.PanelAnimResize(Sender: TObject);
begin
  SBClose.Left:=PanelAnim.Width-20;
end;

procedure TTeeAnimateEditor.SetScales;
begin
  {
  TreeMap.Grid.HorizStep:=IHorizScale;
  TreeMap.Grid.VertStep:=IVertScale;

  with (Tree1.GlobalFormat.ChildManager as TTreeExplorerAlignChild) do
  begin
    VertMargin:=-3+((IVertScale-10) div 4);
    TopPos:=0;
  end;
  }
end;

procedure TTeeAnimateEditor.UpHeightClick(Sender: TObject;
  Button: TUDBtnType);
begin
  {
  IVertScale:=UpHeight.Position;

  SetScales;
  FillNodes;
  }
end;

procedure TTeeAnimateEditor.Delete1Click(Sender: TObject);
begin
  SBRemoveClick(Sender);
end;

procedure TTeeAnimateEditor.Insert1Click(Sender: TObject);
begin
  SBAddClick(Sender);
end;

procedure TTeeAnimateEditor.Duplicate1Click(Sender: TObject);
var tmp : TTeeAnimation;
begin
  tmp:=TTeeAnimationClass(Selected.ClassType).Create(Selected.Owner);
  tmp.Assign(Selected);
  AddNewAnimation(tmp);
end;

procedure TTeeAnimateEditor.PopupMenu1Popup(Sender: TObject);
begin
  Delete1.Enabled:=SBRemove.Enabled;
  Duplicate1.Enabled:=Delete1.Enabled;
end;

procedure TTeeAnimateEditor.SBCloseTreeClick(Sender: TObject);
begin
  Visible:=False;
end;

procedure TTeeAnimateEditor.ListAnimChange(Sender: TObject;
  Node: TTreeNode);
begin
  SelectAnimation(Selected);
end;

procedure TTeeAnimateEditor.Flat1Click(Sender: TObject);
begin
  Flat1.Checked:=True;
  FillNodes;
end;

procedure TTeeAnimateEditor.ree1Click(Sender: TObject);
begin
  ree1.Checked:=True;
  FillNodes;
end;

function TTeeAnimateEditor.NodeIndex(const Node:TTreeNode):Integer;
begin
  if TAnimationAccess(Node.Data).IsFolder then
     if Node.Expanded then
        result:=1
     else
        result:=0
  else
  if TTeeAnimation(Node.Data).Enabled then
     result:=2
  else
     result:=3;
end;

procedure TTeeAnimateEditor.ListAnimGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=NodeIndex(Node);
end;

procedure TTeeAnimateEditor.ListAnimGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex:=NodeIndex(Node);
end;

procedure TTeeAnimateEditor.ListAnimDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tmp : TTreeNode;
begin
  tmp:=ListAnim.GetNodeAt(X,Y);

  Accept:=(Sender=ListAnim) and
          (
             (not Assigned(tmp)) or
             TAnimationAccess(tmp.Data).IsFolder
          );
end;

procedure TTeeAnimateEditor.ListAnimDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp : TTreeNode;
begin
  if Source=ListAnim then
  begin
    tmp:=ListAnim.GetNodeAt(X,Y);

    if Assigned(tmp) then
       Selected.Parent:=TTeeAnimation(tmp.Data)
    else
       Selected.Parent:=nil;

    ListAnim.Selected.MoveTo(tmp,naAddChild);
  end;
end;

procedure TTeeAnimateEditor.ListAnimMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ListAnim.GetNodeAt(X,Y)=nil then
     ListAnim.Selected:=nil;
end;

procedure TTeeAnimateEditor.ListAnimCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not TTeeAnimation(Node.Data).Enabled then
     Sender.Canvas.Font.Color:=clInactiveCaptionText;

  PaintBox1.Invalidate;
end;

procedure TTeeAnimateEditor.ListAnimEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
begin
  TTeeAnimation(Node.Data).Title:=S;
end;

const
  TopPos=8;

procedure TTeeAnimateEditor.PaintBox1Paint(Sender: TObject);
var t : Integer;
    Scale : Double;
    R     : TRect;
    tmp   : Integer;
begin
  tmp:=Animate.EndFrame;
  if tmp=0 then Exit;

  Scale:=(PaintBox1.Width-4)/Animate.EndFrame;

  with ListAnim.Items do
  for t:=0 to Count-1 do
  with Item[t] do
  begin
    R:=DisplayRect(True);

    if R.Bottom>R.Top then
    begin
      if TTeeAnimation(Data).Enabled then
         if ListAnim.Selected=ListAnim.Items[t] then
            PaintBox1.Canvas.Brush.Color:=clHighlight
         else
            PaintBox1.Canvas.Brush.Color:=clDkGray
      else
         PaintBox1.Canvas.Brush.Color:=clSilver;

      with PaintBox1.Canvas.Pen do
      begin
        Color:=clBlack;
        Width:=1;
      end;

      with TTeeAnimation(Data) do
           PaintBox1.Canvas.Rectangle(2+Round(StartFrame*Scale),TopPos+R.Top+2,
                                      Round(EndFrame*Scale),TopPos+R.Bottom-2);

      with PaintBox1.Canvas do
      begin
        Pen.Color:=clSilver;
        MoveTo(0,TopPos+R.Bottom);
        LineTo(PaintBox1.Width,TopPos+R.Bottom);
      end;
    end;
  end;
end;

function TTeeAnimateEditor.ClickedItem(Index,X,Y:Integer):TClickedItem;
var R,tmpR : TRect;
    tmp : Integer;
    Scale : Single;
begin
  result:=ciNone;

  tmp:=Animate.EndFrame;
  if tmp=0 then Exit;

  Scale:=(PaintBox1.Width-4)/Animate.EndFrame;

  with ListAnim.Items.Item[Index] do
  begin
    R:=DisplayRect(True);

    if R.Bottom>R.Top then
      with TTeeAnimation(Data) do
      begin
        tmpR:=TeeRect(2+Round(StartFrame*Scale),TopPos+R.Top+2,
                        Round(EndFrame*Scale),TopPos+R.Bottom-2);

        if Abs(tmpR.Left-X)<5 then
           result:=ciLeftSide
        else
        if Abs(tmpR.Right-X)<5 then
           result:=ciRightSide
        else
        if PointInRect(tmpR,X,Y) then
           result:=ciBody
        else
           result:=ciNone;

        if result<>ciNone then
           Exit;
      end;
   end;
end;

procedure TTeeAnimateEditor.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var t : Integer;
begin
  if IDragging=-1 then
  begin
    for t:=0 to ListAnim.Items.Count-1 do
        if ClickedItem(t,X,Y)<>ciNone then
        begin
          PaintBox1.Cursor:=crHandPoint;
          Exit;
        end;

    PaintBox1.Cursor:=crDefault;
  end
  else
  begin
    if IDraggingMode=ciBody then
    begin
      //with TTeeAnimation(ListAnim.Items[IDragging].Data) do
      //     StartFrame:=StartFrame+(IOldX-X);

      PaintBox1.Invalidate;
    end;
  end;
end;

procedure TTeeAnimateEditor.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t : Integer;
begin
  IDragging:=-1;

  for t:=0 to ListAnim.Items.Count-1 do
      if ClickedItem(t,X,Y)<>ciNone then
      begin
        ListAnim.Selected:=ListAnim.Items[t];
        Exit;
      end;
end;

procedure TTeeAnimateEditor.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t : Integer;
begin
  IDragging:=-1;
  IDraggingMode:=ciNone;

  for t:=0 to ListAnim.Items.Count-1 do
  begin
    IDraggingMode:=ClickedItem(t,X,Y);

    if IDraggingMode<>ciNone then
    begin
      IDragging:=t;
      IOldX:=X;
      Exit;
    end;
  end;
end;

initialization
  RegisterClass(TTeeAnimateEditor);
end.
