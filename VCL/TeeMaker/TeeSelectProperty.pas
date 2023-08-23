unit TeeSelectProperty;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  Classes, TypInfo,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QComCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  SysUtils;

type
  TPropertySelector = class(TForm)
    GroupBox1: TGroupBox;
    TreeObjects: TTreeView;
    PanelButtons: TPanel;
    Splitter1: TSplitter;
    Panel1: TPanel;
    BOK: TButton;
    BCancel: TButton;
    PageControl1: TPageControl;
    TabProps: TTabSheet;
    TreeProps: TTreeView;
    TabEvents: TTabSheet;
    TreeEvents: TTreeView;
    LabelClass: TLabel;
    procedure TreeObjectsChange(Sender: TObject; Node: TTreeNode);
    procedure TreePropsChange(Sender: TObject; Node: TTreeNode);
    procedure TreePropsDblClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    function IsInstance(const AObject:TObject):Boolean;
  protected
    procedure AddProperties(ATree:TTreeView; AObject:TObject; AList:TList;
                            AFilter:TTypeKinds); virtual;
  public
    { Public declarations }
    function SelectedProperty(var AName:String):TObject;
    class function NodeWithObject(Items:TTreeNodes; AObject:TObject):TTreeNode;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

class function TPropertySelector.NodeWithObject(Items:TTreeNodes; AObject:TObject):TTreeNode;
var tmp : TTreeNode;
begin
  tmp:=Items.GetFirstNode;

  while Assigned(tmp) do
  begin
    if tmp.Data=AObject then
    begin
      result:=tmp;
      Exit;
    end;

    tmp:=tmp.GetNext;
  end;

  result:=nil;
end;

function TPropertySelector.IsInstance(const AObject:TObject):Boolean;
begin
  result:=NodeWithObject(TreeObjects.Items,AObject)<>nil;
end;

function TPropertySelector.SelectedProperty(var AName:String):TObject;

  function IsValidInstance(AObject:TObject):Boolean;
  begin
    result:=(AObject is TComponent) and IsInstance(AObject);
  end;

var tmpNode : TTreeNode;
begin
  tmpNode:=TreeProps.Selected;

  if Assigned(tmpNode) then
  begin
    result:=TObject(tmpNode.Data);
    AName:=TreeProps.Selected.Text;

    while not IsValidInstance(result) do
    begin
      tmpNode:=tmpNode.Parent;

      if Assigned(tmpNode) then
         result:=TObject(tmpNode.Data)
      else
      begin
        result:=TreeObjects.Selected.Data;
        break;
      end;

      if not IsValidInstance(result) then
         AName:=tmpNode.Text+'.'+AName;
    end;
  end
  else
  begin
    AName:='';
    result:=TreeObjects.Selected.Data;
  end;
end;

procedure TeeFillProperties(S:TObject; ParentNode:TTreeNode; oList:TList; AOwner:TTreeNodes;
                            AFilter:TTypeKinds);
var n,
    tmp,
    t  : Integer;
    L  : PPropList;
    Accept : Boolean;
    tmpNode: TTreeNode;
    o : TObject;
begin
  if S.ClassInfo=nil then
     Exit;

  L:=nil;

  tmp:=GetTypeData(S.ClassInfo)^.PropCount;
  if tmp>0 then
  begin
    GetMem(L, tmp*SizeOf(Pointer));

    n:=GetPropList(S.ClassInfo,AFilter,L);
  end
  else
    n:=0;

  if n>0 then
  begin
    {$IFDEF D6}
    SortPropList(L,n);
    {$ENDIF}
    
    for t:=0 to n-1 do
    begin
      Accept:=True;

      //if Assigned(Filter) then
      //   Filter(L[t].PropType^^,Accept);

      if Accept then
      begin
        if L[t].PropType^.Kind=tkClass then
        begin
          o:=GetObjectProp(S,L[t]);

          if Assigned(o) and (o.ClassName<>'TSubGradient') then
          begin
            if oList.IndexOf(o)=-1 then
            begin
              tmpNode:=AOwner.AddChildObject(ParentNode,String(L[t].Name),o);
              TeeFillProperties(o,tmpNode,oList,AOwner,AFilter);
              oList.Add(o);
            end;
          end;
        end
        else
          AOwner.AddChildObject(ParentNode,String(L[t].Name),S);

      end;
    end;

    FreeMemory(L);
  end
  else
  if S is TCollection then
  begin
    with TCollection(S) do
    for t:=0 to Count-1 do
    begin
      tmpNode:=AOwner.AddChildObject(ParentNode,'Item'+IntToStr(t),Items[t]);
      TeeFillProperties(Items[t],tmpNode,oList,AOwner,AFilter);
      oList.Add(Items[t]);
    end;
  end;
end;

procedure TPropertySelector.AddProperties(ATree:TTreeView; AObject:TObject; AList:TList;
                                          AFilter:TTypeKinds);
begin
  TeeFillProperties(AObject,nil,AList,ATree.Items,AFilter);
end;

procedure TPropertySelector.TreeObjectsChange(Sender: TObject;
  Node: TTreeNode);

var
  TheObject : TObject;

  procedure DoFillProperties(ATree:TTreeView; const AFilter:TTypeKinds);
  var oList : TList;
  begin
    ATree.Enabled:=Assigned(TheObject);
    ATree.Items.Clear;

    if ATree.Enabled then
    begin
      ATree.Items.BeginUpdate;
      try
        oList:=TList.Create;
        try
          AddProperties(ATree,TheObject,oList,AFilter);
        finally
          oList.Free;
        end;
      finally
        ATree.Items.EndUpdate;
      end;
    end;
  end;

begin
  if Assigned(TreeObjects.Selected) and Assigned(TreeObjects.Selected.Data) then
     TheObject:=TObject(TreeObjects.Selected.Data)
  else
     TheObject:=nil;

  if Assigned(TheObject) then
     LabelClass.Caption:=TheObject.ClassName
  else
     LabelClass.Caption:='';

  DoFillProperties(TreeProps,tkProperties);
  DoFillProperties(TreeEvents,tkMethods);
end;

procedure TPropertySelector.TreePropsChange(Sender: TObject;
  Node: TTreeNode);
begin
  BOK.Enabled:=(
     (PageControl1.ActivePage=TabProps) and  Assigned(TreeProps.Selected)
       ) or
     (
       (PageControl1.ActivePage=TabEvents) and  Assigned(TreeEvents.Selected)
     );
end;

procedure TPropertySelector.TreePropsDblClick(Sender: TObject);
begin
  if BOk.Enabled then
     ModalResult:=mrOk;
end;

procedure TPropertySelector.PageControl1Change(Sender: TObject);
begin
  TreePropsChange(Self,nil);
end;

end.
