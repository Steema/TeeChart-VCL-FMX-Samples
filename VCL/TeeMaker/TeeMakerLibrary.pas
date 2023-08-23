{********************************************}
{ TeeMaker Library Dialog                    }
{ Copyright (c) 2007-2023 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeMakerLibrary {$IFDEF D19}platform{$ENDIF};
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes,

  {$IFNDEF D6}
  FileCtrl,
  {$ENDIF}

  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QComCtrls, QStdCtrls, QMenus,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus,
  {$ENDIF}
  TeeBlocks;

type
  TAddNodeProc=procedure(ATree:TTreeView; AParent:TTreeNode;
                         const APath,AName:String) of object;

  TMakerLibrary = class(TForm)
    GroupObjects: TGroupBox;
    TreeObjects: TTreeView;
    SplitterLibrary: TSplitter;
    GroupTextures: TGroupBox;
    TreeTextures: TTreeView;
    PopupTextures: TPopupMenu;
    CreateFolder1: TMenuItem;
    Rename2: TMenuItem;
    Delete4: TMenuItem;
    PopupObjects: TPopupMenu;
    Addtoscene1: TMenuItem;
    Open3: TMenuItem;
    Delete3: TMenuItem;
    N3: TMenuItem;
    AddFolder1: TMenuItem;
    Rename1: TMenuItem;
    OpeninExplorer1: TMenuItem;
    OpeninExplorer2: TMenuItem;
    Refresh1: TMenuItem;
    N1: TMenuItem;
    Linktofolder1: TMenuItem;
    CreateLinkto1: TMenuItem;
    Preview1: TMenuItem;
    N2: TMenuItem;
    Refresh2: TMenuItem;
    N4: TMenuItem;
    procedure TreeObjectsGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeObjectsGetSelectedIndex(Sender: TObject;
      Node: TTreeNode);
    procedure TreeTexturesGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeTexturesGetSelectedIndex(Sender: TObject;
      Node: TTreeNode);
    procedure Open3Click(Sender: TObject);
    procedure TreeObjectsDblClick(Sender: TObject);
    procedure AddFolder1Click(Sender: TObject);
    procedure Delete3Click(Sender: TObject);
    procedure PopupObjectsPopup(Sender: TObject);
    procedure TreeObjectsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeObjectsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Rename1Click(Sender: TObject);
    procedure CreateFolder1Click(Sender: TObject);
    procedure Rename2Click(Sender: TObject);
    procedure Delete4Click(Sender: TObject);
    procedure PopupTexturesPopup(Sender: TObject);
    procedure TreeTexturesEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure TreeObjectsEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure TreeTexturesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeTexturesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OpeninExplorer1Click(Sender: TObject);
    procedure OpeninExplorer2Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure Linktofolder1Click(Sender: TObject);
    procedure CreateLinkto1Click(Sender: TObject);
    procedure TreeTexturesDblClick(Sender: TObject);
    procedure TreeTexturesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure Preview1Click(Sender: TObject);
    procedure TreeTexturesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeObjectsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Refresh2Click(Sender: TObject);
  private
    { Private declarations }

    FOnOpenObject : TNotifyEvent;

    procedure AddGraphicFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);
    procedure AddMakerFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);
    procedure DoDeleteTree(ATree:TTreeView);
    procedure DropTreeNode(ATree:TTreeView; ANode:TTreeNode; const AExt:String);
    procedure FinishCreateFolder(ATree:TTreeView; ANode:TTreeNode);
    procedure FinishNodeEdited(Node: TTreeNode; const AExt:String; var S:String);
    procedure TryLinkFolder(ATree:TTreeView);
  public
    { Public declarations }

    Current     : TCustomBlock;
    LibraryPath : String;

    class procedure CheckDummyNode(Sender: TObject; Node: TTreeNode;
                             AProc:TAddNodeProc;
                             const APath:String; var AllowExpansion: Boolean);

    function DropExternalFile(ATree:TTreeView; const AFile:String):Boolean;
    class procedure FillFolders(ATree:TTreeView; AParent:TTreeNode; const APath:String;
                                AddNode:TAddNodeProc);
    class procedure FillURL(ATree:TTreeView; AParent:TTreeNode;
                      const APath:String; AddNode:TAddNodeProc);
    function IsBasicFolder(ANode:TTreeNode):Boolean;
    function IsBasicNode(ANode:TTreeNode):Boolean;
    function IsDragOkFrom(Source:TObject; ATree:TTreeView):Boolean;
    procedure FillTextures;

    function LinkFile(ATree:TTreeView; ANode:TTreeNode; const AExtension:String):String; overload;
    function LinkFile(const APath,AFile,AExtension:String):String; overload;

    class function NodeIsFile(ANode:TTreeNode):Boolean;{$IFDEF D9}static;{$ENDIF}
    class function NodeIsLink(ANode:TTreeNode):Boolean;
    class function NodePath(APath:String; ANode:TTreeNode):String; overload;
    function NodePath(ATree:TTreeView; ANode:TTreeNode):String; overload;
    procedure ReFillTrees;
    function SelectedIsMaker:Boolean;
    function SelectedIsObj:Boolean;
    function SelectedLinkFile:String;
    function SelectedTexture:String;
    procedure SelectTextureFile(const FileName:String);
    class procedure TryAddLinkFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);
    procedure TryFillTrees(AddBasicBlocks:Boolean=True);

    property OnOpenObject:TNotifyEvent read FOnOpenObject write FOnOpenObject;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  ShellAPI, TeeURL, TeePenDlg, TeeProcs, TeeTextureSelector,
  TeeObjFormat, Tee3DSFormat, TeeMakerConst;

{ TMakerLibrary }

const
  TeeLinkExtension='.hlink';
  NodeFolderData =Pointer(47547854784);
  NodeLinkData   =Pointer(47547854785);
  NodeDummyData  =Pointer(47547854786);

class function TMakerLibrary.NodeIsFile(ANode:TTreeNode):Boolean;
begin
  result:=(ANode.Data<>NodeFolderData) and (ANode.Data<>NodeLinkData);
end;

procedure AddDummy(ANode:TTreeNode);
begin
  TTreeView(ANode.TreeView).Items.AddChildObject(ANode,
      '(dummy)',NodeDummyData);
end;

class procedure TMakerLibrary.FillURL(ATree:TTreeView; AParent:TTreeNode;
                                  const APath:String; AddNode:TAddNodeProc);
var m: TStringStream;
    tmp,
    tmpExt,
    s: String;
    i: Integer;
begin
  Screen.Cursor:=crHourGlass;
  try
    m:=TStringStream.Create('');
    try
      DownloadURL(TeeMsg_WebLibraryURL+APath+'?F=0',m);
      s:=m.DataString;
    finally
      m.Free;
    end;

    i:=Pos('</li>',s);
    if i>0 then
    begin
      Delete(s,1,i+5);

      repeat
        i:=Pos('<li><a ',s);

        if i>0 then
        begin
          Delete(s,1,i+6);

          i:=Pos('">',s);

          if i>0 then
          begin
            Delete(s,1,i+1);

            i:=Pos('</a></li>',s);

            if i>0 then
            begin
              tmp:=Trim(Copy(s,1,i-1));

              if Copy(tmp,Length(tmp),1)='/' then
                 Delete(tmp,Length(tmp),1);

              tmpExt:=ExtractFileExt(tmp);

              if tmpExt='' then
                 AddDummy(ATree.Items.AddChildObject(AParent,tmp,NodeFolderData))
              else
                 AddNode(ATree,AParent,APath,tmp);

              Delete(s,1,i+5);
            end;
          end;
        end;

      until i=0;

    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

class procedure TMakerLibrary.FillFolders(ATree:TTreeView; AParent:TTreeNode;
                                  const APath:String; AddNode:TAddNodeProc);
var f : TSearchRec;
begin
  if FindFirst(APath+'\*.*',faAnyFile,f)=0 then
  begin
    Repeat
      if (f.Attr and faDirectory)=faDirectory then
      begin
        if (f.Name<>'.') and (f.Name<>'..') then
           AddDummy(ATree.Items.AddChildObject(AParent,f.Name,NodeFolderData));
      end
      else
        AddNode(ATree,AParent,APath,f.Name);

    Until FindNext(f)<>0;

    FindClose(f);
  end;
end;

procedure TMakerLibrary.ReFillTrees;
begin
  TreeObjects.Items.Clear;
  TreeTextures.Items.Clear;

  TryFillTrees;
end;

function NodeLinkPath(const AFileName:String):String;
var f : TextFile;
begin
  AssignFile(f,AFileName);
  Reset(f);
  try
    Readln(f,result);
  finally
    CloseFile(f);
  end;
end;

class procedure TMakerLibrary.TryAddLinkFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);
var tmpNode : TTreeNode;
begin
  if UpperCase(ExtractFileExt(AName))=UpperCase(TeeLinkExtension) then
  begin
    tmpNode:=ATree.Items.AddChildObject(AParent,RemoveFileExtension(AName),NodeLinkData);

    AddDummy(tmpNode);
  end;
end;

procedure TMakerLibrary.AddMakerFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);

  function TryAddExtension(const AExt:String; AKey:Integer):Boolean;
  begin
    result:=UpperCase(ExtractFileExt(AName))=UpperCase(AExt);

    if result then
       ATree.Items.AddChildObject(AParent,RemoveFileExtension(AName),Pointer(AKey));
  end;

begin
  if not TryAddExtension(TeeMakerExtension,1) then
     if not TryAddExtension(TeeObjExtension,2) then
        if not TryAddExtension(Tee3DSExtension,3) then
           TryAddLinkFile(ATree,AParent,APath,AName);
end;

procedure TMakerLibrary.AddGraphicFile(ATree:TTreeView; AParent:TTreeNode; const APath,AName:String);
begin
  if TBlockPicture.FileGraphicClass(AName)<>nil then
     ATree.Items.AddChildObject(AParent,AName,Pointer(1))
  else
     TryAddLinkFile(ATree,AParent,APath,AName);
end;

procedure TMakerLibrary.TryFillTrees(AddBasicBlocks:Boolean=True);

  procedure AddBasic(AParent:TTreeNode);
  var t : Integer;
      tmpList : TStringList;
  begin
    tmpList:=BlockClasses.Sorted;
    try
      for t:=0 to tmpList.Count-1 do
          TreeObjects.Items.AddChildObject(AParent,tmpList[t],tmpList.Objects[t]);
    finally
      tmpList.Free;
    end;
  end;

begin
  if TreeObjects.Items.Count=0 then
  begin
    if AddBasicBlocks then
       AddBasic(TreeObjects.Items.AddChildObject(nil,TeeMsg_MakerBasic,NodeFolderData));

    AddDummy(TreeObjects.Items.AddChildObject(nil,TeeMsg_WebLibrary,NodeLinkData));

    FillFolders(TreeObjects,nil,LibraryPath+'\'+TeeMsg_ObjectsLibrary,AddMakerFile);
  end;

  FillTextures;
end;

procedure TMakerLibrary.FillTextures;
begin
  if GroupTextures.Visible then
     if TreeTextures.Items.Count=0 then
        FillFolders(TreeTextures,nil,LibraryPath+'\'+TeeMsg_TexturesLibrary,AddGraphicFile);
end;

function TMakerLibrary.IsDragOkFrom(Source:TObject; ATree:TTreeView):Boolean;
begin
  result:=(Source=ATree) and Assigned(ATree.Selected) and
          NodeIsFile(ATree.Selected);
end;

function LibNodeImage(Node:TTreeNode):Integer;
begin
  if TMakerLibrary.NodeIsLink(Node) then
     if Node.Expanded then result:=5
                      else result:=6
  else
  if Node.HasChildren or (not TMakerLibrary.NodeIsFile(Node)) then
     if Node.Expanded then result:=1
                      else result:=0
  else
     result:=2
end;

procedure TMakerLibrary.TreeObjectsGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=LibNodeImage(Node);
end;

procedure TMakerLibrary.TreeObjectsGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex:=LibNodeImage(Node);
end;

procedure TMakerLibrary.TreeTexturesGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex:=LibNodeImage(Node);
end;

procedure TMakerLibrary.TreeTexturesGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex:=LibNodeImage(Node);
end;

function TMakerLibrary.IsBasicFolder(ANode:TTreeNode):Boolean;
begin
  result:=Assigned(ANode) and
          (ANode.Data=NodeFolderData) and
          (ANode.Text=TeeMsg_MakerBasic) and (not Assigned(ANode.Parent));
end;

function TMakerLibrary.IsBasicNode(ANode:TTreeNode):Boolean;
begin
  result:=Assigned(ANode) and Assigned(ANode.Parent) and
          (not NodeIsFile(ANode.Parent)) and
          IsBasicFolder(ANode.Parent);
end;

procedure TMakerLibrary.Open3Click(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeObjects.Selected;

  if Assigned(tmp) then
     if NodeIsFile(tmp) and (not IsBasicNode(tmp)) then
        TreeObjectsDblClick(Self)
     else
     if (not NodeIsFile(tmp)) then
        tmp.Expand(False);
end;

class function TMakerLibrary.NodePath(APath:String; ANode:TTreeNode):String;
var tmpIsLink : Boolean;
    tmpLink   : String;
begin
  if Assigned(ANode) then
  begin
    result:=ANode.Text;

    tmpIsLink:=NodeIsLink(ANode);

    if tmpIsLink then
    begin
      tmpLink:=result;
      result:='';
    end;

    while Assigned(ANode.Parent) do
    begin
      ANode:=ANode.Parent;

      if NodeIsLink(ANode) then
      begin
        if Assigned(ANode.Parent) or (ANode.Text<>TeeMsg_WebLibrary) then
           result:=NodePath(APath,ANode)+'\'+result
        else
           result:=ANode.Text+'\'+result;

        Exit;
      end;

      result:=ANode.Text+'\'+result;
    end;

    if Copy(APath,Length(APath)-1,1)='\' then
       result:=APath+result
    else
       result:=APath+'\'+result;

    if tmpIsLink then
       result:=NodeLinkPath(result+tmpLink+TeeLinkExtension);
  end
  else
    result:=APath;
end;

function TMakerLibrary.NodePath(ATree:TTreeView; ANode:TTreeNode):String;
begin
  if Assigned(ANode) and (ANode.Text=TeeMsg_WebLibrary) then
     result:=ANode.Text
  else
  begin
    if ATree=TreeObjects then
       result:=NodePath(LibraryPath+'\'+TeeMsg_ObjectsLibrary+'\',ANode)
    else
       result:=NodePath(LibraryPath+'\'+TeeMsg_TexturesLibrary+'\',ANode);
  end;
end;

procedure TMakerLibrary.TreeObjectsDblClick(Sender: TObject);
var tmp : TTreeNode;
begin
  if Assigned(FOnOpenObject) then
  begin
    tmp:=TreeObjects.Selected;

    if Assigned(tmp) then
       if NodeIsFile(tmp) and (not IsBasicNode(tmp)) then
          FOnOpenObject(tmp);
  end;
end;

class function TMakerLibrary.NodeIsLink(ANode:TTreeNode):Boolean;
begin
  result:=Assigned(ANode) and (ANode.Data=NodeLinkData);
end;

procedure TMakerLibrary.FinishCreateFolder(ATree:TTreeView; ANode:TTreeNode);
var t   : Integer;
    tmpName,
    tmpP : String;
begin
  tmpP:=NodePath(ATree,ANode)+'\';

  tmpName:=TeeMsg_Folder;
  t:=0;

  while {$IFDEF D15}SysUtils.{$ENDIF}DirectoryExists(tmpP+tmpName) do
  begin
    Inc(t);
    tmpName:=TeeMsg_Folder+' '+IntToStr(t);
  end;

  ANode:=ATree.Items.AddChildObject(ANode,tmpName,NodeFolderData);

  ANode.Selected:=True;

  CreateDir(tmpP+tmpName);
end;

procedure TMakerLibrary.AddFolder1Click(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeObjects.Selected;

  if Assigned(tmp) and NodeIsFile(tmp) then
  begin
    if IsBasicNode(tmp) then
       tmp:=nil
    else
       tmp:=tmp.Parent;
  end;

  FinishCreateFolder(TreeObjects,tmp);
end;

procedure TMakerLibrary.DoDeleteTree(ATree:TTreeView);

  function ItemName(ANode:TTreeNode):String;
  begin
    if NodeIsFile(ANode) then
       result:='file: '+ANode.Text
    else
       result:='folder: '+ANode.Text;
  end;

var tmp : TTreeNode;
    tmpP : String;
begin
  tmp:=ATree.Selected;

  if Assigned(tmp) then
  if TeeYesNo(Format(TeeMsg_SureToDelete,[ItemName(tmp)])) then
  begin
    tmpP:=NodePath(ATree,tmp.Parent)+'\';

    if NodeIsFile(tmp) then
       DeleteFile(tmpP+tmp.Text)
    else
    if not NodeIsLink(tmp) then
       RemoveDir(tmpP+tmp.Text)
    else
       DeleteFile(tmpP+tmp.Text+TeeLinkExtension);

    tmp.Free;
  end;
end;

procedure TMakerLibrary.Delete3Click(Sender: TObject);
begin
  DoDeleteTree(TreeObjects);
end;

procedure TMakerLibrary.PopupObjectsPopup(Sender: TObject);
begin
  Delete3.Enabled:=Assigned(TreeObjects.Selected) and (not IsBasicNode(TreeObjects.Selected))
                   and (not IsBasicFolder(TreeObjects.Selected));

  Open3.Enabled:=Delete3.Enabled;
  Rename1.Enabled:=Delete3.Enabled;

  Addtoscene1.Enabled:=Assigned(TreeObjects.Selected) and (not IsBasicFolder(TreeObjects.Selected));

  CreateLinkto1.Enabled:=(not Assigned(TreeObjects.Selected)) or
         ((not IsBasicNode(TreeObjects.Selected) and (not IsBasicFolder(TreeObjects.Selected))));

  AddFolder1.Enabled:=CreateLinkto1.Enabled;

  OpeninExplorer1.Enabled:=Rename1.Enabled and (not NodeIsFile(TreeObjects.Selected));
end;

procedure TMakerLibrary.TreeObjectsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tmp,
    tmpDest : TTreeNode;
begin
  tmp:=TreeObjects.Selected;

  Accept:=(Source=TreeObjects) and Assigned(tmp) and
          (not IsBasicNode(tmp));

  if Accept then
  begin
    tmpDest:=TreeObjects.GetNodeAt(X,Y);

    Accept:=tmp<>tmpDest;

    if Accept then
       Accept:=((not Assigned(tmpDest)) and Assigned(tmp.Parent)) or
               (Assigned(tmpDest) and (not NodeIsFile(tmpDest)) and
                (tmpDest<>tmp.Parent) and
                (not IsBasicFolder(tmpDest)));
  end;
end;

Function SystemLastError(const Error:Integer):String;
begin
  result:=' Error Code: '+IntToStr(Error)+' Description: '+SysErrorMessage(Error);
end;

Procedure FileMove(Const SourcePath,FileName,DestPath:String);
begin
  if not MoveFile(PChar(SourcePath+'\'+FileName),PChar(DestPath+'\'+FileName)) then
     raise Exception.Create('Error Moving file: '+FileName+' from '+SourcePath+' to: '+DestPath+
                             #13+SystemLastError(GetLastError));
end;

function TMakerLibrary.DropExternalFile(ATree:TTreeView; const AFile:String):Boolean;
var P       : TPoint;
    tmpNew  : TTreeNode;
    tmpDest : TTreeNode;
    tmpFile : String;
    tmp     : String;
begin
  P:=ATree.ScreenToClient(Mouse.CursorPos);

  result:=PtInRect(ATree.ClientRect,P);

  if result then
  begin
    tmpDest:=ATree.GetNodeAt(P.X,P.Y);

    if NodeIsFile(tmpDest) then
       tmpDest:=tmpDest.Parent;

    tmpFile:=ExtractFileName(AFile);

    tmp:=NodePath(ATree,tmpDest)+'\'+tmpFile;

    CopyFile(PChar(AFile),PChar(tmp),False);

    if ATree=TreeObjects then
       tmpFile:=RemoveFileExtension(tmpFile);

    tmpNew:=ATree.Items.AddChildObject(tmpDest,tmpFile,Pointer(1));

    if Assigned(tmpDest) and (not tmpDest.Expanded) then
       tmpDest.Expand(False);

    ATree.Selected:=tmpNew;
  end;
end;

procedure TMakerLibrary.DropTreeNode(ATree:TTreeView; ANode:TTreeNode; const AExt:String);
var tmp  : TTreeNode;
    tmp1 : String;
    tmp2 : String;
begin
  tmp:=ATree.Selected;

  tmp1:=NodePath(ATree,tmp.Parent);
  tmp2:=NodePath(ATree,ANode);

  if NodeIsFile(tmp) then
     FileMove(tmp1,tmp.Text+AExt,tmp2)
  else
  if NodeIsLink(tmp) then
     FileMove(tmp1,tmp.Text+TeeLinkExtension,tmp2)
  else
     FileMove(tmp1,tmp.Text,tmp2);

  tmp.MoveTo(ANode,naAddChild);
end;

procedure TMakerLibrary.TreeObjectsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  DropTreeNode(TreeObjects, TreeObjects.GetNodeAt(X,Y),TeeMakerExtension);
end;

procedure TMakerLibrary.Rename1Click(Sender: TObject);
begin
  TreeObjects.Selected.EditText;
end;

procedure RenameDirectory(const DirFrom, DirTo: String);
var shellInfo : TSHFileOpStruct;
    tmp       : Integer;
begin
  with shellInfo do
  begin
    Wnd    := 0;
    wFunc  := FO_RENAME;
    pFrom  := PChar(DirFrom);
    pTo    := PChar(DirTo);
    fFlags := FOF_FILESONLY or FOF_ALLOWUNDO or
              FOF_SILENT or FOF_NOCONFIRMATION;
  end;

  tmp:=SHFileOperation(shellInfo);

  if tmp<>0 then
     Raise Exception.CreateFmt(TeeMsg_ErrorRenamingFolder,[DirFrom,DirTo,SystemLastError(tmp)]);
end;

procedure TMakerLibrary.FinishNodeEdited(Node: TTreeNode; const AExt:String; var S:String);
var tmp  : String;
begin
  tmp:=NodePath(TTreeView(Node.TreeView),Node.Parent);

  if tmp<>'' then
     tmp:=tmp+'\';

  try
    if NodeIsFile(Node) then
       RenameFile(tmp+Node.Text+AExt,tmp+s+AExt)
    else
    if NodeIsLink(Node) then
       RenameFile(tmp+Node.Text+TeeLinkExtension,tmp+s+TeeLinkExtension)
    else
       RenameDirectory(tmp+Node.Text,tmp+s);
  except
    on Exception do
    begin
      S:=Node.Text;
      Raise;
    end;
  end;
end;

procedure TMakerLibrary.CreateFolder1Click(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeTextures.Selected;

  if Assigned(tmp) and NodeIsFile(tmp) then
     tmp:=tmp.Parent;

  FinishCreateFolder(TreeTextures,tmp);
end;

procedure TMakerLibrary.Rename2Click(Sender: TObject);
begin
  TreeTextures.Selected.EditText;
end;

procedure TMakerLibrary.Delete4Click(Sender: TObject);
begin
  DoDeleteTree(TreeTextures);
end;

function TMakerLibrary.SelectedTexture:String;
var tmp : TTreeNode;
begin
  tmp:=TreeTextures.Selected;

  if Assigned(tmp) then
     result:=NodePath(TreeTextures,tmp)
  else
     result:='';
end;

procedure TMakerLibrary.PopupTexturesPopup(Sender: TObject);
var tmp : TTreeNode;
begin
  tmp:=TreeTextures.Selected;

  Delete4.Enabled:=Assigned(tmp);
  Rename2.Enabled:=Delete4.Enabled;

  Preview1.Enabled:=Assigned(tmp);

  OpeninExplorer2.Enabled:=Delete4.Enabled and (not NodeIsFile(tmp));
end;

procedure TMakerLibrary.TreeTexturesEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
begin
  FinishNodeEdited(Node,'',S);
end;

procedure TMakerLibrary.TreeObjectsEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  FinishNodeEdited(Node,TeeMakerExtension,S);
end;

procedure TMakerLibrary.TreeTexturesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tmp,
    tmpDest : TTreeNode;
begin
  tmp:=TreeTextures.Selected;

  Accept:=(Source=TreeTextures) and Assigned(tmp);

  if Accept then
  begin
    tmpDest:=TreeTextures.GetNodeAt(X,Y);

    Accept:=tmp<>tmpDest;

    if Accept then
       Accept:=((not Assigned(tmpDest)) and Assigned(tmp.Parent)) or
               (Assigned(tmpDest) and (not NodeIsFile(tmpDest)) and
                (tmpDest<>tmp.Parent));
  end;
end;

procedure TMakerLibrary.TreeTexturesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  DropTreeNode(TreeTextures, TreeTextures.GetNodeAt(X,Y),'');
end;

procedure TMakerLibrary.OpeninExplorer1Click(Sender: TObject);
begin
  TeeGotoURL(Handle, NodePath(TreeObjects,TreeObjects.Selected));
end;

procedure TMakerLibrary.OpeninExplorer2Click(Sender: TObject);
begin
  TeeGotoURL(Handle, NodePath(TreeTextures,TreeTextures.Selected));
end;

procedure TMakerLibrary.Refresh1Click(Sender: TObject);
begin
  ReFillTrees;
end;

procedure TMakerLibrary.TryLinkFolder(ATree:TTreeView);
var tmpName : String;
    tmpP    : String;
    tmpPath : String;
    tmpN    : String;
    tmp     : TTreeNode;
    f       : TextFile;
    t       : Integer;
begin
  tmpName:='';

  if TTeeVCL.SelectFolder(TeeMsg_FolderToLink,'',tmpName) then
  begin
    tmp:=ATree.Selected;

    if Assigned(tmp) and NodeIsFile(tmp) then
       tmp:=tmp.Parent;

    tmpN:=ExtractFileName(tmpName);

    if tmpN='' then
       tmpN:=TeeMsg_Link;

    t:=0;

    tmpPath:=NodePath(ATree,tmp);

    repeat
      tmpP:=tmpPath+'\'+tmpN;

      if t>0 then
         tmpP:=tmpP+' '+IntToStr(t);

      Inc(t);
    until not FileExists(tmpP);

    AssignFile(f,tmpP+TeeLinkExtension);
    Rewrite(f);
    Writeln(f,tmpName);
    CloseFile(f);

    tmp:=ATree.Items.AddChildObject(tmp,tmpN,NodeLinkData);

    AddDummy(tmp);

    tmp.Selected:=True;
  end;
end;

procedure TMakerLibrary.Linktofolder1Click(Sender: TObject);
begin
  TryLinkFolder(TreeTextures);
end;

procedure TMakerLibrary.CreateLinkto1Click(Sender: TObject);
begin
  TryLinkFolder(TreeObjects);
end;

procedure TMakerLibrary.TreeTexturesDblClick(Sender: TObject);
begin
  if NodeIsFile(TreeTextures.Selected) then
     Preview1Click(Self);
end;

procedure TMakerLibrary.SelectTextureFile(const FileName:String);
var tmp : String;
    t   : Integer;
begin
  tmp:=UpperCase(TBlocks.ParseFileName(TeeMsg_TexturesLibrary,FileName));

  with TreeTextures.Items do
  for t:=0 to Count-1 do
  if NodeIsFile(Item[t]) then
     if UpperCase(NodePath(TreeTextures,Item[t]))=tmp then
     begin
       TreeTextures.Selected:=Item[t];
       break;
     end;
end;

class procedure TMakerLibrary.CheckDummyNode(Sender: TObject; Node: TTreeNode;
                              AProc:TAddNodeProc;
                              const APath:String; var AllowExpansion: Boolean);

  function HasDummy:Boolean;
  begin
    result:=(Node.Count=1) and (Node.Item[0].Data=NodeDummyData);
  end;

var tmpPath : String;
begin
  if HasDummy then
  begin
    Screen.Cursor:=crHourGlass;
    try
      Node.DeleteChildren;

      if (not Assigned(Node.Parent)) and
         (Node.Text=TeeMsg_WebLibrary) then
         FillURL((Sender as TTreeView),Node,'',AProc)
      else
      begin
        tmpPath:=NodePath(APath,Node);

        if Copy(tmpPath,1,Length(TeeMsg_WebLibrary))=TeeMsg_WebLibrary then
        begin
          tmpPath:=Copy(tmpPath,Length(TeeMsg_WebLibrary)+1,Length(tmpPath));
          tmpPath:=ReplaceChar(tmpPath,'/','\');
          FillURL((Sender as TTreeView),Node,tmpPath,AProc);
        end
        else
          FillFolders((Sender as TTreeView),Node,tmpPath,AProc);
      end;

      AllowExpansion:=Node.Count>0;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TMakerLibrary.TreeTexturesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if Sender=TreeObjects then
     CheckDummyNode(Sender,Node,AddMakerFile,LibraryPath+'\'+TeeMsg_ObjectsLibrary,AllowExpansion)
  else
     CheckDummyNode(Sender,Node,AddGraphicFile,LibraryPath+'\'+TeeMsg_TexturesLibrary,AllowExpansion);
end;

function TMakerLibrary.SelectedIsMaker:Boolean;
begin
  result:=TreeObjects.Selected.Data=Pointer(1);
end;

function TMakerLibrary.SelectedIsObj:Boolean;
begin
  result:=TreeObjects.Selected.Data=Pointer(2);
end;

function TMakerLibrary.SelectedLinkFile:String;
begin
  if SelectedIsMaker then
     result:=LinkFile(TreeObjects,TreeObjects.Selected,TeeMakerExtension)
  else
  if SelectedIsObj then
     result:=LinkFile(TreeObjects,TreeObjects.Selected,TeeObjExtension)
  else
     result:=LinkFile(TreeObjects,TreeObjects.Selected,Tee3DSExtension);
end;

function TMakerLibrary.LinkFile(ATree: TTreeView; ANode: TTreeNode; const AExtension:String): String;
begin
  result:=LinkFile(NodePath(ATree,ANode),ANode.Text,AExtension);
end;

function TMakerLibrary.LinkFile(const APath,AFile,AExtension:String): String;
var tmpPath : String;
begin
  tmpPath:=APath;

  if Copy(tmpPath,1,Length(TeeMsg_WebLibrary))=TeeMsg_WebLibrary then
  begin
    tmpPath:=Copy(tmpPath,Length(TeeMsg_WebLibrary)+1,Length(tmpPath));
    tmpPath:=TeeMsg_WebLibraryURL+tmpPath;
  end;

  if Copy(tmpPath,Length(tmpPath),1)='\' then
     Delete(tmpPath,Length(tmpPath),1);

  if Copy(tmpPath,Length(tmpPath)-Length(AFile)+1,Length(AFile))=AFile then
     result:=tmpPath
  else
     result:=tmpPath+'\'+AFile;

  if ExtractFileExt(result)='' then
     result:=result+AExtension;
end;

procedure TMakerLibrary.Preview1Click(Sender: TObject);
begin
  TTextureSelector.ModalShow(Self,NodePath(TreeTextures,TreeTextures.Selected));
end;

procedure TMakerLibrary.TreeTexturesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if TreeTextures.GetNodeAt(X,Y)=nil then
     TreeTextures.Selected:=nil;
end;

procedure TMakerLibrary.TreeObjectsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htNoWhere in TreeObjects.GetHitTestInfoAt(X,Y) then
     TreeObjects.Selected:=nil;
end;

procedure TMakerLibrary.Refresh2Click(Sender: TObject);
begin
  ReFillTrees;
end;

end.
