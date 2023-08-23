unit TeeSoundSelector;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  Classes,

  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls, QStdCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  {$ENDIF}
  SysUtils;

type
  TSoundSelector = class(TForm)
    Panel1: TPanel;
    TreeSounds: TTreeView;
    Panel2: TPanel;
    BOK: TButton;
    Button2: TButton;
    BPreview: TButton;
    TrackVolume: TTrackBar;
    Label1: TLabel;
    procedure TreeSoundsChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure TreeSoundsExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeSoundsDblClick(Sender: TObject);
    procedure BPreviewClick(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
  private
    { Private declarations }

    procedure AddSoundFile(ATree:TTreeView; AParent:TTreeNode;
                           const APath,AName:String);
    function LibrarySounds:String;
  public
    { Public declarations }
    LibraryPath : String;

    class function ModalShow(AOwner:TComponent; const ALibPath:String):String;
    function SelectedSoundFile:String;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeeMakerLibrary, TeeMakerConst, TeeBlocks, TeePlayMP3;

class function TSoundSelector.ModalShow(AOwner:TComponent; const ALibPath:String):String;
begin
  result:='';

  with TSoundSelector.Create(AOwner) do
  try
    LibraryPath:=ALibPath;

    if ShowModal=mrOk then
       result:=SelectedSoundFile;
  finally
    Free;
  end;
end;

procedure TSoundSelector.AddSoundFile(ATree:TTreeView; AParent:TTreeNode;
                                      const APath,AName:String);

  function IsSoundFile(const AFileName:String):Boolean;
  var tmp : String;
  begin
    tmp:=UpperCase(ExtractFileExt(AFileName));

    result:=(tmp='.MP3') or (tmp='.WAV');
  end;

begin
  if IsSoundFile(AName) then
     ATree.Items.AddChildObject(AParent,AName,Pointer(1))
  else
     TMakerLibrary.TryAddLinkFile(ATree,AParent,APath,AName);
end;

procedure TSoundSelector.TrackVolumeChange(Sender: TObject);
begin
  with TPlayMP3Sound.Create(nil) do
  try
    Volume:=TrackVolume.Position;
  finally
    Free;
  end;

  Label1.Caption:=IntToStr(TrackVolume.Position);
end;

procedure TSoundSelector.TreeSoundsChange(Sender: TObject; Node: TTreeNode);
begin
  BOK.Enabled:=Assigned(TreeSounds.Selected) and (TreeSounds.Selected.Count=0);
  BPreview.Enabled:=BOK.Enabled;
end;

function TSoundSelector.LibrarySounds:String;
begin
  result:=LibraryPath+'\'+TeeMsg_SoundsLibrary;
end;

procedure TSoundSelector.FormShow(Sender: TObject);
begin
  TMakerLibrary.FillFolders(TreeSounds,nil,LibrarySounds,AddSoundFile);

  with TPlayMP3Sound.Create(nil) do
  try
    TrackVolume.Position:=Volume;
  finally
    Free;
  end;

  TrackVolumeChange(Self);
end;

procedure TSoundSelector.TreeSoundsExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  TMakerLibrary.CheckDummyNode(Sender,Node,AddSoundFile,LibrarySounds,AllowExpansion);
end;

procedure TSoundSelector.TreeSoundsDblClick(Sender: TObject);
begin
  if Assigned(TreeSounds.Selected) and (TreeSounds.Selected.Count=0) then
     ModalResult:=mrOk;
end;

function TSoundSelector.SelectedSoundFile:String;
begin
  result:=TMakerLibrary.NodePath(LibrarySounds,TreeSounds.Selected);
  TBlocks.CheckLibraryPath(TeeMsg_SoundsLibrary,result);
end;

procedure TSoundSelector.BPreviewClick(Sender: TObject);
begin
  TPlayMP3Sound.PlayFile(TMakerLibrary.NodePath(LibrarySounds,TreeSounds.Selected));
end;

end.
