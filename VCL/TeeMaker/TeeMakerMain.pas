{********************************************}
{ TeeMaker 2.0                               }
{ Copyright (c) 2002-2023 by Steema Software }
{ All Rights Reserved                        }
{********************************************}
unit TeeMakerMain;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs,
  QActnList, QImgList, QMenus, QComCtrls, QStdCtrls, QButtons, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, Menus, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  {$ENDIF}

  {$IFDEF FASTMM}
  FastMM4,
  {$ENDIF}

  TeeMakerEditor, TeeProcs, TeeDraw3D, TeCanvas, TeeComma, TeeEdit,
  TeeMakerControl, OpenGL2;

type
  TMakerEditor1 = class(TMakerEditor)
    N3: TMenuItem;
    GenerateTypes1: TMenuItem;
    Normal1: TMenuItem;
    WithTextures1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    MemoryTracker1: TMenuItem;
    Language1: TMenuItem;
    English1: TMenuItem;
    Spanish1: TMenuItem;
    TabGLExtensions: TTabSheet;
    PageControl1: TPageControl;
    TabExtensions: TTabSheet;
    MemoExtensions: TMemo;
    TabSheet3: TTabSheet;
    MemoGLInfo: TMemo;
    TabSheet4: TTabSheet;
    Panel2: TPanel;
    Label23: TLabel;
    Edit1: TEdit;
    UDPixelFormat: TUpDown;
    LPixelFormatCount: TLabel;
    MemoPixelFormat: TMemo;
    Button3: TButton;
    Label27: TLabel;
    LTotalMemory: TLabel;
    TabProgram: TTabSheet;
    PageControl3: TPageControl;
    TabShaders: TTabSheet;
    Panel13: TPanel;
    BCompileShader: TButton;
    MemoVertexShader: TMemo;
    CBShaderActive: TCheckBox;
    TabShaderDebug: TTabSheet;
    MemoShaderLog: TMemo;
    MemoFragmentShader: TMemo;
    Splitter2: TSplitter;
    ComboShaders: TComboFlat;
    PageShaders: TPageControl;
    TabSheet7: TTabSheet;
    TabSheet9: TTabSheet;
    MemoDefaultVertex: TMemo;
    MemoDefaultFragment: TMemo;
    Button4: TButton;
    CBMouseXYZ: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Normal1Click(Sender: TObject);
    procedure Withtextures1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure MemoryTracker1Click(Sender: TObject);
    procedure English1Click(Sender: TObject);
    procedure Spanish1Click(Sender: TObject);
    procedure EditMode1Click(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BCompileShaderClick(Sender: TObject);
    procedure CBShaderActiveClick(Sender: TObject);
    procedure MemoVertexShaderChange(Sender: TObject);
    procedure ComboShadersChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }

    procedure DepthBufferToBitmap(const tmpBitmap:TBitmap);

    procedure GenerateTypes(Normal:Boolean);

    function GetPixelAttribute(Attribute:Integer; var Value:Integer):Boolean;
    procedure LoadIniConfig;
    function OpenGLRenderer:String;
    procedure ReopenItem(Sender: TObject);
    procedure WriteIniConfig;
  protected
    procedure ActivateMaker(const AMaker:TMaker); override;
    procedure AddReopen(AName:String); override;
    procedure CheckReopen; override;
    procedure Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
  public
    { Public declarations }
  end;

var
  MakerEditor1: TMakerEditor1;

{$IFDEF D6}
{$DEFINE REGMAKER}
{$ELSE}
{$IFNDEF BCB}
{$DEFINE REGMAKER}
{$ENDIF}
{$ENDIF}

{$IFDEF REGMAKER}
Procedure RegisterTeeMaker;
{$ENDIF}

Procedure TrimWorkingSet;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  Math, ShlObj, Registry, TeeGeometry,

  {$IFDEF FASTMM}
  {$IFOPT D+}
  FastMMUsageTracker,
  {$ENDIF}
  {$ENDIF}

  TeeGLUT,
  TeeVideoBlock,

  TeeAbout, TeeOpenGL, TeeBlocks, TeePenDlg, TeeMakerConst,
  TeeGLCanvas, TeeGLSLShaders, TeeMakerLibrary, TeeTextureSelector;

procedure TMakerEditor1.FormShow(Sender: TObject);
var t : Integer;
begin
  CanAddFirstEmptyMaker:=True;

  inherited;

  for t:=1 to ParamCount do
  if UpperCase(ParamStr(t))<>'-DEBUG' then
  begin
    DoLoad(ParamStr(t));
    CanAddFirstEmptyMaker:=False;
  end;

  if not CanAddFirstEmptyMaker then
     InitFirstMaker;

  CurrentMaker.Render.Antialias:=True;
  AntiAlias1.Checked:=True;

  Resetview1Click(Self);
  RGNavigateClick(Self);
  SetEditMode(EditMode1.Checked);
end;

{$IFDEF REGMAKER}

// Registers the file extension to open TeeMaker application
// when double-clicking a file in Windows Explorer.

Procedure RegisterTeeMaker;
Const
  TeeMaker_MIME='\MIME\Database\Content Type\application/teeMaker';
  TeeMakerKey='TeeMaker';

{$IFNDEF LINUX}
var tmp : TRegistry;
{$ENDIF}

  {$IFNDEF LINUX}
  // Returns True if the TeeMaker file extension is correctly registered
  Function ExistExtension:Boolean;
  begin
    result:=tmp.KeyExists(TeeMakerExtension);

    if result then
       if tmp.OpenKey(TeeMakerKey+'\DefaultIcon',True) then
          result:=tmp.ReadString('')=ParamStr(0)+',0';
  end;
  {$ENDIF}

begin
  {$IFNDEF LINUX}
  tmp:=TRegistry.Create;

  with tmp do
  try
    RootKey:=HKEY_CLASSES_ROOT;

    if not ExistExtension then { check if exists... }
    begin
      { extension }
      if OpenKey(TeeMakerExtension,True) then
      try
        WriteString('',TeeMakerKey);
        WriteString('Content Type','application/teeMaker');
      finally
        CloseKey;
      end;

      { application }
      if OpenKey(TeeMakerKey,True) then
      begin
        WriteString('',TeeMsg_TeeMakerFile);

        if OpenKey('DefaultIcon',True) then
        begin
          WriteString('',ParamStr(0)+',0');
          CloseKey;
        end;

        if OpenKey(TeeMakerKey,True) then
        if OpenKey('Shell',True) then
           if OpenKey('Open',True) then
           begin
             WriteString('',TeeMsg_OpenWithTeeMaker);

             if OpenKey('Command',True) then
                WriteString('',ParamStr(0)+' "%1"');
           end;
      end;

      { Notify Windows on creating the new file association }
      SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,nil,nil);
    end;

    { Add MIME content-type to registry database... }
    if not KeyExists(TeeMaker_MIME) then
    begin
      if OpenKey(TeeMaker_MIME,True) then
         WriteString('Extension',TeeMakerExtension);
    end;
  finally
    Free;
  end;

  {$ENDIF}
end;
{$ENDIF}

procedure TMakerEditor1.FormCreate(Sender: TObject);
begin
  // Multiple Makers (Tabs)
  PanelBig.BevelOuter:=bvNone;
  PageMakers:=TPageControl.Create(Self);
  PageMakers.OnChange:=PageMakersChange;
  PageMakers.Align:=alClient;
  PageMakers.Parent:=PanelBig;
  PageMakers.PopupMenu:=PopupTabs;

  inherited;

  {$IFOPT D-}
  N3.Visible:=False;
  GenerateTypes1.Visible:=UpperCase(ParamStr(1))<>'-DEBUG';
  {$ELSE}
  MemoryTracker1.Visible:=True;
  {$ENDIF}


  {$IFDEF REGMAKER}
  RegisterTeeMaker;
  {$ENDIF}

  LoadIniConfig;

  PageControl1.ActivePage:=TabExtensions;
  PageControl2.ActivePage:=TabLinks;
end;

procedure TMakerEditor1.WriteIniConfig;

  procedure WriteLight(const R:TRegistry; ALight:TGLLightSource);
  begin
    with R,ALight do
    begin
      WriteBool('Visible',Visible);
      WriteInteger('Color',Color);
      WriteFloat('Position.X',Position.X);
      WriteFloat('Position.Y',Position.Y);
      WriteFloat('Position.Z',Position.Z);
    end;
  end;

var t : Integer;
begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_CURRENT_USER;

    if OpenKey(TeeMakerKey+'\Main',True) then
    begin
      WriteBool('Maximized',WindowState=wsMaximized);

      if WindowState=wsNormal then
      begin
        WriteInteger('Left',Left);
        WriteInteger('Top',Top);
        WriteInteger('Width',Width);
        WriteInteger('Height',Height);
      end;

      {
      with Maker1.Gradient do
      begin
        WriteBool('Panel','GradientVisible',Visible);
        WriteInteger('Panel','GradientStart',StartColor);
        WriteInteger('Panel','GradientMid',MidColor);
        WriteInteger('Panel','GradientEnd',EndColor);
      end;
      }

      with IBlockEditor.OpenDialog1 do
      if InitialDir<>GetCurrentDir then
         WriteString('Current',InitialDir);

      WriteBool('Mute',SBMute.Down);

      WriteString('Address',IHome);

      // Library
      WriteInteger('GroupTextures',ILibrary.GroupTextures.Height);

      CloseKey;
    end;

    if OpenKey(TeeMakerKey+'\Main\View',True) then
    begin
      if EditMode1.Checked then
      begin
        WriteBool('Editor',PageEditor.Visible);
        WriteBool('Animations',Assigned(IAnimEditor) and IAnimEditor.Visible);
      end
      else
      begin
        WriteBool('Editor',WasEditor);
        WriteBool('Animations',WasAnimations);
      end;

      // Edit Mode
      WriteBool('EditMode',EditMode1.Checked);

      WriteInteger('PanelTree',PanelEditor.Height);
      WriteInteger('PageTree',PageEditor.Width);

      if Assigned(IAnimEditor) then
         WriteInteger('AnimationSize',IAnimEditor.Height);

      {
      with Maker1.View3DOptions do
      begin
        WriteFloat('Zoom',ZoomFloat);
        WriteInteger('Rotation',Rotation);
        WriteInteger('Elevation',Elevation);
        WriteInteger('Tilt',Tilt);
        WriteInteger('HorizOffset',HorizOffset);
        WriteInteger('VertOffset',VertOffset);
        WriteInteger('Perspective',Perspective);
      end;
      }

      WriteBool('AntiAlias',AntiAlias1.Checked);

      {
      with Maker1.Options do
      begin
        WriteInteger(ViewKey,'WalkingMode',Ord(Navigate.Mode));

        // Reflection
        WriteInteger('View','FloorReflection',Floor.Reflection);
        WriteBool('View','FloorVisible',Floor.Visible);

        // Shadows
        WriteBool('View','DrawShadows',DrawShadows);
        WriteInteger('View','ShadowColor',Maker1.Blocks.ShadowColor);
        WriteInteger('View','ShadowTransp',Maker1.Blocks.ShadowTransp);

        // Bounding
        WriteBool('View','BoundingBox',BoundingBox);

        // Threading
        WriteBool('General','UseThreads',UseThreads);
      end;
      }

      CloseKey;
    end;

    {
    WriteLight('Render.Light0',Maker1.Render.Light0);
    WriteLight('Render.Light1',Maker1.Render.Light1);
    WriteLight('Render.Light2',Maker1.Render.Light2);
    }

    if Reopen1.Count>0 then
    if OpenKey(TeeMakerKey+'\Main\Recent',True) then
    begin
      WriteInteger('Count',Reopen1.Count);

      for t:=0 to Reopen1.Count-1 do
          WriteString('Item'+TeeStr(t),RemoveAmpersand(Reopen1.Items[t].Caption));

      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TMakerEditor1.LoadIniConfig;

  procedure ReadLight(const R:TRegistry; ALight:TGLLightSource);
  begin
    with R,ALight do
    begin
      if ValueExists('Visible') then
         Visible:=ReadBool('Visible');

      if ValueExists('Color') then
         Color:=ReadInteger('Color');

      if ValueExists('Position.X') then
         Position.X:=ReadFloat('Position.X');

      if ValueExists('Position.Y') then
         Position.Y:=ReadFloat('Position.Y');

      if ValueExists('Position.Z') then
         Position.Z:=ReadFloat('Position.Z');
    end;
  end;

begin
  with TRegistry.Create do
  try
    RootKey:=HKEY_CURRENT_USER;

    if OpenKeyReadOnly(TeeMakerKey+'\Main') then
    begin
      Position:=poDesigned;

      if ValueExists('Maximized') and ReadBool('Maximized') then
         WindowState:=wsMaximized
      else
      begin
        if ValueExists('Left') then
           Left:=ReadInteger('Left');

        if ValueExists('Left') then
           Top:=ReadInteger('Top');

        if ValueExists('Left') then
           Width:=ReadInteger('Width');

        if ValueExists('Left') then
           Height:=ReadInteger('Height');
      end;

      if ValueExists('Mute') then
         SBMute.Down:=ReadBool('Mute');

      if SBMute.Down then
         SBMuteClick(Self);

      if ValueExists('Current') then
      with IBlockEditor.OpenDialog1 do
           InitialDir:=ReadString('Current');

      if ValueExists('Address') then
         IHome:=ReadString('Address')
      else
         IHome:='default'+TeeMakerExtension;

      // Library
      if ValueExists('GroupTextures') then
      with ILibrary.GroupTextures do
           Height:=ReadInteger('GroupTextures');

      CloseKey;
    end;

    if OpenKeyReadOnly(TeeMakerKey+'\Main\View') then
    begin
      if ValueExists('Editor') then
      begin
        PageEditor.Visible:=ReadBool('Editor');
        Editor1.Checked:=PageEditor.Visible;

        if not Editor1.Checked then
        begin
          Editor1.Checked:=True; // <-- trick, invert
          Editor1Click(Self);
        end;
      end;

      {
      with Maker1.Gradient do
      begin
        Visible:=ReadBool('Panel','GradientVisible',Visible);
        StartColor:=ReadInteger('Panel','GradientStart',StartColor);
        MidColor:=ReadInteger('Panel','GradientMid',MidColor);
        EndColor:=ReadInteger('Panel','GradientEnd',EndColor);
      end;
      }

      if ValueExists('AntiAlias') then
      begin
        AntiAlias1.Checked:=ReadBool('AntiAlias');
        if AntiAlias1.Checked then
           SetAntiAlias(True);
      end;

      if ValueExists('PanelTree') then
      begin
        PanelEditor.Height:=Max(20,ReadInteger('PanelTree'));
        PanelEditor.Height:=Min(PanelEditor.Height,TabBlocks.Height-20);
      end;

      if ValueExists('PageTree') then
         PageEditor.Width:=Max(20,ReadInteger('PageTree'));

      {
      with Maker1.View3DOptions do
      begin
        ZoomFloat:=Max(0.001,ReadFloat(ViewKey,'Zoom',ZoomFloat));
        Rotation:=ReadInteger(ViewKey,'Rotation',Rotation);
        Elevation:=ReadInteger(ViewKey,'Elevation',Elevation);
        Tilt:=ReadInteger(ViewKey,'Tilt',Tilt);
        HorizOffset:=ReadInteger(ViewKey,'HorizOffset',HorizOffset);
        VertOffset:=ReadInteger(ViewKey,'VertOffset',VertOffset);
        Perspective:=ReadInteger(ViewKey,'Perspective',Perspective);
      end;

      with Maker1.Options.Navigate do
      begin
        Mode:=TNavigateMode(ReadInteger(ViewKey,'WalkingMode',Ord(Mode)));
        RGNavigate.ItemIndex:=Ord(Mode);
      end;
      }

      if ValueExists('Animations') and ReadBool('Animations') then
         Animations1Click(Self);

      if Assigned(IAnimEditor) then
         if ValueExists('AnimationSize') then
            IAnimEditor.Height:=ReadInteger('AnimationSize');

      CloseKey;
    end;

    {
    ReadLight('Render.Light0',Maker1.Render.Light0);
    ReadLight('Render.Light1',Maker1.Render.Light1);
    ReadLight('Render.Light2',Maker1.Render.Light2);
    }

    if OpenKeyReadOnly(TeeMakerKey+'\Main\Recent') then
    begin
      if ValueExists('Count') then
      begin
        Reopen1.Clear;
        Reopen1.Visible:=ReadInteger('Count')>0;
      end;

      CloseKey;
    end;

    {
    with Maker1.Options do
    begin
      // Shadows

      DrawShadows:=ReadBool('View','DrawShadows',DrawShadows);
      CBShadows.Checked:=DrawShadows;
      Shadows1.Checked:=DrawShadows;

      Maker1.Blocks.ShadowColor:=ReadInteger('View','ShadowColor',Maker1.Blocks.ShadowColor);
      ShadowColor.Position:=GetRValue(Maker1.Blocks.ShadowColor);

      Maker1.Blocks.ShadowTransp:=ReadInteger('View','ShadowTransp',Maker1.Blocks.ShadowTransp);
      ShadowTransp.Position:=Maker1.Blocks.ShadowTransp;

      // Bounding
      BoundingBox:=ReadBool('View','BoundingBox',BoundingBox);
      Boundingbox1.Checked:=BoundingBox;

      // Threading
      UseThreads:=ReadBool('General','UseThreads',UseThreads);
      CBThreading.Checked:=UseThreads;
    end;
    }

  finally
    Free;
  end;
end;

procedure TMakerEditor1.CheckReopen;
var t, tmpRecent : Integer;
    tmp : String;
begin
  if (Reopen1.Count=0) or Assigned(ReopenDummy) then
  begin
    FreeAndNil(ReopenDummy);

    with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;

      if OpenKeyReadOnly(TeeMakerKey+'\Main\Recent') then
      begin
        if ValueExists('Count') then
        begin
          tmpRecent:=ReadInteger('Count');

          for t:=tmpRecent-1 downto 0 do
          begin
            if ValueExists('Item'+TeeStr(t)) then
            begin
              tmp:=ReadString('Item'+TeeStr(t));

              if tmp<>'' then
                 AddReopen(tmp);
            end;
          end;
        end;

        CloseKey;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TMakerEditor1.Normal1Click(Sender: TObject);
begin
  GenerateTypes(True);
end;

procedure TMakerEditor1.Withtextures1Click(Sender: TObject);
begin
  GenerateTypes(False);
end;

type
  TBlockAccess=class(TCustomBlock);

procedure TMakerEditor1.GenerateTypes(Normal:Boolean);
var tmpList : TStringList;

  procedure AddBlocks(AStart,AEnd:Integer; const ARadius:Double);
  var tmpPi : Double;
      t : Integer;
      tmp : TCustomBlock;
      tmpSin,
      tmpCos : {$IFDEF D15}Single{$ELSE}Extended{$ENDIF};
  begin
    tmpPi:=TwoPi/(AEnd-AStart+1);

    for t:=AStart to AEnd do
    begin
      tmp:=BlockClasses[Integer(tmpList.Objects[t])].Create(CurrentMaker.Owner);
      tmp.Format.Color:=RGB(Random(255),Random(255),Random(255));

      SinCos(tmpPi*(t-AStart),tmpSin,tmpCos);

      tmp.Location.X:=ARadius*tmpSin;
      tmp.Location.Y:=ARadius*tmpCos;

      DoAddClick(tmp,BlockClasses.BlockDescription(tmp.ClassName),nil,True,False);

      TBlockAccess(tmp).PrepareForGallery;  // "protected" across assemblies

      if not Normal then
      begin
        tmp.Format.Border.Visible:=False;
        tmp.Format.Color:=clWhite;
        tmp.Format.Texture.PictureLink:=TeeMakerLibraryTag+'Basic\Jeans.bmp';
      end;

      TreeBlocks.Selected:=nil;
    end;
  end;

var
  tmpMid : Integer;
begin
  New1Click(Self);

  if (not CurrentMakerTab.Dirty) or TeeYesNo(TeeMsg_SureNewFile) then
  begin
    TreeBlocks.Items.BeginUpdate;
    try
      tmpList:=BlockClasses.Sorted;
      try
        tmpMid:=tmpList.Count div 2;

        AddBlocks(0,tmpMid,450);
        AddBlocks(tmpMid+1,tmpList.Count-1,750);
      finally
        tmpList.Free;
      end;
    finally
      TreeBlocks.Items.EndUpdate;
    end;

    FinishAdd(CurrentMaker.Blocks[0]);
  end;
end;

procedure TMakerEditor1.FormDestroy(Sender: TObject);
begin
  WriteIniConfig;
  inherited;
end;

procedure TMakerEditor1.ReopenItem(Sender: TObject);
begin
  //if DoSave(True) then
     DoLoad(RemoveAmpersand(TMenuItem(Sender).Caption));
end;

const
  MaxReopenMenu = 30;

procedure TMakerEditor1.AddReopen(AName:String);
var tmpItem : TMenuItem;
    found   : Boolean;
    t       : Integer;
    tmpA    : String;
    tmpB    : String;
begin
  AName:=RemoveAmpersand(Trim(AName));

  if AName<>'' then
  begin
    Found:=False;

    tmpB:=UpperCase(RemoveFileExtension(AName));

    for t:=0 to Reopen1.Count-1 do
    begin
      tmpA:=RemoveFileExtension(RemoveAmpersand(Trim(Reopen1.Items[t].Caption)));
      tmpA:=UpperCase(tmpA);

      if tmpA=tmpB then
      begin
        Found:=True;
        break;
      end;
    end;

    if not Found then
    //if FileExists(TBlocks.ParseFileName(TeeMsg_ObjectsLibrary,AName)) then
    begin
      while Reopen1.Count>=MaxReopenMenu do
            Reopen1.Items[0].Free;

      TBlocks.CheckLibraryPath(TeeMsg_ObjectsLibrary,AName);

      tmpItem:=TMenuItem.Create(Self);
      tmpItem.Caption:=AName;
      tmpItem.OnClick:=ReopenItem;

      Reopen1.Insert(0,tmpItem);
      Reopen1.Visible:=True;
    end;
  end;
end;

procedure TMakerEditor1.About1Click(Sender: TObject);
begin
  TeeShowAboutBox(); //('','',OpenGLRenderer);
end;

procedure TMakerEditor1.MemoryTracker1Click(Sender: TObject);
begin
  {$IFDEF FASTMM}
  {$IFOPT D+}
  ShowFastMMUsageTracker;
  {$ENDIF}
  {$ENDIF}
end;

procedure TMakerEditor1.English1Click(Sender: TObject);
begin
  English1.Checked:=True;
  TMakerLanguages.English;
end;

procedure TMakerEditor1.Spanish1Click(Sender: TObject);
begin
  Spanish1.Checked:=True;
  TMakerLanguages.Spanish;
end;

procedure TMakerEditor1.EditMode1Click(Sender: TObject);
begin
  inherited;

  if EditMode1.Checked then
     PageMakers.PopupMenu:=PopupTabs
  else
     PageMakers.PopupMenu:=nil;
end;

type
  TGLCanvasAccess=class(TGLCanvas);

function TMakerEditor1.GetPixelAttribute(Attribute:Integer; var Value:Integer):Boolean;
begin
  result:=TGLCanvasAccess(CurrentMaker.Render.Canvas).GetPixelAttribute(Attribute,
            UDPixelFormat.Position,Value);
end;

function TMakerEditor1.OpenGLRenderer:String;
begin
  result:=PChar(glGetString(GL_RENDERER));
end;

procedure TMakerEditor1.PageControl2Change(Sender: TObject);

  function MaxPixelFormats:Integer;
  begin
    if not GetPixelAttribute(WGL_NUMBER_PIXEL_FORMATS_ARB,result) then
       result:=0;
  end;

  procedure AddInteger(const AText:String; AConst:Cardinal);
  var tmpMax : Integer;
  begin
    glGetIntegerv(AConst,@tmpMax);
    MemoGLInfo.Lines.Add(AText+' = '+IntToStr(tmpMax));
  end;

  procedure AddFloats(const AText:String; AConst:Cardinal);
  type
    TFloats=packed record
       A,B: Single;
    end;
  var tmpMax : TFloats;
  begin
    glGetFloatv(AConst,@tmpMax);
    MemoGLInfo.Lines.Add(AText+' = '+FloatToStr(tmpMax.A)+' '+FloatToStr(tmpMax.B));
  end;

  procedure AddFloat(const AText:String; AConst:Cardinal);
  var tmpMax : Single;
  begin
    glGetFloatv(AConst,@tmpMax);
    MemoGLInfo.Lines.Add(AText+' = '+FloatToStr(tmpMax));
  end;

  procedure AddExtensions(const tmp:String);
  var t : Integer;
  begin
    for t:=1 to TeeNumFields(tmp,' ') do
        MemoExtensions.Lines.Add(TeeExtractField(tmp,t,' '));
  end;

  procedure TextureInternalFormat;
  var Value : Integer;
  begin
    glGetTexParameteriv(GL_TEXTURE_2D, 0, @Value);
    MemoGLInfo.Lines.Add('Texture 2D GL_INTERNAL_FORMAT = '+IntToStr(Value));
  end;

  function CPUID:String;
  const
    ProcName='ProcessorNameString';
  begin
    with TRegistry.Create do
    try
      RootKey:=HKEY_LOCAL_MACHINE;

      if OpenKey('HARDWARE\DESCRIPTION\System\CentralProcessor\0',False) and
         ValueExists(ProcName) then
         result:=ReadString(ProcName)
      else
         result:='';
    finally
      Free;
    end;
  end;

  procedure ShowExtensions;
  var t : Integer;
      tmp : String;
      wglGetExtensionsStringARB : function(DC:HDC):PChar; stdcall;
  begin
     if MemoExtensions.Lines.Count=0 then
     begin
       MemoExtensions.Lines.BeginUpdate;

       AddExtensions(PChar(glGetString(GL_EXTENSIONS)));

       wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
       if Assigned(wglGetExtensionsStringARB) then
       begin
         tmp:=PChar(wglGetExtensionsStringARB(wglGetCurrentDC));

         MemoExtensions.Lines.Add('');

         for t:=1 to TeeNumFields(tmp,' ') do
             MemoExtensions.Lines.Add(TeeExtractField(tmp,t,' '));
       end;

       MemoExtensions.Lines.Add('');
       MemoExtensions.Lines.Add('GLU Extensions:');
       AddExtensions(PChar(gluGetString(GLU_EXTENSIONS)));

       MemoExtensions.Lines.EndUpdate;

       MemoGLInfo.Lines.Add('');

       MemoGLInfo.Lines.Add('OpenGL Vendor: '+PChar(glGetString(GL_VENDOR)));
       MemoGLInfo.Lines.Add('OpenGL Version: '+PChar(glGetString(GL_VERSION)));
       MemoGLInfo.Lines.Add('OpenGL Renderer: '+OpenGLRenderer);
       MemoGLInfo.Lines.Add('GLU Version: '+PChar(gluGetString(GLU_VERSION)));
       MemoGLInfo.Lines.Add(CPUID);
       MemoGLInfo.Lines.Add('');

       AddInteger('GL_MAX_LIST_NESTING',GL_MAX_LIST_NESTING);
       AddInteger('GL_MAX_LIGHTS',GL_MAX_LIGHTS);
       AddInteger('GL_MAX_TEXTURE_SIZE',GL_MAX_TEXTURE_SIZE);
       AddInteger('GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB',GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB);
       AddFloats('GL_LINE_WIDTH_RANGE',GL_LINE_WIDTH_RANGE);
       AddFloat('MAX_TEXTURE_LOD_BIAS_EXT',GL_MAX_TEXTURE_LOD_BIAS_EXT);
       AddInteger('GL_AUX_BUFFERS',GL_AUX_BUFFERS);
       AddInteger('GL_MAX_CLIP_PLANES',GL_MAX_CLIP_PLANES);

       TextureInternalFormat;

       // INVALID_ENUM GL_ERROR is raised after doing this:
       //    AddInteger('GL_MAX_ELEMENTS_VERTICES',GL_MAX_ELEMENTS_VERTICES);
       //    AddInteger('GL_MAX_ELEMENTS_VERTICES',GL_MAX_ELEMENTS_INDICES);

       UDPixelFormat.Position:=0;
       UDPixelFormat.Max:=MaxPixelFormats;
       LPixelFormatCount.Caption:=IntToStr(UDPixelFormat.Max);
       MemoGLInfo.Lines.Add('Max num of pixel formats: '+LPixelFormatCount.Caption);

       UDPixelFormat.Position:=GetPixelFormat(CurrentMaker.Canvas.Handle);
     end;
  end;

begin
  inherited;

  if PageControl2.ActivePage=TabGLExtensions then
     ShowExtensions
  else
  if PageControl2.ActivePage=TabAdvanced then
     LTotalMemory.Caption:=IntToStr(TTeeCPU.TotalMemoryAllocated);
end;

procedure TMakerEditor1.Edit1Change(Sender: TObject);

  function GLBoolToStr(Value:GLint):String;
  begin
    if Value=GL_TRUE then
       result:='True'
    else
       result:='False';
  end;

  function GLAccelerationToStr(Value:Integer):String;
  begin
    if Value=WGL_NO_ACCELERATION_ARB then
       result:='NO'
    else
    if Value=WGL_GENERIC_ACCELERATION_ARB then
       result:='GENERIC'
    else
    if Value=WGL_FULL_ACCELERATION_ARB then
       result:='FULL'
    else
       result:='Unknown';
  end;

var
  PFDescriptor : TPixelFormatDescriptor;

  procedure Check(AConst:Cardinal; const AText:String);
  begin
    if (PFDescriptor.dwFlags and AConst)<>0 then
       MemoPixelFormat.Lines.Add(AText);
  end;

var tmp : HDC;
    Value : Integer;
begin
  if PageMakers.ActivePageIndex=-1 then Exit;

  tmp:=CurrentMaker.Canvas.Handle;

  if OpenGL2.DescribePixelFormat(tmp,UDPixelFormat.Position,SizeOf(PFDescriptor),PFDescriptor) then
  with MemoPixelFormat.Lines do
  begin
    BeginUpdate;

    Clear;

    Check(PFD_SUPPORT_OPENGL,'PFD_SUPPORT_OPENGL');
    Check(PFD_SUPPORT_GDI,'PFD_SUPPORT_GDI');
    Check(PFD_DOUBLEBUFFER,'PFD_DOUBLEBUFFER');
    Check(PFD_STEREO,'PFD_STEREO');
    Check(PFD_DRAW_TO_WINDOW,'PFD_DRAW_TO_WINDOW');
    Check(PFD_DRAW_TO_BITMAP,'PFD_DRAW_TO_BITMAP');
    Check(PFD_GENERIC_ACCELERATED,'PFD_GENERIC_ACCELERATED');
    Check(PFD_GENERIC_FORMAT,'PFD_GENERIC_FORMAT');
    Check(PFD_NEED_PALETTE,'PFD_NEED_PALETTE');
    Check(PFD_NEED_SYSTEM_PALETTE,'PFD_NEED_SYSTEM_PALETTE');
    Check(PFD_SWAP_EXCHANGE,'PFD_SWAP_EXCHANGE');
    Check(PFD_SWAP_COPY,'PFD_SWAP_COPY');
    Check(PFD_SWAP_LAYER_BUFFERS,'PFD_SWAP_LAYER_BUFFERS');
    Check(PFD_SUPPORT_COMPOSITION,'PFD_SUPPORT_COMPOSITION');

    with PFDescriptor do
    begin
      if (iLayerType=PFD_MAIN_PLANE) then
         Add('PFD_MAIN_PLANE')
      else
      if (iLayerType=PFD_OVERLAY_PLANE) then
         Add('PFD_OVERLAY_PLANE')
      else
      if (iLayerType=Byte(PFD_UNDERLAY_PLANE)) then
         Add('PFD_UNDERLAY_PLANE')
      else
         Add('LayerType: '+IntToStr(iLayerType));

      if (iPixelType=PFD_TYPE_RGBA) then
         Add('PFD_TYPE_RGBA')
      else
      if (iPixelType=PFD_TYPE_COLORINDEX) then
         Add('PFD_TYPE_COLORINDEX')
      else
         Add('PixelType: '+IntToStr(iPixelType));

      Add('Version: '+IntToStr(nVersion));
      Add('ColorBits: '+IntToStr(cColorBits));
      Add('AlphaBits: '+IntToStr(cAlphaBits));
      Add('AccumBits: '+IntToStr(cAccumBits));
      Add('AccumAlphaBits: '+IntToStr(cAccumAlphaBits));
      Add('DepthBits: '+IntToStr(cDepthBits));
      Add('StencilBits: '+IntToStr(cStencilBits));
      Add('AuxBuffers: '+IntToStr(cAuxBuffers));

    end;

    EndUpdate;
  end
  else
  begin
    with MemoPixelFormat.Lines do
    begin
      Clear;
      Add('Unknown PixelFormat: '+IntToStr(UDPixelFormat.Position));
    end;
  end;

  with MemoPixelFormat.Lines do
  begin
    Add('');

    if GetPixelAttribute(WGL_SUPPORT_OPENGL_ARB,Value) then
       Add('WGL_SUPPORT_OPENGL_ARB = '+GLBoolToStr(Value));

    if GetPixelAttribute(WGL_DRAW_TO_WINDOW_ARB,Value) then
       Add('WGL_DRAW_TO_WINDOW_ARB = '+GLBoolToStr(Value));

    if GetPixelAttribute(WGL_DOUBLE_BUFFER_ARB,Value) then
       Add('WGL_DOUBLE_BUFFER_ARB = '+GLBoolToStr(Value));

    if GetPixelAttribute(WGL_SAMPLE_BUFFERS_ARB,Value) then
       Add('WGL_SAMPLE_BUFFERS_ARB = '+GLBoolToStr(Value));

    if GetPixelAttribute(WGL_SAMPLES_ARB,Value) then
       Add('WGL_SAMPLES_ARB = '+IntToStr(Value));

    if GetPixelAttribute(WGL_ACCELERATION_ARB,Value) then
       Add('WGL_ACCELERATION_ARB = '+GLAccelerationToStr(Value));

    if GetPixelAttribute(WGL_COLOR_BITS_ARB,Value) then
       Add('WGL_COLOR_BITS_ARB = '+IntToStr(Value));

    if GetPixelAttribute(WGL_DEPTH_BITS_ARB,Value) then
       Add('WGL_DEPTH_BITS_ARB = '+IntToStr(Value));

    if GetPixelAttribute(WGL_ALPHA_BITS_ARB,Value) then
       Add('WGL_ALPHA_BITS_ARB = '+IntToStr(Value));

    if GetPixelAttribute(WGL_STENCIL_BITS_ARB,Value) then
       Add('WGL_STENCIL_BITS_ARB = '+IntToStr(Value));

    if GetPixelAttribute(WGL_PIXEL_TYPE_ARB,Value) then
       if Value=WGL_TYPE_RGBA_ARB then
          Add('WGL_PIXEL_TYPE_ARB = WGL_TYPE_RGBA_ARB')
       else
       if Value=WGL_TYPE_COLORINDEX_ARB then
          Add('WGL_PIXEL_TYPE_ARB = WGL_TYPE_COLORINDEX_ARB');

  end;
end;

procedure TMakerEditor1.Button3Click(Sender: TObject);
var PixelFormat : Integer;
    PFDescriptor : TPixelFormatDescriptor;
    tmp : TTeeCanvasHandle;
    HRC : HGLRC;
begin
  PixelFormat:=UDPixelFormat.Position;

  tmp:=CurrentMaker.Canvas.Handle;

  FillDescriptor(tmp,[opDoubleBuffered],32,1,PFDescriptor);

  if OpenGL2.DescribePixelFormat(tmp,PixelFormat,SizeOf(PFDescriptor),PFDescriptor) then
     if OpenGL2.SetPixelFormat(tmp,PixelFormat,@PFDescriptor) then
     begin

       HRC:=OpenGL2.wglCreateContext(tmp);
       ActivateRenderingContext(tmp,HRC);
     end
     else
     begin
       ShowMessage('Error SetPixelFormat: '+IntToStr(GetLastError));
     end;
end;

Procedure TrimWorkingSet;
{$IFNDEF LINUX}
var MainHandle : THandle;
{$ENDIF}
begin  { reduce allocated memory at startup }
  {$IFNDEF LINUX}
  MainHandle:=OpenProcess(PROCESS_ALL_ACCESS,False,GetCurrentProcessID);
  SetProcessWorkingSetSize(MainHandle,$ffffffff,$ffffffff);
  CloseHandle(MainHandle);
  {$ENDIF}
end;

type
  TBlocksAccess=class(TBlocks);

procedure TMakerEditor1.BCompileShaderClick(Sender: TObject);

  procedure TryCompile(Shader:TShader);
  begin
    try
      Shader.CompileAndAttach;
    except
      on Exception do
      begin
        MemoShaderLog.Lines.Clear;
        MemoShaderLog.Lines.Add('Compile: '+#13#10+Shader.ErrorLog);

        PageControl3.ActivePage:=TabShaderDebug;
        raise;
      end;
    end;
  end;

begin
  if not Assigned(glCreateProgram) then
     raise Exception.Create('OpenGL Shaders not supported.');

//  Assert(TGLCanvas(CurrentMaker.Canvas).CheckGLError,'Before CreateProgram: '+IntToStr(TGLCanvasAccess(CurrentMaker.Canvas).ISavedError));

  with TBlocksAccess(CurrentMaker.Blocks).ProgramShader do
  begin
    Vertex.Source:=MemoVertexShader.Text;
    TryCompile(Vertex);

    Fragment.Source:=MemoFragmentShader.Text;
    TryCompile(Fragment);

    try
      Link;
    except
      on Exception do
      begin
        MemoShaderLog.Lines.Clear;
        MemoShaderLog.Lines.Add('Link:'+#13#10+ErrorLog);

        PageControl3.ActivePage:=TabShaderDebug;
        raise;
      end;
    end;
  end;

  BCompileShader.Enabled:=False;
  CBShaderActive.Enabled:=True;
end;

procedure TMakerEditor1.CBShaderActiveClick(Sender: TObject);
begin
  TBlocksAccess(CurrentMaker.Blocks).ShaderEnabled:=CBShaderActive.Checked;
end;

procedure TMakerEditor1.MemoVertexShaderChange(Sender: TObject);
begin
  BCompileShader.Enabled:=True;
end;

procedure TMakerEditor1.ComboShadersChange(Sender: TObject);
begin
  case ComboShaders.ItemIndex of
    0: begin
         MemoVertexShader.Text:=MemoDefaultVertex.Text;
         MemoFragmentShader.Text:=MemoDefaultFragment.Text
       end;
  end;
end;

procedure TMakerEditor1.PageControl1Change(Sender: TObject);
begin
  inherited;

  if PageControl1.ActivePage=TabProgram then
     ComboShadersChange(Self);
end;

procedure TMakerEditor1.ActivateMaker(const AMaker: TMaker);
begin
  inherited;
  
  with TBlocksAccess(CurrentMaker.Blocks).ProgramShader do
  begin
    MemoVertexShader.Text:=Vertex.Source;
    MemoFragmentShader.Text:=Fragment.Source;
    CBShaderActive.Checked:=Enabled;
  end;
end;

procedure TMakerEditor1.DepthBufferToBitmap(const tmpBitmap:TBitmap);
type
  TSingleArray=Array[0..0] of Single;

var x,y    : Integer;
    tmpMin : Single;
    tmpMax : Single;
    tmpZ   : ^TSingleArray;
    tmpLines : TRGBArray;
    tmpVal : Byte;
    tmp    : Single;
    tmpRange: Single;
    tmpW,
    tmpH   : Integer;
begin
  tmpW:=tmpBitmap.Width;
  tmpH:=tmpBitmap.Height;

  GetMem(tmpZ,tmpW*tmpH*SizeOf(Single));
  try
    glReadPixels(0,0,tmpW,tmpH,GL_DEPTH_COMPONENT,GL_FLOAT,tmpZ);
    Assert(CurrentMaker.Render.Canvas.CheckGLError,'ReadPixels: '+IntToStr(TGLCanvasAccess(CurrentMaker.Render.Canvas).ISavedError));

    tmpMin:=1;
    tmpMax:=0;

    for x:=0 to tmpW-1 do
        for y:=0 to tmpH-1 do
        begin
          tmp:=tmpZ^[y*tmpW+x];

          if tmp<tmpMin then tmpMin:=tmp;
          if tmp>tmpMax then tmpMax:=tmp;
        end;

    tmpRange:=255.0/(tmpMax-tmpMin);

    TeeCalcLines(tmpLines,tmpBitmap);

    for x:=0 to tmpW-1 do
        for y:=0 to tmpH-1 do
        begin
          tmpVal:=255-Round(tmpRange*(tmpZ^[y*tmpW+x]-tmpMin));

          with tmpLines[tmpH-y-1,x] do
          begin
            Red:=tmpVal;
            Green:=tmpVal;
            Blue:=tmpVal;
          end;
        end;

    tmpLines:=nil;
  finally
    FreeMem(tmpZ);
  end;
end;

type
  TMakerAccess=class(TMaker);

procedure TMakerEditor1.Button4Click(Sender: TObject);
var tmpDepth : TBitmap;
    //tmpColor : TBitmap;
begin
  tmpDepth:=TBitmap.Create;
  try
    tmpDepth.PixelFormat:=pf24bit;

    with CurrentMaker do
         TeeSetBitmapSize(tmpDepth,Width,Height);

    TGLCanvasAccess(CurrentMaker.Render.Canvas).CheckContext;

    TMakerAccess(CurrentMaker).InternalDraw(CurrentMaker.ClientRect);
    
    glReadBuffer(GL_BACK);
    DepthBufferToBitmap(tmpDepth);

    {
    tmpColor:=TBitmap.Create;
    try
      TGLCanvasAccess(CurrentMaker.Render.Canvas).BufferToBitmap(tmpColor,CurrentMaker.ClientRect);
      TTextureSelector.ModalShow(Self,tmpColor);
    finally
      tmpColor.Free;
    end;
    }

    TTextureSelector.ModalShow(Self,tmpDepth);
  finally
    tmpDepth.Free;
  end;
end;

procedure TMakerEditor1.Maker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
var P : TPoint3DFloat;
begin
  inherited;

  if CBMouseXYZ.Checked and EditMode1.Checked then
  begin
    P:=CurrentMaker.Options.Navigate.MouseToLocation(X,Y);
    StatusBar1.SimpleText:=Format('XY: %d %d  XYZ: %.3f %.3f %.3f',[X,Y,P.X,P.Y,P.Z]);
  end;
end;

end.
