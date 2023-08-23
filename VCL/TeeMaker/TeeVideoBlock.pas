unit TeeVideoBlock;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls, QStdCtrls, QButtons,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, Buttons,
  {$ENDIF}
  TeeBlocks, TeeAnimate, TeCanvas, TeeVideo;

type
  TVideoAnimation=class(TPropertyAnimation)
  private
    FFileName : String;
    FFilters  : TFilterItems;

    IEndFrame : Integer;
    IGetFrame : PGETFRAME;
    IStream   : PAVIStream;
    IPrevFrame: Integer;

    procedure Finish;
    function GetEnd: Integer;
    function GetStart: Integer;
    procedure Open;
  protected
    procedure GetFrame(FrameNumber: Integer; var b:TBitmap); overload;
    procedure NextFrame(const Fraction:Single); override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function IsEnabled: Boolean; override;
    procedure Play; override;
    procedure Stop; override;

    function Picture:TPicture;
    function Filters:TFilterItems;
  published
    property FileName:String read FFileName write FFileName;
    property PropertyName;
  end;

  TVideoEditor = class(TForm)
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    EName: TEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure ENameChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Player     : TVideoAnimation;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  {$IFDEF CLX}
  Qt,
  {$ENDIF}
  TeeFiltersEditor;

procedure TVideoEditor.ENameChange(Sender: TObject);
begin
  Player.FileName:=EName.Text;
end;

procedure TVideoEditor.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog1.FileName:=EName.Text;

  if OpenDialog1.Execute then
     EName.Text:=OpenDialog1.FileName;
end;

procedure TVideoEditor.Button1Click(Sender: TObject);
begin
  TFiltersEditor.ShowEditor(Self,Player.Picture.Graphic,Player.Filters);
end;

procedure TVideoEditor.FormShow(Sender: TObject);
begin
  Player:=TVideoAnimation(Tag);

  if Assigned(Player) then
  with Player do
       EName.Text:=FileName;
end;

procedure TVideoEditor.FormCreate(Sender: TObject);
begin
  OpenDialog1.Filter:='AVI video files'+'|*.avi|'+'Mpeg video files'+'|*.mpg';
end;

{ TVideoAnimation }

Constructor TVideoAnimation.Create(AOwner:TComponent);
begin
  inherited;
  InitVideoForWindows;
  FFilters:=TFilterItems.Create(Self,TTeeFilter);
end;

function TVideoAnimation.IsEnabled: Boolean;
begin
  result:=inherited IsEnabled and (FileName<>'');
end;

procedure TVideoAnimation.NextFrame(const Fraction:Single);
var tmpFrame : Double;
    tmp      : TPicture;
    b        : TBitmap;
    tmpNext   :Integer;
begin
  tmpFrame:=Fraction;

  tmpNext:=Round(IEndFrame*tmpFrame);

  if tmpNext>IPrevFrame then
  begin
    tmp:=Picture;

    // Copy video frame to IBitmap
    b:=TBitmap(tmp.Bitmap);

    try
      GetFrame(tmpNext,b);
    except
      on Exception do
      begin
        Stop;
        raise;
      end;
    end;

    TCustomBlock(Instance).Parent.RemoveTexture(b);

    FFilters.ApplyTo(b);

    TCustomBlock(Instance).Repaint;

    IPrevFrame:=tmpNext;
  end;

  inherited;
end;

procedure TVideoAnimation.Open;
var
  P : PBitmapInfoHeader;
  BitmapInfoHeader : TBitmapInfoHeader;
  StreamInfo       : TAviStreamInfoA;
  tmp              : HResult;

//  FileInfo         : TAVIFILEINFOA;
//  IFile            : Pointer;
begin
  AVIFileInit;

//  AviCheck(AVIFileOpen(IFile, PChar(FFileName), OF_READ or OF_SHARE_DENY_WRITE, nil));
//  AviCheck(AVIFileInfo(IFile, @FileInfo, SizeOf(TAVIFILEINFOA)));
//  AVIFileRelease(IFile);

  tmp:=AVIStreamOpenFromFile(IStream, PChar(FFileName), streamtypeVIDEO, 0, OF_READ or OF_SHARE_DENY_WRITE, nil);

  if tmp<>0 then
     AviCheck(AVIStreamOpenFromFile(IStream, PChar(FFileName),
              streamtypeIAVS, 0, OF_READ or OF_SHARE_DENY_WRITE, nil));

  AviCheck(AVIStreamInfo(IStream, StreamInfo, SizeOf(StreamInfo)));

  P:=@BitmapInfoHeader;

  ZeroMemory(P, SizeOf(BitmapInfoHeader));
  With BitmapInfoHeader do
  begin
    biSize:=SizeOf(TBitmapInfoHeader);
    biBitCount:=24;
    biClrImportant:=0;
    biClrUsed:=0;
    biCompression:=BI_RGB;

    //biHeight:=FileInfo.dwHeight;
    //biWidth:=FileInfo.dwWidth;

    biHeight:=streamInfo.rcFrame.Bottom - streamInfo.rcFrame.Top;
    biWidth:=streamInfo.rcFrame.Right - streamInfo.rcFrame.Left;

    biPlanes:=1;
    biXPelsPerMeter:=0;
    biYPelsPerMeter:=0;

    biSizeImage:=(((biWidth * (biBitCount div 8)) + (biBitCount div 8)) And $FFFC) * biHeight;
//   biSizeImage:=0;
  end;

  try
    IGetFrame:=AVIStreamGetFrameOpen(IStream, P);
  except
    on Exception do
       IGetFrame:=nil;
  end;

  if not Assigned(IGetFrame) then
  begin
    try
      IGetFrame:=AVIStreamGetFrameOpen(IStream, nil);
    except
      on Exception do
         IGetFrame:=nil;
    end;
  end;

  if not Assigned(IGetFrame) then
     Raise Exception.Create('Error GetFrameOpen AVI file stream');
end;

procedure TVideoAnimation.Finish;
begin
  if Assigned(IGetFrame) then
  begin
    AVIStreamGetFrameClose(IGetFrame);
    IGetFrame:=nil;
  end;

  if Assigned(IStream) then
  begin
    AVIStreamRelease(IStream);
    IStream:=nil;
  end;

  if Assigned(@AVIFileExit) then
     AVIFileExit;
end;

procedure TVideoAnimation.GetFrame(FrameNumber: Integer; var b:TBitmap);
var Info: PBITMAPINFOHEADER;
begin
  if Assigned(IGetFrame) then
  begin
    Info:=AVIStreamGetFrame(IGetFrame, FrameNumber);

    if Assigned(Info) then
    with b do
    begin
      Height:=Info.biHeight;
      Width:=Info.biWidth;

      SetDIBits( {$IFDEF CLX}QPainter_handle{$ENDIF}(Canvas.Handle),
                 {$IFDEF CLX}QPixmap_hbm{$ENDIF}(Handle),
                 0,Height,
                 Pointer({$IFDEF D16}NativeInt{$ELSE}Integer{$ENDIF}(Info) + SizeOf(TBITMAPINFOHEADER)),
                 PBITMAPINFO(Info)^, DIB_RGB_COLORS);

    end
    else
      raise Exception.Create('Error GetFrame AVI file stream');
  end;
end;

Destructor TVideoAnimation.Destroy;
begin
  FFilters.Free;
  Finish;
  inherited;
end;

function TVideoAnimation.GetEnd:Integer;
begin
  result:=GetStart;

  if Assigned(IStream) then
     Inc(result,AVIStreamLength(IStream)-1);
end;

function TVideoAnimation.GetStart:Integer;
begin
  if Assigned(IStream) then
     result:=AVIStreamStart(IStream)
  else
     result:=-1;
end;

procedure TVideoAnimation.Play;
begin
  inherited;

  if IPlaying<>asStopped then
  begin
    IPrevFrame:=-1;

    try
      Open;
      IEndFrame:=GetEnd;
    except
      on Exception do
      begin
        Animate.Stop;
        raise;
      end;
    end;
  end;
end;

procedure TVideoAnimation.Stop;
begin
  IPrevFrame:=-1;
  Finish;
  inherited;
end;

function TVideoAnimation.Picture:TPicture;
begin
  result:=TCustomBlock(Instance).Format.Texture.Picture;
end;

function TVideoAnimation.Filters:TFilterItems;
begin
  result:=FFilters;
end;

initialization
  TeeRegisterAnimation(TVideoAnimation);
end.
