unit TeePlayMP3;
{$I TeeDefs.inc}

interface

uses
  Classes, Forms,
  {$IFDEF TEEBASS}
  Bass,
  {$ENDIF}
  TeeAnimate;

type
  TPlayMP3Sound=class(TPlaySoundAnimation)
  private
    {$IFDEF TEEBASS}
    IStream : HSTREAM;

    procedure FreeStream;
    class procedure InitBASS;
    {$ENDIF}

    function GetVolume: Integer;
    procedure SetVolume(const Value: Integer);
  protected
    procedure SetFile(const Value: String); override;
  public
    {$IFDEF TEEBASS}
    Destructor Destroy; override;
    {$ENDIF}

    Procedure Play; override;
    class procedure PlayFile(const AFileName:String);
    property Volume:Integer read GetVolume write SetVolume default 100;
  end;

implementation

uses
  SysUtils, MMSystem, TeeProcs, TeeBlocks;

const
  MP3Extension='.MP3';
  RAMExtension='.RAM';

type
  BASSException=class(Exception);

{ TPlayMP3Sound }

{$IFDEF TEEBASS}
Destructor TPlayMP3Sound.Destroy;
begin
  FreeStream;
  inherited;
end;

class procedure TPlayMP3Sound.InitBASS;
var tmp : Integer;
begin
  if not BASS_Init(-1, 44100, 0, Application.Handle, nil) then
  begin
    tmp:=BASS_ErrorGetCode;

    if tmp<>BASS_ERROR_ALREADY then
       raise BASSException.Create('Error initializing BASS dll (audio): '+IntToStr(tmp));
  end;
end;
{$ENDIF}

procedure TPlayMP3Sound.Play;
{$IFDEF TEEBASS}
var tmpAnsi   : AnsiString;
    tmpError  : String;
    tmpCode   : Integer;
    tmpSt     : TStrings;
    tmpStream : TStream;
{$ENDIF}
var tmp       : String;
begin
  if FileName<>'' then
  begin
    tmp:=CheckSoundLibrary(FileName);

    {$IFDEF TEEBASS}
    if IStream=0 then
    begin
      InitBASS;

      if UpperCase(ExtractFileExt(FileName))=RAMExtension then
      begin
        tmpSt:=TStringList.Create;
        try
          if TeeIsURL(tmp) then
          begin
            tmpStream:=TBlocks.LoadURLStream(tmp,tmpError);

            if Assigned(tmpStream) then
            try
              tmpSt.LoadFromStream(tmpStream);
            finally
              tmpStream.Free;
            end;
          end
          else
            tmpSt.LoadFromFile(tmp);

          tmp:=tmpSt.Text;
        finally
          tmpSt.Free;
        end;
      end;

      if TeeIsURL(tmp) then
         IStream:=BASS_StreamCreateURL(PChar(tmp), 0, 0, nil, 0)
      else
      begin
        tmpAnsi:=tmp;
        IStream:=BASS_StreamCreateFile(False, PAnsiChar(tmpAnsi), 0, 0, 0);
      end;
    end;

    if not BASS_ChannelPlay(IStream, False) then
    begin
       tmpCode:=BASS_ErrorGetCode;
       raise BASSException.Create('Error '+IntToStr(tmpCode)+' playing stream: '+FileName);
    end;
    {$ELSE}
    TPlaySoundAnimation.Play(tmp,False);
    {$ENDIF}
  end;
end;

procedure TPlayMP3Sound.SetFile(const Value: String);
begin
  inherited;

  {$IFDEF TEEBASS}
  FreeStream;
  {$ENDIF}
end;

procedure TPlayMP3Sound.SetVolume(const Value: Integer);
begin
  {$IFDEF TEEBASS}
  InitBASS;
  BASS_SetVolume(Value);
  {$ELSE}
  waveOutSetVolume(0,(Value shl 8)+Value);
  {$ENDIF}
end;

{$IFDEF TEEBASS}
procedure TPlayMP3Sound.FreeStream;
begin
  if IStream<>0 then
  begin
    BASS_StreamFree(IStream);
    IStream:=0;
  end;
end;
{$ENDIF}

function TPlayMP3Sound.GetVolume: Integer;
begin
  {$IFDEF TEEBASS}
  InitBASS;
  result:=BASS_GetVolume;
  {$ELSE}
  waveOutGetVolume(0,@result);
  {$ENDIF}
end;

var
  ISharedPlay : TPlayMP3Sound=nil;

type
  TPlaySoundClass=class of TPlaySoundAnimation;

class procedure TPlayMP3Sound.PlayFile(const AFileName:String);
var tmpExt : String;
begin
  tmpExt:=UpperCase(ExtractFileExt(AFileName));

  if (tmpExt=MP3Extension) or (tmpExt=RAMExtension) then
  begin
    if not Assigned(ISharedPlay) then
       ISharedPlay:=TPlayMP3Sound.Create(nil);

    ISharedPlay.FileName:=AFileName;
    ISharedPlay.Play;
  end
  else
  with TPlaySoundAnimation.Create(nil) do
  try
    FileName:=AFileName;
    Play;
  finally
    Free;
  end;
end;

initialization
  TeeRegisterAnimation(TPlayMP3Sound);
finalization
  ISharedPlay.Free;
  {$IFDEF TEEBASS}
  BASS_Free;
  {$ENDIF}
end.
