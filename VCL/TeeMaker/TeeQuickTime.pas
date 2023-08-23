unit TeeQuickTime;

interface

implementation

const
  qtmlClient='qtmlClient.dll';

type
  OSErr=Smallint;

function InitializeQTML(flag:longint):OSErr; far; cdecl; external qtmlClient;
procedure TerminateQTML; cdecl; far; external qtmlClient;

function NewMovieFromFile(var theMovie:Movie; resRefNum:short; resId:shortPtr;
         resName:StringPtr; newMovieFlags:short; dataRefWasChanged:BooleanPtr):OSErr; cdecl; external qtmlClient';

function OpenMovieFile({const} var fileSpec:FSSpec;var resRefNum:short;
                       permission:SInt8):OSErr; cdecl; external qtmlClient;

function CloseMovieFile(resRefNum:short):OSErr; cdecl; external qtmlClient;

const
  kInitializeQTMLNoSoundFlag	                  = 1; (* flag for requesting no sound when calling InitializeQTML *)
  kInitializeQTMLUseGDIFlag	                    = 2; (* flag for requesting GDI when calling InitializeQTML *)
  kInitializeQTMLDisableDirectSound             = 4; (* disables QTML's use of DirectSound *)
  kInitializeQTMLUseExclusiveFullScreenModeFlag = 8; (* later than QTML 3.0: qtml starts up in exclusive full screen mode *)
  kInitializeQTMLDisableDDClippers              = 16; (* flag for requesting QTML not to use DirectDraw clipper objects; QTML 5.0 and later *)

var
  IsInit : Boolean=False;

procedure InitQuickTime;
begin
  if not IsInit then
  begin
    InitializeQTML(0);
    IsInit:=True;
  end;
end;

initialization
finalization
  if IsInit then
     TerminateQTML;
end.
