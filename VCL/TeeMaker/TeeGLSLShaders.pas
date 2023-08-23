unit TeeGLSLShaders;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OpenGL2;

type
  TProgramShader=class;

  TShader=class
  private
    FHandle  : GLuint;
    FSource  : String;

    IProgram : TProgramShader;
    IType    : GLuint;

    function GetHandle:GLuint;
    procedure SetSource(const Value: String);
  public
    Constructor Create(ShaderType:GLuint);
    Destructor Destroy; override;

    procedure CompileAndAttach;
    function ErrorLog:String;

    property Handle:GLuint read GetHandle;
    property Source:String read FSource write SetSource;
  end;

  TProgramShader=class
  private
    FEnabled : Boolean;

    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  public
    Handle   : GLuint;
    Vertex   : TShader;
    Fragment : TShader;

    Constructor Create;
    Destructor Destroy; override;

    function ErrorLog:String;
    procedure Link;
    procedure SetUniform(const Uniform:String; const Value:Integer);

    property Enabled:Boolean read GetEnabled write SetEnabled default False;
  end;

  TGLSLEditor = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GLSLEditor: TGLSLEditor;

implementation

{$R *.dfm}

Constructor TProgramShader.Create;
begin
  inherited;

  Vertex:=TShader.Create(GL_VERTEX_SHADER);
  Vertex.IProgram:=Self;

  Fragment:=TShader.Create(GL_FRAGMENT_SHADER);
  Fragment.IProgram:=Self;
end;

Destructor TProgramShader.Destroy;
begin
  if Enabled then
     glUseProgram(0);

  Fragment.Free;
  Vertex.Free;

  glDeleteProgram(Handle);
  inherited;
end;

procedure TProgramShader.Link;
var tmp : Integer;
begin
  glLinkProgram(Handle);

  glGetProgramiv(Handle,GL_LINK_STATUS,@tmp);

  if tmp<>GL_TRUE then
     raise Exception.Create('ProgramShader Link failed.');
end;

function TProgramShader.ErrorLog:String;
var tmpLength : Integer;
    tmpS      : AnsiString;
    tmpWritten : Integer;
begin
  glGetProgramiv(Handle, GL_INFO_LOG_LENGTH, @tmpLength);

  if tmpLength > 0 then
  begin
    SetLength(tmpS,tmpLength);
    tmpWritten:=0;

    glGetProgramInfoLog(Handle, tmpLength, tmpWritten, PAnsiChar(tmpS));
    result:=String(tmpS);
  end
  else
    result:='';
end;

{ TShader }

Constructor TShader.Create(ShaderType:GLuint);
begin
  inherited Create;
  IType:=ShaderType;
end;

Destructor TShader.Destroy;
begin
  if Handle<>0 then
     glDeleteShader(Handle);

  inherited;
end;

function TShader.GetHandle:GLuint;
begin
  if IProgram.Handle=0 then
     IProgram.Handle:=glCreateProgram;

  if FHandle=0 then
     FHandle:=glCreateShader(IType);

  result:=FHandle;
end;

procedure TShader.CompileAndAttach;
var tmp : Integer;
begin
  glCompileShader(Handle);

  glGetShaderiv(Handle,GL_COMPILE_STATUS,@tmp);

  if tmp<>GL_TRUE then
     raise Exception.Create('Shader compilation failed.');

  glAttachShader(IProgram.Handle,Handle);
end;

function TShader.ErrorLog:String;
var tmpLength : Integer;
    tmpS      : AnsiString;
    tmpWritten : Integer;
begin
  glGetShaderiv(Handle, GL_INFO_LOG_LENGTH, @tmpLength);

  if tmpLength > 0 then
  begin
    SetLength(tmpS,tmpLength);
    tmpWritten:=0;

    glGetShaderInfoLog(Handle, tmpLength, tmpWritten, PAnsiChar(tmpS));
    result:=String(tmpS);
  end
  else
    result:='';
end;

procedure TShader.SetSource(const Value: String);
var tmp : PAnsiChar;
begin
  if FSource<>Value then
  begin
    FSource:=Value;
    tmp:=PAnsiChar(AnsiString(FSource));
    glShaderSource(Handle,1,@tmp,nil);
  end;
end;

function TProgramShader.GetEnabled: Boolean;
begin
  result:=FEnabled;  // gl_IsProgramEnabled?
end;

procedure TProgramShader.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;

    if FEnabled then
       glUseProgram(Handle)
    else
       glUseProgram(0);
  end;
end;

procedure TProgramShader.SetUniform(const Uniform: String; const Value: Integer);
var tmpLocation : GLInt;
begin
  tmpLocation:=glGetUniformLocation(Handle, PAnsiChar(AnsiString(Uniform)));

  if tmpLocation<>-1 then
     glUniform1i(tmpLocation,Value);
end;

end.
