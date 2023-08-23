unit TeeLoadBlock;
{$I TeeDefs.inc}

interface

uses
  Windows, Messages,
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QButtons, QStdCtrls, QExtCtrls,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls,
  {$ENDIF}
  TeCanvas, TeeProcs;

type
  TLoadBlockDialog = class(TForm)
    RBURL: TRadioButton;
    Label1: TLabel;
    CBURL: TComboFlat;
    RBFile: TRadioButton;
    Label2: TLabel;
    CBFile: TComboFlat;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    PanelButtons: TPanel;
    BOK: TButton;
    Button2: TButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure RBFileClick(Sender: TObject);
    procedure CBFileEnter(Sender: TObject);
    procedure CBURLEnter(Sender: TObject);
    procedure CBURLChange(Sender: TObject);
    procedure RBURLClick(Sender: TObject);
    procedure CBFileChange(Sender: TObject);
  private
    { Private declarations }
    procedure AddFile(const AFile:String);
  public
    { Public declarations }

    function GetSelectedText:String;
    class Function ModalShow(AOwner:TComponent;
                             const DefaultFilter,DefaultExtension:String;
                             var AFileName:String):Boolean;
  end;

implementation

{$IFNDEF CLX}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

uses
  TeePenDlg, TeeURL, TeeBlocks, Registry;

function TLoadBlockDialog.GetSelectedText:String;
begin
  if RBFile.Checked then
     result:=CBFile.Text
  else
  if RBURL.Checked then
     result:=CBURL.Text
  else
     result:='';
end;

class Function TLoadBlockDialog.ModalShow(AOwner:TComponent;
                        const DefaultFilter,DefaultExtension:String;
                        var AFileName:String):Boolean;

  procedure LoadOpened(const APrefix:String; AList:TStrings);
  var tmpList : TStrings;
      t       : Integer;
  begin
    with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;

      if OpenKeyReadOnly(TeeMakerKey+'\LoadBlock\'+APrefix) then
      begin
        tmpList:=TStringList.Create;
        try
          GetValueNames(tmpList);

          for t:=tmpList.Count-1 downto 0 do
              {$IFDEF D7}
              AList.Add(tmpList.ValueFromIndex[t]);
              {$ELSE}
              AList.Add(Copy(tmpList[t], Length(tmpList.Names[t])+2, 255));
              {$ENDIF}
        finally
          tmpList.Free;
        end;

        CloseKey;
      end;
    finally
      Free;
    end;
  end;

  procedure SaveOpened(const APrefix:String; AList:TStrings);
  var t : Integer;
  begin
    with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;

      if OpenKey(TeeMakerKey+'\LoadBlock\'+APrefix,True) then
      begin
        for t:=AList.Count-1 downto 0 do
            WriteString(IntToStr(t),AList[t]);

        CloseKey;
      end;
    finally
      Free;
    end;
  end;

var tmp : TLoadBlockDialog;
begin
  tmp:=TLoadBlockDialog.Create(AOwner);
  with tmp do
  try
    TTeeVCL.PositionToCenter(tmp);

    OpenDialog1.Filter:=DefaultFilter;
    OpenDialog1.DefaultExt:=DefaultExtension;

    LoadOpened('Object.File',CBFile.Items);
    LoadOpened('Object.URL',CBURL.Items);

    if AFileName<>'' then
    begin
      if TeeIsURL(AFileName) then
      begin
        RBURL.Checked:=True;
        CBURL.Text:=AFileName;
      end
      else
      begin
        RBFile.Checked:=True;
        CBFile.Text:=AFileName;
      end;
    end;

    result:=ShowModal=mrOk;

    if result then
    begin
      SaveOpened('Object.File',CBFile.Items);
      SaveOpened('Object.URL',CBURL.Items);

      AFileName:=GetSelectedText;
    end;
  finally
    Free;
  end;
end;

function HasItem(AList:TStrings; AText:String):Boolean;
var t : Integer;
begin
  result:=False;

  AText:=UpperCase(AText);
  
  for t:=0 to AList.Count-1 do
      if UpperCase(AList[t])=AText then
      begin
        result:=True;
        break;
      end;
end;

procedure TLoadBlockDialog.AddFile(const AFile:String);
begin
  if not HasItem(CBFile.Items,AFile) then
     CBFile.Items.Add(AFile);
end;

procedure TLoadBlockDialog.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    RBFile.Checked:=True;
    AddFile(OpenDialog1.FileName);

    CBFile.Text:=OpenDialog1.FileName;

    CBFileChange(Self);
  end;
end;

procedure TLoadBlockDialog.RBFileClick(Sender: TObject);
begin
  CBFile.SetFocus;
end;

procedure TLoadBlockDialog.CBFileEnter(Sender: TObject);
begin
  RBFile.Checked:=True;
end;

procedure TLoadBlockDialog.CBURLEnter(Sender: TObject);
begin
  RBURL.Checked:=True;
end;

procedure TLoadBlockDialog.CBURLChange(Sender: TObject);
begin
  BOK.Enabled:=(CBURL.Text<>'') and (UpperCase(CBURL.Text)<>'HTTP://');
end;

procedure TLoadBlockDialog.RBURLClick(Sender: TObject);
begin
  CBURL.SetFocus;
end;

procedure TLoadBlockDialog.CBFileChange(Sender: TObject);
begin
  BOK.Enabled:=CBFile.Text<>'';
end;

end.
