unit Squarified_FileSystem;

interface

// Helper method to fill a Squarified map series with all files and subfolder
// of a given file system folder.

// eg:

// TFileSystem.FillTree('c:\myfolder', Series1)
uses
  TeeSquarifiedMap;

type
  TFileSystem=class
    // Fills the ASeries with all subfolders and files from Folder
    class procedure FillTree(const Folder:String; const ASeries:TSquarifiedMapSeries);

    // Returns the concatenated path for a given integer AItem index in the ASeries
    class function PathOf(const AItem:Integer; const ASeries:TSquarifiedMapSeries):String;

    // returns, eg: C:\Program Files (x86)\Embarcadero\Studio\22.0
    class function RADFolder:String;

    // returns, eg: c:\windows\system32
    class function WindowsFolder:String;
  end;

implementation

uses
  Windows,
  Registry,
  SysUtils;

const
  FolderSeparator='\';

// Just for demo purposes.
class function TFileSystem.RADFolder:String;
var Version : Integer;
begin
  result:='';

  Version:=32;

  with TRegistry.Create do
  try
    repeat
      if OpenKeyReadOnly('\SOFTWARE\Embarcadero\BDS\'+IntToStr(Version)+'.0') then
      begin
        result:=ReadString('RootDir');
        Exit;
      end
      else
         Dec(Version);

    until Version<7;
  finally
    Free;
  end;
end;

// Just for the demo. Returns: 'c:\windows\system32'
class function TFileSystem.WindowsFolder:String;
var L : Integer;
begin
  SetLength(result, MAX_PATH);
  L := GetSystemDirectory(PChar(result), MAX_PATH);
  SetLength(result, L);
end;

// Quick way to obtain a file size in bytes.
function GetFileSize(const APath:String):Int64;
var  I: TWin32FileAttributeData;
begin
  if GetFileAttributesEx(PChar(APath), GetFileExInfoStandard, PWin32FileAttributeData(@I)) then
     result := (I.nFileSizeHigh shl 32) + I.nFileSizeLow
  else
     result := -1;
end;

// Given a folder, recursively add all subfolder and all files into ACushion series
class procedure TFileSystem.FillTree(const Folder:String;
                                        const ASeries:TSquarifiedMapSeries);

  procedure DoFill(const AFolder:String; const AParent:Integer);
  var f : TSearchRec;

    function AddItem:Integer;
    var tmp : Int64;
    begin
      tmp:=GetFileSize(AFolder+'\'+f.Name);
      result:=ASeries.AddItem(AParent,tmp,f.Name);
    end;

    procedure AddChildren;
    var tmp : Integer;
    begin
      if FindFirst(AFolder+FolderSeparator+'*.*',faAnyFile,f)=0 then
      begin
        Repeat
          if (f.Attr and faDirectory)=faDirectory then
          begin
            if (f.Name='.') or (f.Name='..') then
            else
            begin
              tmp:=AddItem;

              DoFill(AFolder+FolderSeparator+f.Name,tmp);
            end;
          end
          else
            AddItem;

        Until FindNext(f)<>0;

        FindClose(f);
      end;
    end;

  begin
    AddChildren;
  end;

begin
  ASeries.BeginUpdate;
  try
    ASeries.Clear;

    ASeries.AddItem(-1,0,''{Root Folder});
    DoFill(Folder,0);

    ASeries.FinishData;
  finally
    ASeries.EndUpdate;
  end;
end;

class function TFileSystem.PathOf(const AItem: Integer;
  const ASeries: TSquarifiedMapSeries): String;
var tmp : Integer;
begin
  result:=ASeries.Labels[AItem];

  tmp:=AItem;

  repeat
    tmp:=ASeries.Items[tmp].Parent;

    if tmp<>-1 then
       result:=ASeries.Labels[tmp]+FolderSeparator+result;

  until tmp=-1;
end;

end.
