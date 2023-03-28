unit Series_SquarifiedFileSystem;
{$I TeeDefs.inc}

interface

uses
  {$IFNDEF LINUX}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  Types,
  Dialogs, StdCtrls, TeCanvas, TeeProcs, TeEngine, Chart,
  ExtCtrls, TeeSquarifiedMap, TeeEdit, Base, TeePenDlg;

type
  TSquarifiedFileSystem = class(TBaseForm)
    Panel3: TPanel;
    Label2: TLabel;
    CBMarks: TCheckBox;
    CBBorders: TCheckBox;
    ListBox1: TListBox;
    ComboViewMode: TComboFlat;
    CBColorBy: TComboFlat;
    Panel2: TPanel;
    LFile: TLabel;
    Panel4: TPanel;
    Button1: TButton;
    ChartEditor1: TChartEditor;
    EditFolder: TEdit;
    ButtonGo: TButton;
    Label1: TLabel;
    procedure EditFolderChange(Sender: TObject);
    procedure ButtonGoClick(Sender: TObject);
    procedure CBMarksClick(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ComboViewModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure CBColorByChange(Sender: TObject);
    procedure CBBordersClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Series1 : TSquarifiedMapSeries;

    procedure Fill_Level0_Items;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  TeeEditPro,
  Squarified_FileSystem;

procedure TSquarifiedFileSystem.EditFolderChange(Sender: TObject);
begin
  ButtonGo.Enabled := Trim(EditFolder.Text) <> '';
end;

procedure TSquarifiedFileSystem.ButtonGoClick(Sender: TObject);
var t1 : Int64;
begin
  Screen.Cursor:= crHourGlass;
  try
    t1:=GetTickCount;

    TFileSystem.FillTree(EditFolder.Text, Series1);

    Label1.Caption:= IntToStr(Series1.TotalCount(0))+' nodes.'+
                     ' Size: '+IntToStr(Round(Series1.Values[0]))+' bytes. '+
                     ' Time: '+IntToStr(GetTickCount-t1)+' msec.';

    Fill_Level0_Items;

    // Reset color-by mode if different than default:
    if CBColorBy.ItemIndex<>0 then
       CBColorByChange(Self);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSquarifiedFileSystem.CBMarksClick(Sender: TObject);
begin
  Series1.Marks.Visible:=CBMarks.Checked;
end;

// Show the path of the file under current mouse position
procedure TSquarifiedFileSystem.Chart1AfterDraw(Sender: TObject);
var t : Integer;
begin
  // Example:
  // Draw rectangle around top-level folders

  if CBBorders.Checked then
  if Series1.Count>0 then
  if not Series1.FlatMode then  // <-- only in hierarchy view mode
  begin
    Chart1.Canvas.Pen.Color:=clBlack;
    Chart1.Canvas.Pen.Width:=1;
    Chart1.Canvas.Brush.Style:=bsClear;

    for t:=0 to High(Series1.Items[0].Children) do
        Chart1.Canvas.Rectangle(Series1.RectangleOf(Series1.Items[0].Children[t]));
  end;
end;

procedure TSquarifiedFileSystem.Chart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var tmp : Integer;
begin
  tmp:=Series1.Clicked(X,Y);

  if tmp=TeeNoPointClicked then
     LFile.Caption:=''
  else
     LFile.Caption:=TFileSystem.PathOf(tmp,Series1)+' '+
                      IntToStr(Round(Series1.Values[tmp]))+' bytes';
end;

procedure TSquarifiedFileSystem.ComboViewModeChange(Sender: TObject);
begin
  Series1.FlatMode:=ComboViewMode.ItemIndex=1;

  // Refresh color-by file size colors:
  if CBColorBy.ItemIndex=1 then
     CBColorByChange(Self);
end;

procedure TSquarifiedFileSystem.FormCreate(Sender: TObject);

  procedure CreateChartAndSeries;
  begin
    Chart1.View3D := False;

    Series1 := TSquarifiedMapSeries.Create(Self);
    Series1.ParentChart := Chart1;

    Series1.Pen.Show; // show borders
    Series1.Marks.Hide;  // hide mark texts

    Series1.Selected.Hover.Pen.Color:=clWhite;  // when moving the mouse over
  end;

begin
  CreateChartAndSeries;

  EditFolder.Text := TFileSystem.RADFolder;

  if EditFolder.Text='' then
     EditFolder.Text:=TFileSystem.WindowsFolder
  else
     EditFolder.Text:=EditFolder.Text+'\source';
end;

procedure TSquarifiedFileSystem.ListBox1Click(Sender: TObject);
var tmp : Integer;
begin
  tmp:=ListBox1.ItemIndex;

  if tmp<>-1 then
     tmp:=Integer(ListBox1.Items.Objects[tmp]);

  // Set the selected children to display a rectangle around it
  Series1.Selected.HoverIndex:=tmp;
end;

// Example of several ways to color items, based on their file-size,
// or level, or file extension.
procedure TSquarifiedFileSystem.CBColorByChange(Sender: TObject);

  // Returns the color for a given "level"
  function LevelColorOf(const AIndex:Integer):TColor;
  var tmp : Integer;
  begin
    tmp:=Series1.ItemLevel(AIndex);
    result:=Chart1.GetDefaultColor(tmp);
  end;

  // Keep a list of extensions, each with a different color
  var Extensions : Array of String;

  function ExtensionIndex(const S:String):Integer;
  var t,L : Integer;
  begin
    L:=Length(Extensions);

    // Lookup extension:
    for t:=0 to L-1 do
        if Extensions[t]=S then
        begin
          result:=t;
          Exit;
        end;

    // If not found, add it:

    SetLength(Extensions,L+1);
    Extensions[L]:=S;

    result:=L;
  end;

  function ExtensionColorOf(const AIndex:Integer):TColor;
  var tmp : String;
  begin
    tmp:=TFileSystem.PathOf(AIndex,Series1);

    if tmp='' then
       result:=clTeeColor  // No extension
    else
    begin
      tmp:=UpperCase(ExtractFileExt(tmp));

      result:=Chart1.GetDefaultColor(ExtensionIndex(tmp));
    end;
  end;

  // Item colors are obtained from a palette calculated using all sibling
  // values, from min to max.
  procedure CalcColorsOf(const AIndex:Integer);
  var t : Integer;
      tmp : TItemChildren;
  begin
    tmp:=Series1.Items[AIndex].Children;

    if tmp=nil then
       Series1.ValueColor[AIndex]:=Series1.PaletteColorOf(Series1.Values[AIndex])
    else
    begin
      Series1.CalcPaletteMinMax(tmp);

      for t:=0 to High(tmp) do
          CalcColorsOf(tmp[t]);
    end;
  end;

  // Item colors are calculated using a palette constructed with min and max of
  // all non-folder items.
  procedure CalcFlatColors;
  var t : Integer;
  begin
    Series1.CalcPaletteMinMax;

    for t:=0 to Series1.Count-1 do
        if Series1.Items[t].Children=nil then
           Series1.ValueColor[t]:=Series1.PaletteColorOf(Series1.Values[t])
  end;

var t : Integer;
begin
  case CBColorBy.ItemIndex of
    0: // Default mode, reset colors

        for t:=0 to Series1.Count-1 do
            Series1.ValueColor[t]:=clTeeColor;

    1: // File size determines color, relative to parent or flat

        if Series1.Count>0 then
           if Series1.FlatMode then
              CalcFlatColors
           else
              CalcColorsOf(0);

    2: // Color by parent level

        for t:=0 to Series1.Count-1 do
            Series1.ValueColor[t]:=LevelColorOf(t);

    3: // Color by file extension

        for t:=0 to Series1.Count-1 do
            Series1.ValueColor[t]:=ExtensionColorOf(t);
  end;
end;

procedure TSquarifiedFileSystem.CBBordersClick(Sender: TObject);
begin
  Series1.Pen.Visible:=CBBorders.Checked;
end;

procedure TSquarifiedFileSystem.Fill_Level0_Items;
var c : Integer;
    tmp : TItemChildren;
begin
  ListBox1.Clear;
  ListBox1.Items.BeginUpdate;
  try
    tmp:=Series1.Items[0].Children;

    for c:=0 to High(tmp) do
        ListBox1.Items.AddObject(Series1.Labels[tmp[c]],TObject(tmp[c]));
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

procedure TSquarifiedFileSystem.Button1Click(Sender: TObject);
begin
  ChartEditor1.Execute
end;

initialization
  RegisterClass(TSquarifiedFileSystem);
end.
