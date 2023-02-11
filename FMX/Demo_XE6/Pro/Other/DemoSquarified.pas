unit DemoSquarified;
{$I TeeDefs.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Base, FMX.Controls.Presentation, FMX.Edit, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMX.ListBox, FMXTee.Series.SquarifiedMap, FMX.Layouts;

type
  TDemoSquarifiedSeries = class(TBaseForm)
    Chart1: TChart;
    EditFolder: TEdit;
    ButtonGO: TButton;
    Panel2: TPanel;
    CBBorders: TCheckBox;
    CBMarks: TCheckBox;
    CBViewMode: TComboBox;
    LabelView: TLabel;
    Label2: TLabel;
    CBColorBy: TComboBox;
    Label1: TLabel;
    Panel3: TPanel;
    LFile: TLabel;
    ListBox1: TListBox;
    procedure ButtonGOClick(Sender: TObject);
    procedure CBBordersChange(Sender: TObject);
    procedure CBMarksChange(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure CBViewModeChange(Sender: TObject);
    procedure EditFolderChangeTracking(Sender: TObject);
    procedure CBColorByChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
  private
    { Private declarations }

    Series1 : TSquarifiedMapSeries;

    procedure Fill_Level0_Items;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  System.Diagnostics,

  Squarified_FileSystem;

procedure TDemoSquarifiedSeries.ButtonGOClick(Sender: TObject);
var t1 : TStopwatch;
begin
  t1:=TStopwatch.StartNew;

  TSquarifiedFileSystem.FillTree(EditFolder.Text, Series1);

  Label1.Text:= Series1.TotalCount(0).ToString+' nodes.'+
                   ' Size: '+Round(Series1.Values[0]).ToString+' bytes. '+
                   ' Time: '+t1.ElapsedMilliseconds.ToString+' msec.';

  Fill_Level0_Items;

  // Reset color-by mode if different than default:
  if CBColorBy.ItemIndex<>0 then
     CBColorByChange(Self);
end;

procedure TDemoSquarifiedSeries.CBBordersChange(Sender: TObject);
begin
  Series1.Pen.Visible:=CBBorders.IsChecked;
end;

// Example of several ways to color items, based on their file-size,
// or level, or file extension.
procedure TDemoSquarifiedSeries.CBColorByChange(Sender: TObject);

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
    tmp:=TSquarifiedFileSystem.PathOf(AIndex,Series1);

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

procedure TDemoSquarifiedSeries.CBMarksChange(Sender: TObject);
begin
  Series1.Marks.Visible:=CBMarks.IsChecked;
end;

procedure TDemoSquarifiedSeries.CBViewModeChange(Sender: TObject);
begin
  Series1.FlatMode:=CBViewMode.ItemIndex=1;

  // Refresh color-by file size colors:
  if CBColorBy.ItemIndex=1 then
     CBColorByChange(Self);
end;

// Show the path of the file under current mouse position
procedure TDemoSquarifiedSeries.Chart1AfterDraw(Sender: TObject);
var t : Integer;
begin
  // Example:
  // Draw rectangle around top-level folders

  if CBBorders.IsChecked then
  if Series1.Count>0 then
  if not Series1.FlatMode then  // <-- only in hierarchy view mode
  begin
    Chart1.Canvas.Pen.Color:=TAlphaColorRec.Black;
    Chart1.Canvas.Pen.Width:=1;
    Chart1.Canvas.Brush.Style:=TBrushKind.None;

    for t:=0 to High(Series1.Items[0].Children) do
        Chart1.Canvas.Rectangle(Series1.RectangleOf(Series1.Items[0].Children[t]));
  end;
end;

procedure TDemoSquarifiedSeries.Chart1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var tmp : Integer;
begin
  tmp:=Series1.Clicked(X,Y);

  if tmp=TeeNoPointClicked then
     LFile.Text:=''
  else
     LFile.Text:=TSquarifiedFileSystem.PathOf(tmp,Series1)+' '+
                      Round(Series1.Values[tmp]).ToString+' bytes';
end;

procedure TDemoSquarifiedSeries.EditFolderChangeTracking(Sender: TObject);
begin
  ButtonGo.Enabled := Trim(EditFolder.Text) <> '';
end;

procedure TDemoSquarifiedSeries.FormCreate(Sender: TObject);

  // Example by code
  procedure CreateSeries;
  begin
    Series1 := TSquarifiedMapSeries.Create(Self);
    Series1.ParentChart := Chart1;

    Series1.Pen.Show; // show borders
    Series1.Marks.Hide;  // hide mark texts

    Series1.Selected.Hover.Pen.Color:=clWhite;  // when moving the mouse over
  end;

var tmp : String;
begin
  inherited;

  CreateSeries;

  // Select an initial folder
  tmp:=GetRADFolder;

  if tmp='' then
     {$IFDEF MSWINDOWS}
     tmp:=GetWindowsFolder
     {$ENDIF}
  else
     tmp:=tmp+'\source';

  EditFolder.Text := tmp;
end;

procedure TDemoSquarifiedSeries.ListBox1Change(Sender: TObject);
var tmp : Integer;
begin
  tmp:=ListBox1.ItemIndex;

  if tmp<>-1 then
     tmp:=Integer(ListBox1.Items.Objects[tmp]);

  // Set the selected children to display a rectangle around it
  Series1.Selected.HoverIndex:=tmp;
end;

procedure TDemoSquarifiedSeries.Fill_Level0_Items;
var c : Integer;
begin
  ListBox1.Clear;
  ListBox1.Items.BeginUpdate;
  try
    for c in Series1.Items[0].Children do
        ListBox1.Items.AddObject(Series1.Labels[c],TObject(c));
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

initialization
  RegisterClass(TDemoSquarifiedSeries);
end.
