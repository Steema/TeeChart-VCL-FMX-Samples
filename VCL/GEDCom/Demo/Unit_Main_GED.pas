unit Unit_Main_GED;

{
  Example using the TeeGEDCom.pas unit to load GEDCom genealogy trees,
  and using TeeChart to show stats.

  https://github.com/Steema/TeeChart-VCL-FMX-Samples/tree/main/VCL/GEDCom

  GED Examples from:

  https://github.com/arbre-app/public-gedcoms

}

{
  Other statistics and information that can be retrieved from the GEDCom data:

  Maximum number of generations
  Average Lifespan
  Longest lived versus Shortest lived
  Age Distribution (deads and alive)
  Birth and Death by months / centuries
  Average age at first marriage
  Average family size
  Largest families
  Longest marriages
  Multiple marriages
  Top geographical locations
  Migration trajectories
  Peak migration decades
  Surname frequency
  Popular given names
  Occupation breakdowns
  Errors with dates (died before born, married at age 4, had children at 105)
  Sourcing completeness
  Duplicate detections
  Orphan branches

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  TeeGEDCom, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart,
  VCLTee.Series, Vcl.Menus;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    CBFile: TComboBox;
    BLoad: TButton;
    Chart1: TChart;
    Panel2: TPanel;
    MemoInfo: TMemo;
    LBCharts: TListBox;
    Panel3: TPanel;
    LBSurname: TListBox;
    LBIndividuals: TListBox;
    MemoPerson: TMemo;
    PopupMenu1: TPopupMenu;
    Sort1: TMenuItem;
    Splitter1: TSplitter;
    Label2: TLabel;
    EFilter: TEdit;
    procedure CBFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure LBChartsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBSurnameClick(Sender: TObject);
    procedure LBIndividualsClick(Sender: TObject);
    procedure Sort1Click(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
  private
    { Private declarations }

    GED : TGEDCom;

    procedure ShowIndividuals;
    procedure ShowInfo;
    procedure LoadFromURL(const URL:String);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ShLwApi, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Generics.Collections;

function IsURL(const URL:String):Boolean;
begin
  result :=PathIsURL(PChar(URL));
end;

function GetFirstLetter(const ASurname:String):String;
begin
  result:=UpperCase(Copy(Trim(ASurname),1,1));
end;

function Surname_FistLetters(const GED:TGEDCom):TArray<String>;

  function Exists(const S:String; const Items:TArray<String>):Boolean;
  var t : Integer;
  begin
    for t:=0 to High(Items) do
        if Items[t]=S then
        begin
          result:=True;
          Exit;
        end;

    result:=False;
  end;

var t : Integer;
    tmp : String;
begin
  result:=nil;

  for t:=0 to High(GED.Individuals) do
  begin
    tmp:=GetFirstLetter(GED.Individuals[t].Surname);

    if tmp<>'' then
       if not Exists(tmp,result) then
          result:=result+[tmp];
  end;

  TArray.Sort<String>(result);
end;

procedure TMainForm.BLoadClick(Sender: TObject);

  procedure LoadGED;
  begin
    GED.Clear;

    if IsURL(CBFile.Text) then
       LoadFromURL(CBFile.Text)
    else
       GED.LoadFrom(CBFile.Text);
  end;

  procedure FillSurnames;
  begin
    LBSurname.Items.BeginUpdate;
    try
      LBSurname.Clear;

      LBSurname.Items.AddStrings(Surname_FistLetters(GED));
      LBSurname.Items.Insert(0,'All');

      LBSurname.ItemIndex:=0;
    finally
      LBSurname.Items.EndUpdate;
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  try
    LoadGED;
    ShowInfo;
    ShowIndividuals;

    EFilter.Clear;

    MemoPerson.Clear;
    MemoPerson.Hide;

    FillSurnames;

    Chart1.ClearChart;

    LBCharts.Enabled:=True;
    LBCharts.ItemIndex:=-1;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TMainForm.CBFileChange(Sender: TObject);
begin
  BLoad.Enabled:=Trim(CBFile.Text)<>'';
end;

procedure TMainForm.EFilterChange(Sender: TObject);
begin
  ShowIndividuals;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CBFile.ItemIndex:=0;
  CBFileChange(Self);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if ParamCount>0 then
  begin
    CBFile.Items.Add(ParamStr(1));

    CBFile.Text:=ParamStr(1);
    CBFileChange(Self);

    BLoadClick(Self);
  end;
end;

function URLToStrings(const URL:string):TStrings;
var FHTTP: TNetHTTPClient;
begin
  result:=TStringList.Create;

  FHttp:=TNetHTTPClient.Create(nil);
  try
    result.Text:=FHttp.Get(URL).ContentAsString;
  finally
    FHTTP.Free;
  end;
end;

procedure TMainForm.LBChartsClick(Sender: TObject);

  function ByGender:TArray<Integer>;
  var t : Integer;
  begin
    SetLength(result,3);

    for t:=0 to High(GED.Individuals) do
       Inc(result[Ord(GED.Individuals[t].Sex)]);
  end;

  function DeadAlive:TArray<Integer>;
  var t : Integer;
  begin
    SetLength(result,2);

    for t:=0 to High(GED.Individuals) do
       if GED.Individuals[t].Dead then
          Inc(result[0])
       else
          Inc(result[1]);
  end;

  function NumberOfChildren:TArray<Integer>;
  var L, tmp, t : Integer;
  begin
    SetLength(result,1);

    for t:=0 to High(GED.Individuals) do
    begin
      tmp:=GED.FindInFamilies(GED.Individuals[t].Code);

      if tmp=-1 then
         Inc(result[0])
      else
      begin
        L:=Length(GED.Families[tmp].Children);

        if L>Length(result)-2 then
           SetLength(result,L+2);

        Inc(result[L+1]);
      end;
    end;
  end;

var tmp : TChartSeries;
    v : TArray<Integer>;
begin
  Chart1.ClearChart;

  tmp:=Chart1.AddSeries(TBarSeries);

  tmp.ColorEachPoint:=True;

  if LBCharts.ItemIndex=-1 then
     Chart1.Title.Caption:=''
  else
     Chart1.Title.Caption:=LBCharts.Items[LBCharts.ItemIndex];

  case LBCharts.ItemIndex of

    0: begin
         v:=ByGender;
         tmp.Add(v[0],'Male');
         tmp.Add(v[1],'Female');
         tmp.Add(v[2],'Unknown');
       end;

    1: begin
         v:=DeadAlive;
         tmp.Add(v[0],'Dead');
         tmp.Add(v[1],'Alive');
       end;

    2: tmp.AddArray(NumberOfChildren);
  end;
end;

procedure TMainForm.LBIndividualsClick(Sender: TObject);

  procedure AddPerson(const Person:TIndividual; const ALines:TStrings);

    function Gender:String;
    begin
      case Person.Sex of
          TSex.Male: result:='Male';
        TSex.Female: result:='Female';
      else
        result:='Unknown';
      end;
    end;

    function EventToString(const AEvent:TDatePlaceNote):String;
    begin
      result:=AEvent.Date.ToString;

      if AEvent.Place<>'' then
         result:=result+' '+AEvent.Place;
    end;

    procedure ShowPerson(const AIndi:Integer);
    begin
      if AIndi<>-1 then
         ALines.Add('Marriage: '+GED.Individuals[AIndi].Name+' '+GED.Individuals[AIndi].Surname);
    end;

  var t,tmp : Integer;
  begin
    ALines.Clear;

    ALines.Add(Person.Name+' '+Person.Surname);
    ALines.Add('Gender: '+Gender);

    ALines.Add('Birth: '+EventToString(Person.Birth));
    ALines.Add('Baptism: '+EventToString(Person.Baptism));
    ALines.Add('Death: '+EventToString(Person.Death));

    ALines.Add('Age: '+Person.AgeYears.ToString+' years');
    ALines.Add('');

    if Person.Occupation.Profession<>'' then
       ALines.Add('Occupation: '+Person.Occupation.Profession);

    for t:=0 to High(Person.SpouseOf) do
    begin
      tmp:=GED.FindFamily(Person.SpouseOf[t]);

      if tmp<>-1 then
      begin
        if GED.Families[tmp].Husband=Person.Code then
           ShowPerson(GED.FindIndividual(GED.Families[tmp].Wife))
        else
           ShowPerson(GED.FindIndividual(GED.Families[tmp].Husband));

        if GED.Families[tmp].Marriage.Value.Date.Start.Value>0 then
           ALines.Add(EventToString(GED.Families[tmp].Marriage.Value));

        if GED.Families[tmp].Separation.Date.Start.Value>0 then
           ALines.Add('Separation: '+EventToString(GED.Families[tmp].Separation));

        if GED.Families[tmp].Divorce.Date.Start.Value>0 then
           ALines.Add('Divorce: '+EventToString(GED.Families[tmp].Divorce));
      end;
    end;
  end;

var tmp : Integer;
begin
  tmp:=LBIndividuals.ItemIndex;

  MemoPerson.Visible:=tmp<>-1;

  if MemoPerson.Visible then
     AddPerson(GED.Individuals[Integer(LBIndividuals.Items.Objects[tmp])],MemoPerson.Lines);
end;

procedure TMainForm.LBSurnameClick(Sender: TObject);
begin
  ShowIndividuals;
end;

procedure TMainForm.LoadFromURL(const URL: String);
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    tmp:=URLToStrings(URL);
    GED.LoadFrom(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TMainForm.ShowIndividuals;
var t : Integer;
    FirstLetter,
    tmpFilter,
    tmpSurname : String;
begin
  if LBSurname.ItemIndex>0 then
     FirstLetter:=LBSurname.Items[LBSurname.ItemIndex]
  else
     FirstLetter:='';

  tmpFilter:=UpperCase(Trim(EFilter.Text));

  LBIndividuals.Items.BeginUpdate;
  try
    LBIndividuals.Clear;

    for t:=0 to High(GED.Individuals) do
    begin
      tmpSurname:=GED.Individuals[t].Surname;

      if (FirstLetter='') or (GetFirstLetter(tmpSurname)=FirstLetter) then
         if (tmpFilter='') or (Pos(tmpFilter,UpperCase(tmpSurname))>0) then
            LBIndividuals.Items.AddObject(GED.Individuals[t].Name+' '+tmpSurname,TObject(t));
    end;

  finally
    LBIndividuals.Items.EndUpdate;
  end;
end;

procedure TMainForm.ShowInfo;
begin
  MemoInfo.Clear;

  MemoInfo.Lines.Add(Length(GED.Individuals).ToString + ' Individuals');
  MemoInfo.Lines.Add(Length(GED.Families).ToString + ' Families');

//  MemoInfo.Lines.Add(GED.Generations.ToString + ' Max Generations');

end;


procedure TMainForm.Sort1Click(Sender: TObject);
begin
  LBIndividuals.Sorted:=True;
end;

end.
