{ Steema Software 2026 }
{ www.steema.com }


{ GEDCOM Reader from *.ged file, returns record structures.

  https://www.gedcom.org/

  Note: This is not a full implementation of the GEDCom file format, but can
        read most GED files from several sites, like:

        Ancestry
        FamilySearch
        MyHeritage
        GeneatNet
        GetMatch

        etc

}

unit TeeGEDCOM;
{$SCOPEDENUMS ON}

interface

uses
  Classes;

type
  TSex=(Male,Female,Unknown);

  TDateStyle=(Unknown,Exact,Estimated,About,Before,After,Between,From);

  TDateParts=record
  public
    Value : TDateTime;
    Day, Month, Year : Boolean;

    procedure Clear;
    function ToString:String;
  end;

  TGEDDate=record
  public
    Start,
    Finish : TDateParts;
    Style : TDateStyle;

    procedure Clear;
    function ToString:String;
    function Year:Word;
  end;

  TLatLon=record
  public
    Latitude : String;
    Longitude : String;

    procedure Clear;
  end;

  TDatePlaceNote=record
  public
    Date : TGEDDate;
    Place : String;
    LatLon : TLatLon;
    Note : String;
    Source : String;

    procedure Clear;
  end;

  TBirth=TDatePlaceNote;
  TDeath=TDatePlaceNote;
  TBaptism=TDatePlaceNote;

  TOccupation=record
  public
    Profession : String;
    DatePlace : TDatePlaceNote;

    procedure Clear;
  end;

  TResidence=TDatePlaceNote;

  TBurial=record
  public
    Cemetery : String;
    DatePlace : TDatePlaceNote;

    procedure Clear;
  end;

  TChristening=TDatePlaceNote;

  TEvent=record
  public
    EventType : String;
    Info : String;
    Event : TDatePlaceNote;

    procedure Clear;
  end;

  TNaturalized=TDatePlaceNote;

  TAdopted=record
  public
    Family : Integer;
    Date : TGEDDate;
    Style : String;

    procedure Clear;
  end;

  TReference=record
  public
    ID : String;

    procedure Clear;
  end;

  TIndividual=record
  public
    Code : Integer;
    Name,
    Surname : String;

    Sex : TSex;

    Baptism : TBaptism;
    Birth : TBirth;
    Burial: TBurial;

    Dead : Boolean;
    Death : TDeath;
    Note : String;

    Occupation : TOccupation;
    Residence : TResidence;

    Naturalized : TNaturalized;
    Source : String;
    Title : String;
    Adopted : TAdopted;

    Reference : TReference;

    Christening : TChristening;

    ChildOf : Integer;
    SpouseOf : TArray<Integer>;

    Events : TArray<TEvent>;

    procedure Clear;
    function AgeYears:Integer;
  end;

  TMarriageType=(Unknown,Marriage,MarriageC);

  TMarriage=record
  public
    MarriageType : TMarriageType;
    Info : String;
    Value : TDatePlaceNote;

    procedure Clear;
  end;

  TSeparation=TDatePlaceNote;
  TDivorce=TDatePlaceNote;

  TFamily=record
  public
    Code : Integer;
    Husband : Integer;
    Wife : Integer;
    Children : TArray<Integer>;
    Marriage : TMarriage;
    MarriageLicense :  TDatePlaceNote;
    Separation : TSeparation;
    Divorce : TDivorce;

    Events : TArray<TEvent>;

    procedure Clear;
  end;

  TNote=record
  public
    Code : Integer;
    Note : String;

    procedure Clear;
  end;

  TGEDLoadProc=procedure(Line:Integer; const S:String) of object;

  TGEDCom=record
  public
    Individuals : TArray<TIndividual>;
    Families : TArray<TFamily>;
    Notes : TArray<TNote>;

    OnUnknownTag : TGEDLoadProc;

    procedure Clear;

    function FindFamily(const AFamily: Integer): Integer;
    function FindIndividual(const AIndi:Integer):Integer;
    function FindInFamilies(const AIndi:Integer):Integer;
    procedure LoadFrom(const ALines:TStrings); overload;
    procedure LoadFrom(const AFile:String); overload;
  end;

implementation

uses
  SysUtils,
  IOUtils;

procedure TGEDCom.Clear;
begin
  Individuals:=nil;
  Families:=nil;
end;

procedure TGEDCom.LoadFrom(const ALines:TStrings);
var
  Start:Integer;

  procedure DoError(const S:String); // noreturn;
  begin
    raise Exception.Create(Start.ToString+' '+S);
  end;

  procedure SkipTo(Level:Integer);
  var s : String;
  begin
    while Start+1<ALines.Count do
    begin
      s:=ALines[Start+1];

      if Copy(s,1,1)<=Level.ToString then
         break
      else
         Inc(Start);
    end;
  end;

  function PosOf(const ATag:String; const S:TArray<String>):Integer;
  var t : Integer;
  begin
    for t:=0 to High(s) do
        if S[t].ToUpper=ATag then
           Exit(t);

    result:=-1;
  end;

  function CodeOf(const ID:String):Integer;
  begin
    result:=Copy(ID.Replace('@',''),2,Length(ID)).ToInteger;
  end;

  function GetLevel(const S:String):Integer;
  var i : Integer;
      tmp : String;
  begin
    result:=-1;

    i:=Pos(' ',s);

    if i=0 then
       DoError('Wrong: '+s)
    else
    begin
      tmp:=Copy(s,1,i-1);

      if not Integer.TryParse(tmp,result) then
         DoError('Wrong level: '+tmp);
    end;
  end;

  function ReadDate(const S:String):TGEDDate;

    function MonthOf(S:String):Integer;
    begin
      S:=S.ToUpper;

      if S='JAN' then result:=1 else
      if S='FEB' then result:=2 else
      if S='MAR' then result:=3 else
      if S='APR' then result:=4 else
      if S='MAY' then result:=5 else
      if S='JUN' then result:=6 else
      if S='JUL' then result:=7 else
      if S='AUG' then result:=8 else
      if S='SEP' then result:=9 else
      if S='OCT' then result:=10 else
      if S='NOV' then result:=11 else
      if S='DEC' then result:=12 else
      begin
        DoError('Wrong month: '+s);
        result:=0;
      end;
    end;

    function YearToDate(const Year:String):TDateParts;
    begin
      result.Value:=EncodeDate(Year.ToInteger,1,1);
      result.Day:=False;
      result.Month:=False;
      result.Year:=True;
    end;

    function DateParts(const SS:TArray<String>; const AFrom,ATo:Integer):TDateParts;
    var L : Integer;
        Dummy : Integer;
    begin
      L:=ATo-AFrom+1;

      if L>2 then
      begin
        result.Value:=EncodeDate(ss[AFrom+2].ToInteger,MonthOf(ss[AFrom+1]),ss[AFrom].ToInteger);
        result.Day:=True;
        result.Month:=True;
        result.Year:=True;
      end
      else
      if L>1 then
      begin
        // Trick:
        if Copy(ss[AFrom],1,1)='(' then
        begin
          // (29 DEC)
          result.Value:=EncodeDate(1,MonthOf(ss[AFrom+1].Replace(')','')),ss[AFrom].Replace('(','').ToInteger);
          result.Day:=True;
          result.Month:=True;
          result.Year:=False;
        end
        else
        begin
          if TryStrToInt(ss[AFrom+1],Dummy) then
          begin
            // DEC 1929
            result.Value:=EncodeDate(ss[AFrom+1].ToInteger,MonthOf(ss[AFrom]),1);
            result.Day:=False;
            result.Month:=True;
            result.Year:=True;
          end
          else
          begin
            // 10 JAN
            result.Value:=EncodeDate(1,MonthOf(ss[AFrom+1]),ss[AFrom].ToInteger);
            result.Day:=True;
            result.Month:=True;
            result.Year:=False;
          end
        end;
      end
      else
        result:=YearToDate(ss[AFrom]);
    end;

    procedure TrimEmpty(var S:TArray<String>);
    var t : Integer;
    begin
      t:=0;

      while t<Length(S) do
        if Trim(S[t])='' then
           Delete(S,t,1)
        else
           Inc(t);
    end;

  var ss : TArray<String>;
      tmp : Integer;
      tmpS : String;
  begin
    result.Clear;

    ss:=s.Split([' ']);

    result.Style:=TDateStyle.Exact;

    if High(ss)>-1 then
    begin
      ss[0]:=ss[0].ToUpper;

      if ss[0]='EST' then
         result.Style:=TDateStyle.Estimated
      else
      if ss[0]='ABT' then
         result.Style:=TDateStyle.About
      else
      if ss[0]='BET' then
         result.Style:=TDateStyle.Between
      else
      if ss[0]='AFT' then
         result.Style:=TDateStyle.After
      else
      if ss[0]='BEF' then
         result.Style:=TDateStyle.Before
      else
      if ss[0]='FROM' then
         result.Style:=TDateStyle.From;

      if result.Style<>TDateStyle.Exact then
         Delete(ss,0,1);
    end;

    TrimEmpty(ss);

    case result.Style of
      TDateStyle.Estimated,
      TDateStyle.About,
      TDateStyle.Exact,
      TDateStyle.After,
      TDateStyle.Before :
          begin
            tmpS:=ss[High(ss)];

            tmp:=Pos('/',tmpS);

            if tmp>0 then
            begin
              ss[High(ss)]:=Copy(tmpS,1,tmp-1);
              result.Start:=DateParts(ss,0,High(ss));

              ss[High(ss)]:=Copy(tmpS,tmp+1,Length(tmpS));
              result.Finish:=DateParts(ss,0,High(ss));
            end
            else
              result.Start:=DateParts(ss,0,High(ss));
          end;

      TDateStyle.Between :
          begin
            tmp:=PosOf('AND',ss);

            result.Start:=DateParts(ss,0,tmp-1);
            result.Finish:=DateParts(ss,tmp+1,High(ss));
          end;

      TDateStyle.From :
          begin
            tmp:=PosOf('TO',ss);

            result.Start:=DateParts(ss,0,tmp-1);
            result.Finish:=DateParts(ss,tmp+1,High(ss));
          end;

     else
        DoError('TODO: '+s);
     end;
  end;

  function TryReadConc(UpToLevel:Integer):String;
  var Tag, s : String;
      i, Level : Integer;
  begin
    result:='';

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level<UpToLevel then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=UpToLevel then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          Tag:=Copy(s,1,i-1);

          if (Tag='CONC') or (Tag='CONT') then
          begin
            Delete(s,1,i);
            result:=result+s;
          end
          else
          begin
            Dec(Start);
            break;

            //DoError('Wrong: '+s);
          end;

        end;
      end
      else
        DoError('Wrong: '+s);

      Inc(Start);
    end;
  end;

  function TryReadLatLon:TLatLon;
  var s, Tag : String;
      i : Integer;
      Level : Integer;
  begin
    result.Clear;

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level<3 then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=4 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          Tag:=Copy(s,1,i-1);

          if Tag='LATI' then
             result.Latitude:=Copy(s,i+1,Length(s))
          else
          if Tag='LONG' then
             result.Longitude:=Copy(s,i+1,Length(s))
        end
      end;

      Inc(Start);
    end;
  end;

  function ReadDatePlaceNote:TDatePlaceNote;
  var s, Tag : String;
      i : Integer;
      Level : Integer;
  begin
    result.Date.Clear;
    result.Place:='';
    result.Note:='';

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level<2 then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=2 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          Tag:=Copy(s,1,i-1);

          if Tag='DATE' then
             result.Date:=ReadDate(Copy(s,i+1,Length(s)))
          else
          if Tag='PLAC' then
          begin
            Delete(s,1,i);
            result.Place:=s;

            result.LatLon:=TryReadLatLon;
          end
          else
          if Tag='NOTE' then
          begin
            Delete(s,1,i);
            result.Note:=s+TryReadConc(3);
          end
          else
          if Tag='SOUR' then
          begin
            Delete(s,1,i);
            result.Source:=s;
            SkipTo(1);  // 3 PAGE ...
          end
          else
            DoError('Wrong: '+s);
        end;
      end
      else
        DoError('Wrong: '+s);

      Inc(Start);
    end;
  end;

  function LastPart(const S:String):String;
  var ss : TArray<String>;
  begin
    ss:=s.Split([' ']);

    if High(ss)=0 then
       result:=''
    else
       result:=ss[1].Trim;
  end;

  function ReadEvent(const Extra:String):TEvent;
  var Tag, s : String;
      Level, i : Integer;
  begin
    result.Clear;

    result.Info:=Extra;

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level<2 then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=2 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          tag:=Copy(s,1,i-1);

          if tag='TYPE' then
          begin
            result.EventType:=LastPart(s);
            result.Event:=ReadDatePlaceNote;
          end
          else
          if Assigned(OnUnknownTag) then
             OnUnknownTag(Start,Tag);
        end;
      end
      else
        DoError('Wrong: '+s);

      Inc(Start);
    end;
  end;

  function ReadAdopted:TAdopted;
  var Tag, s : String;
      Level, i : Integer;
  begin
    result.Clear;

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level<2 then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=2 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          tag:=Copy(s,1,i-1);

          if tag='FAMC' then
          begin
            result.Family:=CodeOf(LastPart(s));
            SkipTo(1); // 2 PEDI adopted
          end
          else
          if Tag='DATE' then
             result.Date:=ReadDate(Copy(s,i+1,Length(s)))
          else
          if Assigned(OnUnknownTag) then
             OnUnknownTag(Start,Tag);
        end;
      end
      else
      if Level=3 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i>0 then
        begin
          tag:=Copy(s,1,i-1);

          if tag='ADOP' then
             result.Style:=LastPart(s)
          else
          if Assigned(OnUnknownTag) then
             OnUnknownTag(Start,Tag);
        end;
      end
      else
        DoError('Wrong: '+s);

      Inc(Start);
    end;
  end;

  function ReadFamily(const ID:String):TFamily;
  var Tag, s,
      tmpID, tmp : String;
      Level, i : Integer;
  begin
    result.Clear;

    result.Code:=CodeOf(ID);

    result.Husband:=0;
    result.Wife:=0;
    result.Children:=nil;

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start].Trim;

      i:=Pos(' ',s);

      if i=0 then
         DoError('Wrong: '+s)
      else
      begin
        tmp:=Copy(s,1,i-1);

        if not Integer.TryParse(tmp,Level) then
           DoError('Wrong level: '+tmp)
        else
        if Level=0 then
        begin
          Dec(Start);
          break;
        end
        else
        if Level=1 then
        begin
          Delete(s,1,i);

          i:=Pos(' ',s);

          if i=0 then
          begin
            tag:=s;

            if tag='MARR' then
            begin
              result.Marriage.MarriageType:=TMarriageType.Marriage;
              result.Marriage.Info:='';
              result.Marriage.Value:=ReadDatePlaceNote;
            end
            else
            if tag='MARL' then
            begin
              result.MarriageLicense:=ReadDatePlaceNote;
            end
            else
            if tag='SEP' then
               result.Separation:=ReadDatePlaceNote
            else
            if tag='DIV' then
               result.Divorce:=ReadDatePlaceNote
            else
            if tag='EVEN' then
               result.Events:=result.Events+[ReadEvent('')]
            else
            if tag='MARC' then
            begin
              result.Marriage.MarriageType:=TMarriageType.MarriageC;
              result.Marriage.Value:=ReadDatePlaceNote;
            end
            else
            if Assigned(OnUnknownTag) then
               OnUnknownTag(Start,Tag);
          end
          else
          begin
            tag:=Copy(s,1,i-1);

            tmpID:=Copy(s,i+1,Length(s));

            if tag='HUSB' then
            begin
              if result.Husband<>0 then
                 DoError('Duplicate Husband');

              result.Husband:=CodeOf(tmpID);
            end
            else
            if tag='WIFE' then
            begin
              if result.Wife<>0 then
                 DoError('Duplicate Wife');

              result.Wife:=CodeOf(tmpID);
            end
            else
            if tag='CHIL' then
            begin
              result.Children:=result.Children+[CodeOf(tmpID)];

              // Pending:
              {
                if tag='_FREL' then  // Father relationship
                else
                if tag='_MREL' then  // Mother relationship
              }

              SkipTo(1);
            end
            else
            if tag='DIV' then  // DIV Y
            else
            if tag='MARR' then // MARR Y   // MARR 1st Presby Chur.,
            begin
              result.Marriage.MarriageType:=TMarriageType.Marriage;
              result.Marriage.Info:=tmpID;
              result.Marriage.Value:=ReadDatePlaceNote;
            end
            else
            if tag='EVEN' then
               result.Events:=result.Events+[ReadEvent(tmpID)]
            else
            if Assigned(OnUnknownTag) then
               OnUnknownTag(Start,Tag);
          end;
        end
        else
          DoError('Wrong: '+s);
      end;

      Inc(Start);
    end;
  end;

  function RightPart(const S:String):String;
  begin
    result:=S;
    Delete(result,1,5);
  end;

  function ReadIndi(const ID:String):TIndividual;
  var Tag, s : String;
      Level, i : Integer;
  begin
    result.Clear;

    result.Code:=CodeOf(ID);

    Inc(Start);

    while Start<ALines.Count do
    begin
      s:=ALines[Start];

      Level:=GetLevel(s);

      if Level=0 then
      begin
        Dec(Start);
        break;
      end
      else
      if Level=1 then
      begin
        Delete(s,1,2);

        i:=Pos(' ',s);

        if i=0 then
        begin
          tag:=s;

          if tag='BIRT' then
             result.Birth:=ReadDatePlaceNote
          else
          if tag='DEAT' then
          begin
            result.Death:=ReadDatePlaceNote;
            result.Dead:=result.Death.Date.Style<>TDateStyle.Unknown;
          end
          else
          if tag='RESI' then
          begin
            // Pending: result.Residence.Info:='';
            TryReadConc(2);
            result.Residence:=ReadDatePlaceNote;
          end
          else
          if tag='OCCU' then
          begin
            result.Occupation.Profession:=LastPart(s);
            result.Occupation.DatePlace:=ReadDatePlaceNote;
          end
          else
          if tag='BAPM' then
             result.Baptism:=ReadDatePlaceNote
          else
          if tag='EVEN' then
             result.Events:=result.Events+[ReadEvent('')]
          else
          if tag='BURI' then
          begin
            result.Burial.Cemetery:='';
            result.Burial.DatePlace:=ReadDatePlaceNote;
          end
          else
          if (tag='OBJE') or (tag='_PHOTO') then
          begin
            SkipTo(1) // Tag not supported, not an error
          end
          else
          if tag='NATU' then
             result.Naturalized:=ReadDatePlaceNote
          else
          if tag='ADOP' then
             result.Adopted:=ReadAdopted
          else
          if tag='IMMI' then
          begin
            SkipTo(1)
          end
          else
          if tag='WILL' then
          begin
            SkipTo(1)
          end
          else
          if tag='CHR' then
             result.Christening:=ReadDatePlaceNote
          else
          if tag='_MILT' then // Military Service
          begin
            // Pending to replace with a new Event, Type=Military
            SkipTo(1);
          end
          else
          if tag='PROB' then // Probate (Validation of Will)
          begin
            SkipTo(1);
          end
          else
          if Assigned(OnUnknownTag) then
             OnUnknownTag(Start,Tag)
          else
             DoError('Wrong: '+s);
        end
        else
        begin
          tag:=Copy(s,1,i-1);

          if tag='NAME' then
          begin
            Delete(s,1,4);
            i:=Pos('/',s);

            result.Surname:='';

            if i=0 then
               result.Name:=s
            else
            begin
              result.Name:=Copy(s,2,i-3);
              result.Surname:=Copy(s,i+1,Length(s)-i-1);
            end;

            SkipTo(1);
          end
          else
          if tag='SEX' then
          begin
            s:=LastPart(s);

            if s='' then
               DoError('Wrong Sex: '+s)
            else
            if s='M' then
               result.Sex:=TSex.Male
            else
            if s='F' then
               result.Sex:=TSex.Female
            else
               result.Sex:=TSex.Unknown;

          end
          else
          if tag='FAMC' then
          begin
            Delete(s,1,5);
            result.ChildOf:=CodeOf(s);

            SkipTo(1); // 2 PEDI adopted
          end
          else
          if tag='FAMS' then
          begin
            Delete(s,1,5);
            result.SpouseOf:=result.SpouseOf+[CodeOf(s)];
          end
          else
          if tag='SOUR' then
          begin
            result.Source:=RightPart(s);
            SkipTo(0);  // 3 PAGE ...
          end
          else
          if tag='OCCU' then
          begin
            result.Occupation.Profession:=RightPart(s);
            result.Occupation.DatePlace:=ReadDatePlaceNote;
          end
          else
          if tag='TITL' then
             result.Title:=RightPart(s)
          else
          if tag='NOTE' then
             result.Note:=RightPart(s)+TryReadConc(2)
          else
          if tag='DEAT' then // DEAT Y
          begin
            Delete(s,1,5);

            if s='Y' then
               result.Dead:=True
            else
            if s='N' then
               result.Dead:=False
            else
            {
              // Pending: result.Info:=RightPart(s)
              DoError('Wrong: '+s);
            }
            ;

            result.Death:=ReadDatePlaceNote;
          end
          else
          if tag='REFN' then
             result.Reference.ID:=RightPart(s)
          else
          if tag='EVEN' then
             result.Events:=result.Events+[ReadEvent(RightPart(s))]
          else
          if tag='RESI' then
          begin
            TryReadConc(2);

            // PENDING: result.Residence.Info:=RightPart(s)
            result.Residence:=ReadDatePlaceNote;
          end
          else
          if tag='BIRT' then
             result.Birth:=ReadDatePlaceNote
          else
          if tag='BURI' then
          begin
            result.Burial.Cemetery:=RightPart(s);
            result.Burial.DatePlace:=ReadDatePlaceNote;
          end
          else
          if (tag='OBJE') or (tag='_PHOTO') then
             // not supported
          else
          if tag='SSN' then // Social Security Number
             SkipTo(1) // pending
          else
          if (tag='FSID') or (tag='_FSID') or (tag='_FSFTID') then // FamilySearch ID
             SkipTo(1) // pending
          else
          if tag='_MILT' then // Military Service
          begin
            // Pending to replace with a new Event, Type=Military
            SkipTo(1);
          end
          else
          if Assigned(OnUnknownTag) then
             OnUnknownTag(Start,Tag)
          else
             DoError('Wrong: '+s);
        end;
      end
      else
        DoError('Wrong: '+s);

      Inc(Start);
    end;
  end;

  function ReadNote(const ID:String):TNote;
  var Tag, s,
      tmpID, tmp : String;
      Level, i : Integer;
  begin
    result.Clear;
    result.Code:=CodeOf(ID);
  end;

var L : Integer;
    s : String;
    ss : TArray<String>;
begin
  Start:=0;

  while Start<ALines.Count do
  begin
    s:=ALines[Start];

    if Copy(s,1,2)='0 ' then
    begin
      Delete(s,1,2);

      if s='HEAD' then
         SkipTo(0)
      else
      if s='TRLR' then
         SkipTo(0)
      else
      begin
        ss:=s.Split([' ']);

        if High(ss)>0 then
        begin
          if ss[1]='INDI' then
          begin
            L:=Length(Individuals);
            SetLength(Individuals,L+1);
            Individuals[L]:=ReadIndi(ss[0]);
          end
          else
          if ss[1]='FAM' then
          begin
            L:=Length(Families);
            SetLength(Families,L+1);
            Families[L]:=ReadFamily(ss[0]);
          end
          else
          if ss[1]='SUBM' then // Submitter Author
             SkipTo(0)
          else
          if ss[1]='SOUR' then // Source
             SkipTo(0)
          else
          if ss[1]='REPO' then // Repository
             SkipTo(0)
          else
          if ss[1]='OBJE' then // Object
             SkipTo(0)
          else
          if ss[1]='NOTE' then // Note
          begin
            L:=Length(Notes);
            SetLength(Notes,L+1);
            Notes[L]:=ReadNote(ss[0]);

            SkipTo(0); // Pending
          end
          else
            DoError('Unknown 0: '+s);
        end
        else
          DoError('Wrong: '+s);
      end;
    end
    else
      DoError('Wrong: '+s);

    Inc(Start);
  end;
end;

function TGEDCom.FindIndividual(const AIndi: Integer): Integer;
var t : Integer;
begin
  for t:=0 to High(Individuals) do
      if Individuals[t].Code=AIndi then
         Exit(t);

  result:=-1;
end;

function TGEDCom.FindFamily(const AFamily: Integer): Integer;
var t : Integer;
begin
  for t:=0 to High(Families) do
      if Families[t].Code=AFamily then
         Exit(t);

  result:=-1;
end;

function TGEDCom.FindInFamilies(const AIndi: Integer): Integer;
var t : Integer;
begin
  for t:=0 to High(Families) do
      if (Families[t].Husband=AIndi) or
         (Families[t].Wife=AIndi) then
            Exit(t);

  result:=-1;
end;

procedure TGEDCom.LoadFrom(const AFile: String);
var s : TStrings;
begin
  s:=TStringList.Create;
  try
    s.Text:=TFile.ReadAllText(AFile);
    LoadFrom(s);
  finally
    s.Free;
  end;
end;

{ TGEDDate }

procedure TGEDDate.Clear;
begin
  Start.Clear;
  Finish.Clear;
  Style:=TDateStyle.Unknown;
end;

function TGEDDate.ToString: String;
begin
  result:='';

  if Style=TDateStyle.Unknown then
     result:='?'
  else
  begin
    result:=Start.ToString;

    case Style of
      TDateStyle.Estimated : result:='Estimated: '+result;
      TDateStyle.About     : result:='About: '+result;
      TDateStyle.Before    : result:='Before: '+result;
      TDateStyle.After     : result:='After: '+result;
      TDateStyle.Between   : result:='Between: '+result+' and '+Finish.ToString;
    end;
  end;
end;

function TGEDDate.Year: Word;
var m,d : Word;
begin
  if Start.Year then
     DecodeDate(Start.Value,result,m,d)
  else
     result:=0;
end;

{ TDateParts }

procedure TDateParts.Clear;
begin
  Value:=0;
  Day:=False;
  Month:=False;
  Year:=False;
end;

function TDateParts.ToString: String;
var D,M,Y : Word;
begin
  result:='';

  DecodeDate(Value,Y,M,D);

  if Year then
     result:=Y.ToString;

  if Month then
     result:=M.ToString+' / '+result;

  if Day then
     result:=D.ToString+' / '+result;
end;

{ TDatePlaceNote }

procedure TDatePlaceNote.Clear;
begin
  Date.Clear;
  Place:='';
  Note:='';
  Source:='';
  LatLon.Clear;
end;

{ TOccupation }

procedure TOccupation.Clear;
begin
  Profession:='';
  DatePlace.Clear;
end;

{ TIndividual }

function TIndividual.AgeYears: Integer;
begin
  if Dead then
     result:=Death.Date.Year-Birth.Date.Year
  else
     result:=CurrentYear-Birth.Date.Year;
end;

procedure TIndividual.Clear;
begin
  Code:=0;
  Name:='';
  Surname:='';
  Sex:=TSex.Unknown;
  Birth.Clear;
  Dead:=False;
  Death.Clear;
  Occupation.Clear;
  Residence.Clear;
  Baptism.Clear;
  Burial.Clear;
  Naturalized.Clear;
  Adopted.Clear;
  Reference.Clear;
  Christening.Clear;

  Note:='';
  Source:='';
  Title:='';

  ChildOf:=0;
  SpouseOf:=nil;
  Events:=nil;
end;

{ TFamily }

procedure TFamily.Clear;
begin
  Code:=0;
  Husband:=0;
  Wife:=0;
  Children:=nil;
  Marriage.Clear;
  MarriageLicense.Clear;
  Separation.Clear;
  Divorce.Clear;
  Events:=nil;
end;

{ TEvent }

procedure TEvent.Clear;
begin
  EventType:='';
  Info:='';
  Event.Clear;
end;

{ TMarriage }

procedure TMarriage.Clear;
begin
  MarriageType:=TMarriageType.Unknown;
  Info:='';
  Value.Clear;
end;

{ TAdopted }

procedure TAdopted.Clear;
begin
  Family:=0;
  Style:='';
  Date.Clear;
end;

{ TReference }

procedure TReference.Clear;
begin
  ID:='';
end;

{ TLatLon }

procedure TLatLon.Clear;
begin
  Latitude:='';
  Longitude:='';
end;

{ TBurial }

procedure TBurial.Clear;
begin
  Cemetery:='';
  DatePlace.Clear;
end;

{ TNote }

procedure TNote.Clear;
begin
  Code:=0;
  Note:='';
end;

end.
