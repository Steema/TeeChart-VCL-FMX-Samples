// Targa unit developed by Davie Reed, davie@smatters.com
// Adapted by Steema Software

unit TeeTarga;
{$I TeeDefs.inc}

interface

Uses
   Windows, Classes, Graphics;
{
{ Setup the following variable before calling LoadFromFileX
{
{ Global_KeepTrueFormat:Word
{ 0 = Use the files native bits per pixel for the TBitMap
{ 1 = Force TBitMap of 256 colors and use gray it file was 24bit
{ 2 = Force TBitMap to 24bit
{
{ SAVETOFILEX(parm1,parm2,parm3);
{    Parm1=Filename
{    Parm2=TBitMap to save
{    Parm3=Type of TGA file to create
{           1 = Save as 256 Color file
{           2 = Save as 16M file
}

Procedure SaveToFileX(FileName:String;Const BitMap:TBitMap;MyPcxType:Byte);
Procedure LoadFromFileX(FileName:String;Const BitMap:TBitMap);

type
  TTGAImage = class(TBitmap)
  public
    procedure LoadFromFile(const Filename: string); override;
  end;

implementation

Type
  TGAHeader=Packed Record
     IdentSize:Byte;
     ColorMapType:Byte;
     ImageType:Byte;
     ColorMapStart:Word;
     ColorMapLength:Word;
     ColorMapBits:Byte;
     XStart:Word;
     YStart:Word;
     Width:Word;
     Height:Word;
     Bits:Byte;
     Descriptor:Byte;
     End;

Type
   TypeRegVer=Set Of (Non_Registered,Registered,OEM,PRO,SYSOP);
   DataLineArray=Array[0..65535] Of Byte;
   DataWordArray=Array[0..65535] Of SmallInt;
   FakePalette= Packed Record
      LPal : TLogPalette;
      Dummy:Array[1..255] of TPaletteEntry;
      End;

   TypeEgaPalette=Array[0..16] Of Byte;
   TypePalette=Array[0..255,1..3] Of Byte;

Const
   Global_HiColor=3;
   Global_KeepTrueFormat:Word=0;

Var
  PictureFile:File;
  PaletteVGA:TypePalette;
  SysPal:FakePalette;
  TempArrayD:^DataLineArray;
  TempArrayD2:^DataLineArray;
  TempArrayDBIg:^DataLineArray;

  ErrorString:ShortString;
  Width:Word;
  Height:Word;
  BitsPerPixel:SmallInt;
  Compressed:Boolean;
  TGAHead:TGAHeader;
  MyKeepTrueFormat:Boolean;
  MyKeepTrueBits:Word;
  FileOk:Boolean;

var
  Index1:Word=0;
  Index2:Word=0;

const
  Const4096=8*1024;

Var
  IndexData:Array[0..Const4096-1] Of Byte;

Procedure FileGetMore;
Var
  NumRead:Integer;
Begin
FillChar(IndexData,Const4096,0);
BlockRead(PictureFile,IndexData,Const4096,NumRead);
Index1:=Const4096;
Index2:=0;
End;

Procedure FastGetBytes(Var Ptr1;NumBytes:Word);
Var
  X:Integer;
Begin
{
{ If we have enough the block it!
{ Otherwise do one at a time!
}
If Index1<NumBytes Then
   Begin
   If Index1=0 Then
      Begin
      FileGetMore;
      End;
   For X:=0 To NumBytes-1 Do
       Begin
       DataLineArray(Ptr1)[X]:=IndexData[Index2];
       Inc(Index2);
       Dec(Index1);
       If Index1=0 Then
          FileGetMore;
       End;
   End
Else
   Begin
   {
   { Block it fast!
   }
   Move(IndexData[Index2],DataLineArray(Ptr1)[0],NumBytes);
   Index2:=Index2+Numbytes;
   Index1:=Index1-NumBytes;
   End;
End;

Function FastGetByte:Byte;
Begin
If Index1=0 Then
   Begin
   FileGetMore;
   End;
FastGetByte:=IndexData[Index2];
Inc(Index2);
Dec(Index1);
End;

Function FastGetWord:Word;
Begin
FastGetWord:=Word(FastGetByte)+Word(FastGetByte)*256;
End;

Procedure FileIoReset;
Begin
Index1:=0;
Index2:=0;
End;

Procedure OpenFile(Var FileName:String;Var FileOk:Boolean);
Var
  Io:Integer;
  OldFileMode:Word;
Begin
FileIoReset;
// Io:=IoResult;
OldFileMode:=FileMode;
FileMode:=0;
AssignFile(PictureFile,FileName);
ReSet(PictureFile,1);
Io:=IoResult;
If Io<>0 Then
   Begin
   FileOk:=False;
   End;
FileMode:=OldFileMode;
End;

Procedure FillerUp(Var TempArrayD;Size:Word;B1:Byte);
Begin
FillChar(TempArrayD,Size,B1);
End;

Procedure ConvertXBitsToYBits(Var Input,Output:DataLineArray;Xbits,Ybits,Width:Word);
Var
  X,Z:Word;
  B1:Byte;
Begin
{
{ Generic converter to a single data line :)
{ Can go only from smaller bits to larger bits, otherwise you need to
{     dither down!
{ PaletteVGA MUST be setup already!
}
Case Xbits Of
     1:Begin
       Case Ybits Of
            4:Begin
              {
              { From 1 bit to 4 bit, hmmmmm EZ :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 3] Shr (7-(X Mod 8))) And 1;
                  OutPut[X Shr 1]:=OutPut[X Shr 1] Or (B1 Shl ((1-(X Mod 2))*4));
                  End;
              End;
            8:Begin
              {
              { From 1 bit to 8 bit, hmmmmm EZ :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 3] Shr (7-(X Mod 8) )) And 1;
                  OutPut[X]:=B1;
                  End;
              End;
           24:Begin
              {
              { From 1 bit to 8 bit, hmmmmm EZ :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=((Input[X Shr 3] Shr (7-(X Mod 8))) And 1)*255;
                  OutPut[Z+0]:=B1;
                  OutPut[Z+1]:=B1;
                  OutPut[Z+2]:=B1;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
     4:Begin
       Case Ybits Of
            4:Begin
              Move(Input[0],Output[0],Width);
              End;
            8:Begin
              {
              { Go from 4 bits to 8 bit :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 1] Shr ((1-(X Mod 2))*4)) And $0F;
                  OutPut[X]:=B1;
                  End;
              End;
           24:Begin
              {
              { Go from 4 bits to 24 bit :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 1] Shr ((1-(X Mod 2))*4)) And $0F;
                  OutPut[Z+0]:=(PaletteVGA[B1,3]*255) Div 63;
                  OutPut[Z+1]:=(PaletteVGA[B1,2]*255) Div 63;
                  OutPut[Z+2]:=(PaletteVGA[B1,1]*255) Div 63;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
     8:Begin
       Case Ybits Of
            1:Begin
              For X:=0 To Width-1 Do
                  OutPut[X Shr 3]:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=InPut[X];
                  OutPut[X Shr 3]:=OutPut[X Shr 3] Or (B1 Shl (7-(X Mod 8)));
                  End;
              End;
            8:Begin
              Move(Input[0],Output[0],Width);
              End;
           24:Begin
              {
              { From 8 bit to 24 bit, hmmmmm 2EZ :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=Input[X];
                  OutPut[Z+0]:=(PaletteVGA[B1,3]*255) Div 63;
                  OutPut[Z+1]:=(PaletteVGA[B1,2]*255) Div 63;
                  OutPut[Z+2]:=(PaletteVGA[B1,1]*255) Div 63;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
    24:Begin
       Case Ybits Of
            24:Begin
               Move(Input[0],Output[0],Width*3);
               End;
            End;
       End;
    End;
End;






Procedure SetUpMaskGrayPalette;
Var
  I,J:Word;
Begin
For J:=0 To 255 Do
    Begin
    For I:=1 To 3 Do
        Begin
        PaletteVga[J,I]:=J*63 Div 255;
        End;
    End;
End;

Function PCXGrayValue(R,G,B:Word):Word;
Begin
PCXGrayValue:=((R Shl 5)+(G Shl 6)+(B*12)) Div 108;
End;

Procedure MakePalBW(Const BitMap:TBitMap);
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=2;
Syspal.LPal.PalPalEntry[0].peRed:=0;
Syspal.LPal.PalPalEntry[0].peGreen:=0;
Syspal.LPal.PalPalEntry[0].peBlue:=0;
Syspal.LPal.PalPalEntry[0].peFlags:=0;
Syspal.Dummy[1].peRed:=255;
Syspal.Dummy[1].peGreen:=255;
Syspal.Dummy[1].peBlue:=255;
Syspal.Dummy[1].peFlags:=0;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure MakePalPalette(Const BitMap:TBitMap);
Var
  I:Word;
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=256;
For I:=0 To 255 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=  (PaletteVga[I,1])*4;
    Syspal.LPal.PalPalEntry[I].peGreen:=(PaletteVga[I,2])*4;
    Syspal.LPal.PalPalEntry[I].peBlue:= (PaletteVga[I,3])*4;
    Syspal.LPal.PalPalEntry[I].peFlags:= 0;
    End;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure MakePalPaletteX(Const BitMap:TBitMap;HowMany:Word);
Var
  I:Word;
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=HowMany;
For I:=0 To HowMany-1 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=  (PaletteVga[I,1])*4;
    Syspal.LPal.PalPalEntry[I].peGreen:=(PaletteVga[I,2])*4;
    Syspal.LPal.PalPalEntry[I].peBlue:= (PaletteVga[I,3])*4;
    Syspal.LPal.PalPalEntry[I].peFlags:= 0;
    End;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure SaveThePalette(Const HPal:HPalette;Var SavePal:TypePalette);
Var
  I:Word;
Begin
For I:=0 To 255 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=0;
    Syspal.LPal.PalPalEntry[I].peGreen:=0;
    Syspal.LPal.PalPalEntry[I].peBlue:=0;
    End;
GetPaletteEntries(HPal,0,256,SysPal.LPal.PalPalEntry[0]);
For I:=0 To 255 Do
    Begin
    SavePal[I,1]:=(((Syspal.LPal.PalPalEntry[I].peRed)) Div 4);
    SavePal[I,2]:=(((Syspal.LPal.PalPalEntry[I].peGreen)) Div 4);
    SavePal[I,3]:=(((Syspal.LPal.PalPalEntry[I].peBlue)) Div 4);
    End;
End;

Procedure MakeGenPalette;
Var
  X:Word;
  R,G,B:Word;
Begin
X:=0;
For R:=0 To 7 Do
    Begin
    For G:=0 To 7 Do
        Begin
        For B:=0 To 3 Do
            Begin
            PaletteVga[X,1]:=(R+1)*8-1;
            PaletteVga[X,2]:=(G+1)*8-1;
            PaletteVga[X,3]:=(B+1)*16-1;
            Inc(X);
            End;
        End;
    End;
End;

Function  ShouldIKeepTrueFormat(Var BPP:Word):Boolean;
Begin
{
{ Choices
{    Use File Colors
{    Force 256 Colors
{    Force 16M Colors
}
If Global_KeepTrueFormat=0 Then
   ShouldIKeepTrueFormat:=True
Else
   ShouldIKeepTrueFormat:=False;
If Global_KeepTrueFormat=1 Then
   BPP:=8;
If Global_KeepTrueFormat=2 Then
   BPP:=24;
End;











Procedure ReadTGAFileHeader(
              Var FileOk:Boolean;
              Var ErrorString:ShortString;
              Var Width:Word;
              Var Height:Word;
              Var BitsPerPixel:SmallInt;
              Var Compressed:Boolean
              );
Label
  ExitNow;
Var
  I,W1:Word;
  DummyArray:Array[1..4048] Of Char;
Begin
{
{ Read Targa Header
}
FastGetBytes(TGAHead,SizeOf(TGAHeader));
If NOT(TGAHead.ImageType In [1,2,4,9,10,11]) Then
   Begin
   ErrorString:='Invalid TGA file!';
   FileOk:=False;
   Goto ExitNow;
   End;
If NOT(TGAHead.Bits In [1,4,8,16,24,32]) Then
   Begin
   ErrorString:='Invalid TGA file!';
   FileOk:=False;
   Goto ExitNow;
   End;
Width:=TGAHead.Width;
Height:=TGAHead.Height;
BitsPerPixel:=TGAHead.Bits;
FastGetBytes(DummyArray,TGAHead.IdentSize);
{
{ Read in colormap
}
For I:=0 To 255 Do
    Begin
    PaletteVGA[I,1]:=0;
    PaletteVGA[I,2]:=0;
    PaletteVGA[I,3]:=0;
    End;
If TGAHead.ColorMapType<>0 Then
   Begin
   Case TGAHead.ColorMapBits Of
     24:Begin
        For I:=TGAHead.ColorMapStart To TGAHead.ColorMapStart+
                                        TGAHead.ColorMapLength-1 Do
            Begin
            PaletteVGA[I,3]:=FastGetByte Div 4;
            PaletteVGA[I,2]:=FastGetByte Div 4;
            PaletteVGA[I,1]:=FastGetByte Div 4;
            End;
        End;
     16:Begin
        For I:=TGAHead.ColorMapStart To TGAHead.ColorMapStart+
                                        TGAHead.ColorMapLength-1 Do
            Begin
            W1:=FastGetWord;
            PaletteVGA[I,3]:=((W1 Shr 10) And $1F) Shl 1;
            PaletteVGA[I,2]:=((W1 Shr  5) And $1F) Shl 1;
            PaletteVGA[I,1]:=((W1 Shr  0) And $1F) Shl 1;
            End;
        End;
     End;

   End;
If ((BitsPerPixel=8) And (TGAHead.ColorMapType=0)) Then
   SetUpMaskGrayPalette
Else
   Begin
   If BitsPerPixel=1 Then
      Begin
      PaletteVGA[0,1]:=0;
      PaletteVGA[0,2]:=0;
      PaletteVGA[0,3]:=0;
      PaletteVGA[1,1]:=63;
      PaletteVGA[1,2]:=63;
      PaletteVGA[1,3]:=63;
      End;
   End;
Compressed:=False;
If TGAHead.ImageType In [9,10,11] Then
   Compressed:=True;
ExitNow:
End;


Procedure LoadFromFileX(FileName:String;Const BitMap:TBitMap);
Const
  MaskTable:Array[0..7] Of Byte=(128,64,32,16,8,4,2,1);
Var
  II,NewWidth:Word;
  TrueLineBytes,LineBytes:Word;
  StartLine,IncLine,I:SmallInt;
  Ptr1:Pointer;

Procedure PixelSwapArray(Var TempArrayD;Wide:Word);
Var
  W,X,Y,Z:Word;
  Byte1,Byte2,Byte3:Byte;
Begin
{
{ Should I do 1 byte pixel or 3 byte pixels
}
Case BitMap.PixelFormat Of
     pf8Bit: Begin
             Y:=Wide Div 2;
             Z:=Wide-1;
             For X:=0 To Y-1 Do
                 Begin
                 Byte1:=DataLineArray(TempArrayD)[X];
                 DataLineArray(TempArrayD)[X]:=DataLineArray(TempArrayD)[Z];
                 DataLineArray(TempArrayD)[Z]:=Byte1;
                 Dec(Z);
                 End;
             End;
     pf24Bit:Begin
             Y:=(Wide Div 3) Div 2;
             Z:=Wide-3;
             W:=0;
             For X:=0 To Y-1 Do
                 Begin
                 Byte1:=DataLineArray(TempArrayD)[W+0];
                 Byte2:=DataLineArray(TempArrayD)[W+1];
                 Byte3:=DataLineArray(TempArrayD)[W+2];
                 DataLineArray(TempArrayD)[W+0]:=DataLineArray(TempArrayD)[Z+0];
                 DataLineArray(TempArrayD)[W+1]:=DataLineArray(TempArrayD)[Z+1];
                 DataLineArray(TempArrayD)[W+2]:=DataLineArray(TempArrayD)[Z+2];
                 DataLineArray(TempArrayD)[Z+0]:=Byte1;
                 DataLineArray(TempArrayD)[Z+1]:=Byte2;
                 DataLineArray(TempArrayD)[Z+2]:=Byte3;
                 Z:=Z-3;
                 W:=W+3;
                 End;
             End;
     End;
End;

Procedure TGAReverse(Var TempArrayD:DataLineArray);
Begin
If TGAHead.Descriptor And $10<>0 Then
   PixelSwapArray(TempArrayD,TrueLineBytes);
End;

Procedure TGAMono2Vga;
Var
  I:SmallInt;
Begin
For I:=0 To Width-1 Do
    Begin
    If (TempArrayD^[I] Shr 3) And MaskTable[I And 7]<>0 Then
       TempArrayD2^[I]:=1
    Else
       TempArrayD2^[I]:=0
    End;
Move(TempArrayD2^[0],TempArrayD^[0],Width);
End;

Function Pixels2Bytes(Width:Word):Word;
Begin
Pixels2Bytes:=(Width+7) Div 8;
End;

Procedure TGA16_ANY_U(Var Z:Word;Var TempArrayD;Width:Word);
Procedure Do24;
Var
  W1,I:Word;
  R,G,B:Byte;
Begin
   For I:=0 To Width-1 Do
       Begin
       W1:=FastGetWord;
       R:=((W1 Shr 10) And $1F) Shl 3;
       G:=((W1 Shr  5) And $1F) Shl 3;
       B:=((W1 Shr  0) And $1F) Shl 3;
       DataLineArray(TempArrayD)[Z+0]:=B;
       DataLineArray(TempArrayD)[Z+1]:=G;
       DataLineArray(TempArrayD)[Z+2]:=R;
       Z:=Z+Global_HiColor;
       End;
End;
Procedure Do8;
Var
  W1,I:Word;
  R,G,B:Byte;
Begin
   For I:=0 To Width-1 Do
       Begin
       W1:=FastGetWord;
       R:=((W1 Shr 10) And $1F) Shl 3;
       G:=((W1 Shr  5) And $1F) Shl 3;
       B:=((W1 Shr  0) And $1F) Shl 3;
       DataLineArray(TempArrayD)[Z]:=PcxGrayValue(R,G,B);
       Inc(Z);
       End;
End;

Begin
If MyKeepTrueFormat Then
   Do24
Else
   Begin
   Case MyKeepTrueBits Of
        8:Do8;
       24:Do24;
       End;
   End;
End;

Procedure TGA24_ANY_U(Var Z:Word;Flag:Byte;Var TempArrayD;Width:Word);
Type
   TypeRGB=Packed Record
      B,G,R:Byte;
      End;
Var
  RGB:TypeRGB;
Procedure Do8;
Var
  I:Word;
Begin
      For I:=0 To Width-1 Do
          Begin
          FastGetBytes(RGB,3);
          DataLineArray(TempArrayD)[Z]:=PcxGrayValue(RGB.R,RGB.G,RGB.B);
          Inc(Z);
          If Flag=1 Then
             FastGetByte;
          End;
End;
Procedure Do24;
Var
  I:Word;
Begin
For I:=0 To Width-1 Do
    Begin
    DataLineArray(TempArrayD)[Z+0]:=FastGetByte;
    DataLineArray(TempArrayD)[Z+1]:=FastGetByte;
    DataLineArray(TempArrayD)[Z+2]:=FastGetByte;
    Z:=Z+Global_HiColor;
    If Flag=1 Then
       FastGetByte;
    End;
End;

Begin
If (Z=1) Or (MyKeepTrueFormat=False) Then
   Begin
   If MyKeepTrueFormat Then
      Do24
   Else
      Begin
      Case MyKeepTrueBits Of
           8:Do8;
          24:Do24;
          End;
      End;
   End
Else
   Begin
   {
   { Z=0 AND keep=true
   }
   FastGetBytes(DataLineArray(TempArrayD)[Z],Width*Global_HiColor);
   Z:=Z+Global_HiColor*Width;
   End;
End;


Procedure ReadTGALine;
Var
  N,Size,LineSize:SmallInt;
  W1,Z:Word;
  R,G,B,B1:Byte;
Procedure Do8;
Var
  I:Word;
Begin
For I:=0 To Size-1 Do
    Begin
    TempArrayD^[Z]:=PcxGrayValue(R,G,B);
    Inc(Z);
    End;
End;
Procedure Do8Raw;
Begin
FastGetBytes(TempArrayD^[0],Width);
End;
Procedure Do8RawPart;
Begin
FastGetBytes(TempArrayD^[Z],Size);
Z:=Z+Size;
End;
Procedure Do8Fill(B1:Byte);
Begin
FillerUp(TempArrayD^[Z],Size,B1);
Z:=Z+Size;
End;
Procedure Do24Raw;
Var
  I,Z:Word;
Begin
Z:=0;
For I:=0 To Width-1 Do
    Begin
    B1:=FastGetByte;
    TempArrayD^[Z+0]:=PaletteVGA[B1,3]*4+3;
    TempArrayD^[Z+1]:=PaletteVGA[B1,2]*4+3;
    TempArrayD^[Z+2]:=PaletteVGA[B1,1]*4+3;
    Z:=Z+Global_HiColor;
    End;
End;
Procedure Do24RawPart;
Var
  I:Word;
Begin
For I:=0 To Size-1 Do
    Begin
    B1:=FastGetByte;
    TempArrayD^[Z+0]:=PaletteVGA[B1,3]*4+3;
    TempArrayD^[Z+1]:=PaletteVGA[B1,2]*4+3;
    TempArrayD^[Z+2]:=PaletteVGA[B1,1]*4+3;
    Z:=Z+Global_HiColor;
    End;
End;
Procedure Do24Fill(B1:Byte);
Var
  I:Word;
  R,G,B:Byte;
Begin
R:=PaletteVGA[B1,1]*4+3;
G:=PaletteVGA[B1,2]*4+3;
B:=PaletteVGA[B1,3]*4+3;
For I:=0 To Size-1 Do
    Begin
    TempArrayD^[Z+0]:=B;
    TempArrayD^[Z+1]:=G;
    TempArrayD^[Z+2]:=R;
    Z:=Z+Global_HiColor;
    End;
End;
Procedure Do24;
Var
  I:Word;
Begin
For I:=0 To Size-1 Do
    Begin
    TempArrayD^[Z+0]:=B;
    TempArrayD^[Z+1]:=G;
    TempArrayD^[Z+2]:=R;
    Z:=Z+Global_HiColor;
    End;
End;

Begin
N:=0;
If BitsPerPixel=1 Then
   LineSize:=Pixels2Bytes(Width)
Else
   LineSize:=Width;
{
{ Uncompressed Lines
}
If TGAHead.ImageType In [1,2,3] Then
   Begin
   Case BitsPerPixel Of
        1:FastGetBytes(TempArrayD^[0],LineBytes);
        8:Begin
          If MyKeepTrueFormat Then
             Do8Raw
          Else
             Begin
             Case MyKeepTrueBits Of
                  8:Do8Raw;
                 24:Do24Raw;
                 End;
             End;
          End;
       16:Begin
          Z:=0;
          TGA16_ANY_U(Z,TempArrayD^[0],Width);
          End;
       24:Begin
          Z:=0;
          TGA24_ANY_U(Z,0,TempArrayD^[0],Width);
          End;
       32:Begin
          Z:=0;
          TGA24_ANY_U(Z,1,TempArrayD^[0],Width);
          End;
       End;
   End
Else
{
{ Compressed Lines
}
   Begin
   Z:=0;
 Repeat
  B1:=FastGetByte;
  Size:=(B1 And $7F)+1;
  N:=N+Size;
  If (B1 And $80)<>0 Then
     Begin
     Case BitsPerPixel Of
       1,
       8:Begin
         B1:=FastGetByte;
         If MyKeepTrueFormat Then
            Do8Fill(B1)
         Else
            Begin
            Case MyKeepTrueBits Of
                 8:Do8Fill(B1);
                24:Do24Fill(B1);
                End;
            End
         End;
      16:Begin
         W1:=FastGetWord;
         R:=((W1 Shr 10) And $1F) Shl 3;
         G:=((W1 Shr  5) And $1F) Shl 3;
         B:=((W1 Shr  0) And $1F) Shl 3;
         If MyKeepTrueFormat Then
            Do24
         Else
            Begin
            Case MyKeepTrueBits Of
                 8:Do8;
                24:Do24;
                End;
            End;
         End;
   24,32:Begin
         B:=FastGetByte;
         G:=FastGetByte;
         R:=FastGetByte;
         If BitsPerPixel=32 Then
            B1:=FastGetByte;
         If MyKeepTrueFormat Then
            Do24
         Else
            Begin
            Case MyKeepTrueBits Of
                 8:Do8;
                24:Do24;
                End;
            End;
         End;
      End;
  End
  Else
{
{ Single bytes
}
  Begin
  Case BitsPerPixel Of
       1,
       8:Begin
         If MyKeepTrueFormat Then
            Do8RawPart
         Else
            Begin
            Case MyKeepTrueBits Of
                 8:Do8RawPart;
                24:Do24RawPart;
                End;
            End;
         End;
      16:Begin
         TGA16_ANY_U(Z,TempArrayD^[0],Size);
         End;
      24:Begin
         TGA24_ANY_U(Z,0,TempArrayD^[0],Size);
         End;
      32:Begin
         TGA24_ANY_U(Z,1,TempArrayD^[0],Size);
         End;
      End;
  End;
  Until N>=LineSize;
  End;
End;

Begin
{
{ Read Targa File
}
MyKeepTrueFormat:=ShouldIKeepTrueFormat(MyKeepTrueBits);
ErrorString:='';
FileOk:=True;
OpenFile(FileName,FileOk);
ReadTgaFileHeader(FileOK,ErrorString,Width,Height,BitsPerPixel,Compressed);
If FileOk Then
   Begin
   BitMap.Height:=Height;
   BitMap.Width:=Width;
   Case BitsPerPixel Of
     1:Begin
       BitMap.PixelFormat:=pf1bit;
       MakePalBW(BitMap);
       End;
     8:Begin
       BitMap.PixelFormat:=pf8bit;
       MakePalPalette(BitMap);
       End;
     16:Begin
        BitMap.PixelFormat:=pf24bit;
        End;
     24:Begin
        BitMap.PixelFormat:=pf24bit;
        End;
     32:Begin
        BitMap.PixelFormat:=pf24bit;
        End;
     End;
   Case BitsPerPixel Of
          1,8:Begin
              If MyKeepTrueFormat Then
                 Begin
                 End
              Else
                 Begin
                 Case MyKeepTrueBits Of
                      8:Begin
                        BitMap.PixelFormat:=pf8bit;
                        End;
                     24:Begin
                        BitMap.PixelFormat:=pf24bit;
                        If BitsPerPixel<>8 Then
                           MakeGenPalette;
                        End;
                     End;
                 MakePalPalette(BitMap);
                 End;
              End;
     16,24,32:Begin
              If MyKeepTrueFormat Then
                 MakeGenPalette
              Else
                 Begin
                 Case MyKeepTrueBits Of
                      8:Begin
                        BitMap.PixelFormat:=pf8bit;
                        SetUpMaskGrayPalette;
                        End;
                     24:MakeGenPalette;
                     End;
                 End;
              MakePalPalette(BitMap);
              End;
     End;

   NewWidth:=Width*Global_HiColor;
   GetMem(TempArrayD,NewWidth);
   GetMem(TempArrayD2,NewWidth);
   If BitsPerPixel=1 Then
      LineBytes:=Pixels2Bytes(Width)
   Else
      Begin
      If BitsPerPixel=8 Then
         LineBytes:=Width
      Else
         LineBytes:=Width*3;
      End;
   If MyKeepTrueFormat=True Then
      TrueLineBytes:=LineBytes
   Else
      Begin
      Case MyKeepTrueBits Of
           8:TrueLineBytes:=Width;
          24:TrueLineBytes:=Width*Global_HiColor;
          End;
      End;
   If TGAHead.Descriptor And $20=0 Then
      Begin
      StartLine:=Height-1;
      IncLine:=-1;
      End
   Else
      Begin
      StartLine:=0;
      IncLine:=1;
      End;
   I:=StartLine;
   II:=0;

   If TGAHead.ImageType In [1,2,3,9,10,11] Then
   Begin
   Repeat
    Begin
    ReadTGALine;
    Case BitsPerPixel Of
              1:TGAMono2Vga;
              End;
    TGAReverse(TempArrayD^);
    Ptr1:=BitMap.ScanLine[I];
    {
    { Copy the data
    }
    Move(TempArrayD^,Ptr1^,TrueLineBytes);
    End;
    Inc(II);
    I:=I+IncLine;
   Until II>=Height;
   End;
   FreeMem(TempArrayD,NewWidth);
   FreeMem(TempArrayD2,NewWidth);
   End
Else
   Begin
   BitMap.Width:=1;
   BitMap.Height:=1;
   BitsPerPixel:=8;
   End;
If IoResult<>0 Then ;
Close(PictureFile);
If IoResult<>0 Then ;
End;

Procedure SaveToFileX(FileName:String;Const BitMap:TBitMap;MyPcxType:Byte);
Label
  ErrExitClose;
Var
  ResultStatus:Boolean;
  File1:File;
  TGAHead:TGAHeader;
  MyWidth:Word;
  MyHeight:Word;
  CurrBitsPerPixel:Word;
  NewLine:^DataLineArray;

Procedure TGAWrite256Palette;
Var
  X,Y:Word;
  B1:Byte;
Begin
For X:=0 To 255 Do
    Begin
    For Y:=3 DownTo 1 Do
        Begin
        B1:=(PaletteVga[X,Y]*255) Div 63;
        BlockWrite(File1,B1,1);
        End;
    End;
End;

Const
   TGADescriptor:String[60]='TurboView(GIF-REED) produced this TARGA file!'+Chr($1A);
Procedure TGAWriteHeader;
Begin
TGAHead.IdentSize:=Length(TGADescriptor);
If MyPcxType=1 Then
   Begin
   TGAHead.ColorMapType:=1;
   TGAHead.ImageType:=1;
   TGAHead.ColorMapStart:=0;
   TGAHead.ColorMapLength:=256;
   TGAHead.ColorMapBits:=24;
   End
Else
   Begin
   TGAHead.ColorMapType:=0;
   TGAHead.ImageType:=2;
   TGAHead.ColorMapStart:=0;
   TGAHead.ColorMapLength:=0;
   TGAHead.ColorMapBits:=24;
   End;
TGAHead.XStart:=0;
TGAHead.YStart:=0;
TGAHead.Width:=MyWidth;
TGAHead.Height:=MyHeight;
Case MyPcxType Of
     1:TGAHead.Bits:=8;
     2:TGAHead.Bits:=24;
     End;
TGAHead.Descriptor:=$20;
BlockWrite(File1,TGAHead,SizeOf(TGAHead));
BlockWrite(File1,TGADescriptor[1],Length(TGADescriptor));
If TGAHead.ColorMapType=1 Then
   TGAWrite256Palette;
End;

Procedure TGAWriteBody(Var ResultStatus:Boolean);
Var
  Width_24:Word;
  I:Word;
Begin
Width_24:=MyWidth*3;
I:=0;
ResultStatus:=True;
Repeat
    Begin
    TempArrayD:=BitMap.ScanLine[I];
    Case MyPcxType Of
     1:Begin
       Case CurrBitsPerPixel Of
            1:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,1,8,MyWidth);
            4:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,4,8,MyWidth);
            8:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,8,8,MyWidth);
            End;
       BlockWrite(File1,TempArrayDBIG^[0],MyWidth);
       End;
     2:Begin
       Case CurrBitsPerPixel Of
            1:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,1,24,MyWidth);
            4:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,4,24,MyWidth);
            8:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,8,24,MyWidth);
           24:ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,24,24,MyWidth);
           End;
       BlockWrite(File1,TempArrayDBIG^[0],Width_24);
       End;
     End;
    Inc(I);
    End;
Until (I>=MyHeight) Or (ResultStatus=False);
End;

Begin
{
{ Write TARGA files.
}
SaveThePalette(BitMap.Palette,PaletteVGA);
MyWidth:=BitMap.Width;
MyHeight:=BitMap.Height;
Case BitMap.PixelFormat Of
     pf1bit:CurrBitsPerPixel:=1;
     pf4bit:CurrBitsPerPixel:=4;
     pf8bit:CurrBitsPerPixel:=8;
     pf24bit:CurrBitsPerPixel:=24;
     End;
GetMem(NewLine,MyWidth*4);
GetMem(TempArrayDBig,MyWidth*4);
Assign(File1,FileName);
ReWrite(File1,1);
TGAWriteHeader;
TGAWriteBody(ResultStatus);
If ResultStatus=False Then
   Begin
   Goto ErrExitClose;
   End;
ErrExitClose:;
Close(File1);
FreeMem(TempArrayDBig,MyWidth*4);
FreeMem(NewLine,MyWidth*4);
End;

{$IFNDEF TEEOCX}
{ TTGAImage }

{ TTGAImage }

procedure TTGAImage.LoadFromFile(const Filename: string);
begin
  LoadFromFileX(FileName,Self);
end;

initialization
  TPicture.RegisterFileFormat('tga','Targa',TTGAImage); // Do not localize
finalization
  TPicture.UnRegisterGraphicClass(TTGAImage);
{$ENDIF}
end.

