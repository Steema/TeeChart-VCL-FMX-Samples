�
 TEXPORTCHARTSTRING 0�  TPF0�TExportChartStringExportChartStringLeft�Top� VertScrollBar.Range 
AutoScrollCaption#Export Charts and Series as stringsPixelsPerInch`
TextHeight �	TSplitter	Splitter1LeftTop� Height{  �TMemoMemo1Lines.StringseCharts and Series and their data can be exported and imported as strings using the following methods: uses  TeeStore; .  Memo1.Lines.Text:=SaveChartToString(Chart1); /  LoadChartFromString(Chart1,Memo1.Lines.Text);    �TChartChart1Width�AlignalLeftColor	clBtnFaceColorPaletteIndex 
TBarSeriesSeries1ColorEachPoint	Marks.OnTop	XValues.NameXXValues.OrderloAscendingYValues.NameBarYValues.OrderloNoneData
5            x�@     ��@     ��@     ,�@     Б@     t�@   TPanelPanel2Left�Top� WidthHeight{AlignalLeftTabOrder TButtonButton1LeftTop WidthaHeightCaptionTo String -->TabOrder OnClickButton1Click  TButtonButton2LeftTopHWidthaHeightCaption<-- From StringEnabledTabOrderOnClickButton2Click   TPageControlPageControl1LeftTop� Width� Height{
ActivePage	TabSheet1AlignalClientTabOrder 	TTabSheet	TabSheet1CaptionChart and Data TMemoMemo2Left Top Width� Height_AlignalClient
ScrollBarsssBothTabOrder WordWrap   	TTabSheet	TabSheet2CaptionData
ImageIndex TMemoMemo3Left Top Width� Height_AlignalClient
ScrollBarsssBothTabOrder WordWrap     