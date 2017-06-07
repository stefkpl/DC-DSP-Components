unit Unit1;

interface

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DSPack,
  Menus, ExtCtrls, ComCtrls, StdCtrls,directshow9, OleServer, DSUtil,
  ImgList, ToolWin, shellapi, DSPackTDCDSPFilter, visWaveform, dspConst;

type
  TFormDVDPlayer = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    OpenMenu: TMenuItem;
    ExitMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    ControlMenu: TMenuItem;
    PlayMenu: TMenuItem;
    PauseMenu: TMenuItem;
    StopMenu: TMenuItem;
    ToolBar: TToolBar;
    btPreviousChapter: TToolButton;
    btRewind: TToolButton;
    btPause: TToolButton;
    btPlay: TToolButton;
    btStop: TToolButton;
    btFastForward: TToolButton;
    btNextChapter: TToolButton;
    ToolButton8: TToolButton;
    btRootMenu: TToolButton;
    btFullScreen: TToolButton;
    btFrameStep: TToolButton;
    ImageList: TImageList;
    StepForwardMenu: TMenuItem;
    OptionsMenu: TMenuItem;
    FullscreenMenu: TMenuItem;
    Popup: TPopupMenu;
    N1: TMenuItem;
    GoToMenu: TMenuItem;
    PlaySpeed1: TMenuItem;
    NextChapterMenu: TMenuItem;
    PreviousChapterMenu: TMenuItem;
    FastForward1: TMenuItem;
    Rewind1: TMenuItem;
    N4: TMenuItem;
    MenuRoot: TMenuItem;
    TitleMenu: TMenuItem;
    N5: TMenuItem;
    BookmarksMenu: TMenuItem;
    SavebookmarkMenu: TMenuItem;
    RestorebookmarkMenu: TMenuItem;
    Play2: TMenuItem;
    Pause2: TMenuItem;
    Stop2: TMenuItem;
    N2: TMenuItem;
    GoTo2: TMenuItem;
    NextChapter2: TMenuItem;
    PreviousChapter2: TMenuItem;
    PlaySpeed2: TMenuItem;
    FastForward2: TMenuItem;
    Rewind2: TMenuItem;
    N3: TMenuItem;
    MenuRoot2: TMenuItem;
    TitleMenu2: TMenuItem;
    Fullscreen2: TMenuItem;
    N6: TMenuItem;
    Bookmarks2: TMenuItem;
    Savebookmark2: TMenuItem;
    Restorebookmark2: TMenuItem;
    StatusBar: TStatusBar;
    N7: TMenuItem;
    progdigycom1: TMenuItem;
    VideoWindow: TVideoWindow;
    FilterGraph: TFilterGraph;
    DSPackDCDSPFilter: TDSPackDCDSPFilter;
    DCWaveform: TDCWaveform;
    Panel1: TPanel;
    pbVisual: TPaintBox;
    procedure PlayMenuClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btFullScreenClick(Sender: TObject);
    procedure btFrameStepClick(Sender: TObject);
    procedure btRootMenuClick(Sender: TObject);
    procedure VideoWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VideoWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btNextChapterClick(Sender: TObject);
    procedure btPreviousChapterClick(Sender: TObject);
    procedure btFastForwardClick(Sender: TObject);
    procedure btRewindClick(Sender: TObject);
    procedure TitleMenuClick(Sender: TObject);
    procedure SavebookmarkMenuClick(Sender: TObject);
    procedure RestorebookmarkMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    procedure FilterGraphDVDTitleChange(sender: TObject; title: Integer);
    procedure FilterGraphDVDChapterStart(sender: TObject;
      chapter: Integer);
    procedure FilterGraphDVDCurrentHMSFTime(sender: TObject;
      HMSFTimeCode: tagDVD_HMSF_TIMECODE; TimeCode: tagDVD_TIMECODE);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DCWaveformWaveformData(Sender: TObject; Data: PVisualBuffer;
      MinY, MaxY, NumSamples, Channels: Integer);
    procedure DSPackDCDSPFilterPCMData(Sender: TObject; Buffer: Pointer;
      Size: Integer; out NewSize: Integer; Stream: PDSStream);
    procedure DSPackDCDSPFilterVisualData(Sender: TObject; Buffer: Pointer;
      Size: Integer; Stream: PDSStream);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    BackBMP : TBitmap;
    procedure PlayDVD(FileName: WideString);
  end;

var
  FormDVDPlayer: TFormDVDPlayer;

implementation

{$R *.DFM}

procedure TFormDVDPlayer.PlayDVD(FileName: WideString);
var
  Status : TAMDVDRenderStatus;
  DvdCmd: IDvdCmd;
  HR: HRESULT;
  EnumFilters : IEnumFilters;
  Filter : IBaseFilter;
  EnumPins : IEnumPins;
  Pin : IPin;
  ClassID : TGUID;
  RendererInput : IPin;
  RendererConnectedOutput : IPin;
  DCDSPFilterInput : IPin;
  DCDSPFilterOutput : IPin;
  PinDirection : TPinDirection;
begin
  // Activate the filter Graph
  if not FilterGraph.Active then
  begin
    // The DVD Graph removes DC-DSP Filter from the Graph.
    // To get it work it´s needed to render the DVD without my
    // Filter and connect it after the rendering succeeded.
    DSPackDCDSPFilter.FilterGraph := nil;

    FilterGraph.Active := true;
    // Render DVD
    HR := FilterGraph.RenderDvd(Status, FileName);
    if HR <> S_OK then
    begin
      case HR of
        HRESULT(E_INVALIDARG): Application.MessageBox('Invalid Argument.','Error', mb_ok);
        HRESULT(S_FALSE)     : begin
                        if Status.hrVPEStatus <> 0 then Application.MessageBox(PChar(GetErrorString(Status.hrVPEStatus)), 'Error', mb_OK);
                        if Status.bDvdVolInvalid   then Application.MessageBox('The specified DVD volume to be played does not exist.', 'Error', mb_OK);
                        if Status.bDvdVolUnknown   then Application.MessageBox('No DVD volume is specified or isn''t found.', 'Error', mb_OK);
                        if Status.bNoLine21In      then Application.MessageBox('The video decoder doesn''t produce line 21 (closed captioning) data.', 'Error', mb_OK);
                        if Status.bNoLine21Out     then Application.MessageBox('The video decoder can''t be shown as closed captioning on video due to a problem with graph building.', 'Error', mb_OK);
                        if status.iNumStreamsFailed > 0 then Application.MessageBox('Can''t render one or more stream.', 'Error', mb_OK);
                      end;
        HRESULT(VFW_E_DVD_DECNOTENOUGH) : Application.MessageBox('There isn''t enough hardware or software decoders to decode all streams.','Error', mb_OK);
        HRESULT(VFW_E_DVD_RENDERFAIL)   : Application.MessageBox('Some basic error occurred in building the graph.'#13'Possibilities include the DVD Navigator filter or the video or audio renderer not instantiating,'#13'a trivial connection or pin enumeration failing, or none of the streams rendering.','Error', mb_OK);
      end;
      FilterGraph.ClearGraph;
      exit;
    end;
    // After the rendering succeeded add my Filter to the Graph and
    // connect it to the DirectSound Device.
    DSPackDCDSPFilter.FilterGraph := FilterGraph;
    if (FilterGraph as IFilterGraph).EnumFilters(EnumFilters) = S_OK then
    begin
      while EnumFilters.Next(1,Filter,nil) = S_OK do
      begin
        if Filter.GetClassID(ClassID) = S_OK then
        begin
          if IsEqualGUID(ClassID,CLSID_AudioRender) or IsEqualGUID(ClassID,CLSID_DSoundRender) then
          begin
            Filter.EnumPins(EnumPins);
            EnumPins.Next(1,RendererInput,nil);
            RendererInput.ConnectedTo(RendererConnectedOutput);
            RendererInput.Disconnect;
            RendererConnectedOutput.Disconnect;
            (DSPackDCDSPFilter as IBaseFilter).EnumPins(EnumPins);
            while EnumPins.Next(1,Pin,nil) = S_OK do
            begin
              Pin.QueryDirection(PinDirection);
              case PinDirection of
                PINDIR_INPUT : DCDSPFilterInput := Pin;
                PINDIR_OUTPUT : DCDSPFilterOutput := Pin;
              end;
            end;
            RendererConnectedOutput.Connect(DCDSPFilterInput,nil);
            DCDSPFilterOutput.Connect(RendererInput,nil);
            break;
          end;
        end;
      end;
    end;
    EnumFilters := nil;
    Filter := nil;
    EnumPins := nil;
    Pin := nil;
    RendererInput := nil;
    RendererConnectedOutput := nil;
    DCDSPFilterInput := nil;
    DCDSPFilterOutput := nil;

    FilterGraph.Play;
    with FilterGraph as IDVDControl2 do
    begin
      SetOption(DVD_NotifyParentalLevelChange,false); //not notify us when parental level changes
      SetOption(DVD_HMSF_TimeCodeEvents, true);       // use new HMSF timecode format
    end;
  end
  else
  begin
    FilterGraph.Play;
    with FilterGraph as IDvdControl2 do PlayForwards(1.0,DVD_CMD_FLAG_None, DvdCmd);
  end;
end;

procedure TFormDVDPlayer.PlayMenuClick(Sender: TObject);
begin
  PlayDVD('');
end;

procedure TFormDVDPlayer.btStopClick(Sender: TObject);
begin
  FilterGraph.Stop;
end;

procedure TFormDVDPlayer.btPauseClick(Sender: TObject);
begin
  FilterGraph.Pause;
end;

procedure TFormDVDPlayer.btFullScreenClick(Sender: TObject);
begin
  VideoWindow.FullScreen := not VideoWindow.FullScreen;
  btFullScreen.Down := VideoWindow.FullScreen;
end;

procedure TFormDVDPlayer.btFrameStepClick(Sender: TObject);
begin
  if FilterGraph.Active then
    with FilterGraph as IVideoFrameStep do Step(1,nil);
end;

procedure TFormDVDPlayer.btRootMenuClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do ShowMenu(DVD_MENU_Root, 0, DvdCmd);
end;

procedure TFormDVDPlayer.VideoWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var Point : TPoint;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do
    begin
      Point.x := x;
      Point.y := y;
      SelectAtPosition(Point);
    end;
end;

procedure TFormDVDPlayer.VideoWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Point : TPoint;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do
    begin
      Point.x := x;
      Point.y := y;
      ActivateAtPosition(Point);
    end;
end;

procedure TFormDVDPlayer.btNextChapterClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do PlayNextChapter(DVD_CMD_FLAG_None, DvdCmd);
end;

procedure TFormDVDPlayer.btPreviousChapterClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do PlayPrevChapter(DVD_CMD_FLAG_None, DvdCmd);
end;


procedure TFormDVDPlayer.btFastForwardClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do PlayForwards(8.0,DVD_CMD_FLAG_None, DvdCmd);
end;

procedure TFormDVDPlayer.btRewindClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do PlayBackwards(8.0,DVD_CMD_FLAG_None, DvdCmd);
end;

procedure TFormDVDPlayer.TitleMenuClick(Sender: TObject);
var DvdCmd: IDvdCmd;
begin
  if FilterGraph.Active then
    with FilterGraph as IDvdControl2 do ShowMenu(DVD_MENU_Title, DVD_CMD_FLAG_None, DvdCmd);
end;

procedure TFormDVDPlayer.SavebookmarkMenuClick(Sender: TObject);
begin
  FilterGraph.DVDSaveBookmark(ExtractFilePath(Application.EXEName)+'bookmark.bmk');
end;

procedure TFormDVDPlayer.RestorebookmarkMenuClick(Sender: TObject);
begin
  FilterGraph.DVDRestoreBookmark(ExtractFilePath(Application.EXEName)+'bookmark.bmk');
end;

procedure TFormDVDPlayer.ExitMenuClick(Sender: TObject);
begin
  FilterGraph.ClearGraph;
  FilterGraph.Active := false;
  Application.Terminate;
end;

procedure TFormDVDPlayer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  ExitMenuClick(nil)
end;

procedure TFormDVDPlayer.OpenMenuClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FilterGraph.ClearGraph;
    FilterGraph.Active := false;
    PlayDVD(OpenDialog.FileName);
  end;

end;

procedure TFormDVDPlayer.FilterGraphDVDTitleChange(sender: TObject;
  title: Integer);
begin
  statusbar.Panels.Items[1].Text := 'Title '+inttostr(title);
end;

procedure TFormDVDPlayer.FilterGraphDVDChapterStart(sender: TObject;
  chapter: Integer);
begin
  statusbar.Panels.Items[2].Text := 'Chapter '+ inttostr(chapter);
end;

procedure TFormDVDPlayer.FilterGraphDVDCurrentHMSFTime(sender: TObject;
  HMSFTimeCode: tagDVD_HMSF_TIMECODE; TimeCode: tagDVD_TIMECODE);
begin
  with HMSFTimeCode do
    StatusBar.Panels.Items[0].Text :=
      format('%d:%d:%d',[bHours, bMinutes ,bSeconds]);
end;

procedure TFormDVDPlayer.FormCreate(Sender: TObject);
begin
  BackBMP := TBitmap.Create;
  BackBMP.PixelFormat := pfDevice;
  BackBMP.Height := pbVisual.Height;
  BackBMP.Width := pbVisual.Width;
end;

procedure TFormDVDPlayer.FormDestroy(Sender: TObject);
begin
  BackBMP.Free;
end;

procedure TFormDVDPlayer.DCWaveformWaveformData(Sender: TObject;
  Data: PVisualBuffer; MinY, MaxY, NumSamples, Channels: Integer);
var
  i : integer;
begin
  BackBMP.Canvas.Pen.Color := clBlack;
  BackBMP.Canvas.Brush.Color := clBlack;
  BackBMP.Canvas.Rectangle(BackBMP.Canvas.ClipRect);
  BackBMP.Canvas.Pen.Color := clRed;
  BackBMP.Canvas.Brush.Color := clRed;
  for i := 0 to BackBMP.Width -1 -1 do
  begin
    if i = 0 then BackBMP.Canvas.MoveTo(i,Data^[0,i])
             else BackBMP.Canvas.LineTo(i,Data^[0,i]);
  end;
  pbVisual.Canvas.Draw(0,0,BackBMP);
end;

procedure TFormDVDPlayer.DSPackDCDSPFilterPCMData(Sender: TObject;
  Buffer: Pointer; Size: Integer; out NewSize: Integer; Stream: PDSStream);
begin
  NewSize := Size;
end;

procedure TFormDVDPlayer.DSPackDCDSPFilterVisualData(Sender: TObject;
  Buffer: Pointer; Size: Integer; Stream: PDSStream);
begin
  DCWaveform.NumSamples := pbVisual.Width;
  BackBMP.Width := pbVisual.Width;
  DCWaveform.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
end;

end.
