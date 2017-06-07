unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSPack, DSPackTDCDSPFilter, ComCtrls, ToolWin, ExtCtrls, ImgList,
  StdCtrls, Buttons, DirectShow9, DSUtil, DirectShowUtils, dspConst, DCPluginRenderers;

type
  TfrmMain = class(TForm)
    DSPackDCDSPFilter: TDSPackDCDSPFilter;
    FilterGraph: TFilterGraph;
    VideoWindow: TVideoWindow;
    log: TMemo;
    StatusBar: TStatusBar;
    Panel1: TPanel;
    DSTrackBar: TDSTrackBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SoundLevel: TTrackBar;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    OpenDialog: TOpenDialog;
    tbPan: TTrackBar;
    lbFilters: TListBox;
    DCDSPPluginRenderer: TDCDSPPluginRenderer;
    DCVISPluginRenderer: TDCVISPluginRenderer;
    procedure FormCreate(Sender: TObject);
    procedure DSPackDCDSPFilterFlush(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SoundLevelChange(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FilterGraphDSEvent(sender: TComponent; Event, Param1,
      Param2: Integer);
    procedure DSTrackBarChange(Sender: TObject);
    procedure tbPanChange(Sender: TObject);
    procedure lbFiltersDblClick(Sender: TObject);
    procedure DSPackDCDSPFilterPCMData(Sender: TObject; Buffer: Pointer;
      Size: Integer; out NewSize: Integer; Stream: PDSStream);
    procedure DSPackDCDSPFilterMediaTypeChanged(Sender: TObject;
      Stream: PDSStream);
    procedure SpeedButton5Click(Sender: TObject);
    procedure VideoWindowResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VideoWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VideoWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VideoWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    fMouseDown : Boolean;
    cx, cy : integer;
    procedure EnumAllFilters;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  formPlugins;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DCDSPPluginRenderer.Plugins.Directory := ExtractFilePath(Application.ExeName) + '\plugins';
  DCVISPluginRenderer.Plugins.Directory := ExtractFilePath(Application.ExeName) + '\plugins';
  VideoWindow.Align := alClient;
end;

procedure TfrmMain.DSPackDCDSPFilterFlush(Sender: TObject);
begin
  // Callback : called when the Filter gets Flushed.
  DCDSPPluginRenderer.Plugins.Flush;
  log.Lines.Add(inttostr(log.Lines.Count+1) + ') DCDSPFilter - Flush');
end;

procedure TfrmMain.DSPackDCDSPFilterPCMData(Sender: TObject;
  Buffer: Pointer; Size: Integer; out NewSize: Integer; Stream: PDSStream);
begin
  // This is the main Callback for the DiretShow Filters Transform
  // function. Newsize will be the New Buffersize.
  NewSize := DCDSPPluginRenderer.Plugins.Process(Buffer,Size);
end;

procedure TfrmMain.DSPackDCDSPFilterMediaTypeChanged(Sender: TObject;
  Stream: PDSStream);
begin
  // Callback : called when a Mediatype has changed
  DCDSPPluginRenderer.Plugins.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  log.Lines.Add(inttostr(log.Lines.Count+1) + ') DCDSPFilter - MediaType Changed');
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FilterGraph.ClearGraph;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
  FilterGraph.Play;
end;

procedure TfrmMain.SpeedButton2Click(Sender: TObject);
begin
  FilterGraph.Pause;
end;

procedure TfrmMain.SpeedButton3Click(Sender: TObject);
begin
  FilterGraph.Stop;
end;

procedure TfrmMain.SoundLevelChange(Sender: TObject);
begin
  FilterGraph.Volume := 10000 + SetBasicAudioVolume(SoundLevel.Position);
end;

procedure TfrmMain.SpeedButton4Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DCVISPluginRenderer.Plugins.SetAudiofilter(nil);
    FilterGraph.Stop;
    FilterGraph.ClearGraph;
    FilterGraph.Active := False;
    VideoWindow.FilterGraph := FilterGraph;
    FilterGraph.Active := True;
    log.Clear;
    FilterGraph.RenderFile(OpenDialog.FileName);
    SoundLevel.Position := GetBasicAudioVolume(FilterGraph.Volume - 10000);
    StatusBar.Panels[1].Text := ExtractFileName(OpenDialog.FileName);
    if not IsVideoWindowConnected(VideoWindow) then VideoWindow.FilterGraph := nil;
    EnumAllFilters;
    DCVISPluginRenderer.Plugins.SetAudiofilter(DSPackDCDSPFilter as IBaseFilter);
    DCVISPluginRenderer.Plugins.SetMediaName(ExtractFileName(OpenDialog.FileName));
    FilterGraph.Play;
  end;
end;

procedure TfrmMain.FilterGraphDSEvent(sender: TComponent; Event, Param1,
  Param2: Integer);
begin
  log.Lines.Add(inttostr(log.Lines.Count+1) + ') ' + GetEventCodeDef(Event))
end;

procedure TfrmMain.DSTrackBarChange(Sender: TObject);
begin
  if FilterGraph.Active then StatusBar.Panels[0].Text := CalcTimeMStoString(DSTrackBar.Position * Int64(DSTrackBar.TimerInterval));
end;

procedure TfrmMain.tbPanChange(Sender: TObject);
begin
  FilterGraph.Balance := SetBasicAudioPan(tbPan.Position);
end;

procedure TfrmMain.EnumAllFilters;
var
  EnumFilters : IEnumFilters;
  Filter : IBaseFilter;
  Info : TFilterInfo;
  FileSource : IFileSourceFilter;
begin
  lbFilters.Clear;
  if (FilterGraph as IFilterGraph).EnumFilters(EnumFilters) = S_OK then
  begin
    while EnumFilters.Next(1,Filter,nil) = S_OK do
    begin
       if Filter.QueryFilterInfo(Info) = S_OK then
       begin
         if Filter.QueryInterface(IID_IFileSourceFilter,FileSource) = S_OK
           then lbFilters.items.add(ExtractFileName(Info.achName))
           else lbFilters.items.add(Info.achName);
       end;
    end;
  end;
  EnumFilters := nil;
  Filter := nil;
  FileSource := nil;
end;

procedure TfrmMain.lbFiltersDblClick(Sender: TObject);
var
  Filter : IBaseFilter;
begin
  if lbFilters.ItemIndex < 0 then Exit;
  if (FilterGraph as IFilterGraph).FindFilterByName(StringToOleStr(lbFilters.Items[lbFilters.itemindex]),Filter) = S_OK then
  begin
    ShowFilterPropertyPage(Handle,Filter);
    Filter := nil;
  end;
end;

procedure TfrmMain.SpeedButton5Click(Sender: TObject);
begin
  frmPlugins.Show;
end;

procedure TfrmMain.VideoWindowResize(Sender: TObject);
begin
  if Assigned(DCVISPluginRenderer.Plugins) then DCVISPluginRenderer.Plugins.Resize(VideoWindow.Width,VideoWindow.Height)
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  DCDSPPluginRenderer.Plugins.OwnerWindow := frmPlugins.pnlPlugins.Handle;
  DCVISPluginRenderer.Plugins.OwnerWindow := frmPlugins.pnlPlugins.Handle;
  DCVISPluginRenderer.Plugins.VISOwnerWindow := VideoWindow.Handle;
  frmPlugins.UpdateDSPPlugins;
end;

procedure TfrmMain.VideoWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    cx := x;
    cy := y;
    fMouseDown := True;
  end;  
end;

procedure TfrmMain.VideoWindowMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDown := False;
end;

procedure TfrmMain.VideoWindowMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if fMouseDown then
  begin
    Left := left + (x - cx);
    top := top + (y - cy);
  end;  
end;

end.
