unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ActiveX, DirectShow9, DSUtil,
  dmoConst, DirectShowUtils, visWaveform, dspConst, DCDSPFilterInterfaces;

type
  TfrmMain = class(TForm)
    pnlVideo: TPanel;
    lbFilters: TListBox;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    tbVolume: TTrackBar;
    tbPan: TTrackBar;
    tbPosition: TTrackBar;
    OpenDialog1: TOpenDialog;
    tbAmplify: TTrackBar;
    eq0: TTrackBar;
    eq1: TTrackBar;
    eq2: TTrackBar;
    eq3: TTrackBar;
    eq4: TTrackBar;
    eq5: TTrackBar;
    eq6: TTrackBar;
    eq7: TTrackBar;
    eq8: TTrackBar;
    eq9: TTrackBar;
    pbVisual: TProgressBar;
    tmrVisual: TTimer;
    DCWaveform: TDCWaveform;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure pnlVideoResize(Sender: TObject);
    procedure lbFiltersDblClick(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure tbPanChange(Sender: TObject);
    procedure tbPositionChange(Sender: TObject);
    procedure tbAmplifyChange(Sender: TObject);
    procedure OnEqualizerChange(Sender: TObject);
    procedure tmrVisualTimer(Sender: TObject);
    procedure DCWaveformWaveformData(Sender: TObject; Data: PVisualBuffer;
      MinY, MaxY, NumSamples, Channels: Integer);
  private
    fGraph : IGraphBuilder;
    iBA : IBasicAudio;
    iMC : IMediaControl;
    iMS : IMediaSeeking;
    iVW : IVideoWindow;
    iDCDSPFilterControl: IDCDSPFilterInterface;
    iDCDSPFilterVisual: IDCDSPFilterVisualInterface;
    iAmplify : IDCAmplify;
    iEqualizer : IDCEqualizer;
    procedure ClearDirectShow;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // initialize COM
  CoInitialize(nil);

  // setup Component alignment
  lbfilters.Align := alRight;
  pnlVideo.Align := alClient;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ClearDirectShow;
  CoUninitialize;
end;

procedure TfrmMain.ClearDirectShow;
begin
  if Assigned(fGraph) then
  begin
    tmrVisual.Enabled := False;
    iEqualizer := nil;
    iAmplify := nil;
    iBA := nil;
    iMC := nil;
    iMS := nil;
    iVW := nil;
    iDCDSPFilterControl := nil;
    iDCDSPFilterVisual := nil;
    fGraph := nil;
    tbVolume.Enabled := False;
    tbPan.Enabled := False;
    tbPosition.Position := 0;
    tbPosition.Enabled := False;
    lbFilters.Clear;
  end;
end;

procedure TfrmMain.SpeedButton4Click(Sender: TObject);
var
  hr : HRESULT;
  SeekCaps : Cardinal;
  Duration : Int64;
  EnumFilters: IEnumFilters;
  Filter: IBaseFilter;
  FilterInfo: TFilterInfo;
begin
  if OpenDialog1.Execute then
  begin
    ClearDirectShow;
    // create the Graph Builder
    hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, fGraph);
    if (hr <> S_OK) then
    begin
      ShowMessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
      ClearDirectShow;
      Exit;
    end;

    // Render the File
    hr := fGraph.RenderFile(StringToOleStr(OpenDialog1.FileName),nil);
    if (hr <> S_OK) then
    begin
      case hr of
        VFW_S_PARTIAL_RENDER:
        begin
          ShowMessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
        end else
        begin
          ShowMessage(GetErrorString(hr) + ' (Error Code : ' + inttohex(hr,8) + ')');
          ClearDirectShow;
          Exit;
        end;
      end;
    end;

    // Query for needed Interfaces
    fGraph.QueryInterface(IID_IMediaControl,iMC);
    fGraph.QueryInterface(IID_IBasicAudio,iBA);
    fGraph.QueryInterface(IID_IMediaSeeking,iMS);
    fGraph.QueryInterface(IID_IVideoWindow,iVW);

    // check whether seeking is supported
    if Assigned(iMS) then
    begin
      iMS.GetDuration(Duration);
      tbPosition.Max := Duration div One_Second;
      iMS.GetCapabilities(SeekCaps);
      tbPosition.Enabled := (SeekCaps and AM_SEEKING_CanSeekAbsolute) <> 0;
    end;

    // check if changing Volume/Balance is supported
    if Assigned(iBA) then
    begin
      tbVolume.Enabled := True;
      tbPan.Enabled := True;
      tbVolumeChange(Self);
      tbPanChange(Self);
    end;

    // check if a Video is present
    if Assigned(iVW) then
    begin
      iVW.put_Owner(pnlVideo.Handle);
      iVW.put_WindowStyle(WS_CHILD or WS_CLIPSIBLINGS);
      iVW.put_WindowStyleEx(0);
      iVW.put_MessageDrain(Handle);
      pnlVideoResize(Self);
      iVW.put_Visible(True);
    end;

    // now get the available Filters
    if fGraph.EnumFilters(EnumFilters) = S_OK then
      while EnumFilters.Next(1,Filter,nil) = S_OK do
        if Filter.QueryFilterInfo(FilterInfo) = S_OK then
          lbFilters.Items.Add(FilterInfo.achName);
    EnumFilters := nil;
    Filter := nil;


    // Check whether DC-DSP Filter is used
    if fGraph.EnumFilters(EnumFilters) = S_OK then
    begin
      while EnumFilters.Next(1,Filter,nil) = S_OK do
      begin
        if Filter.QueryInterface(IID_DCDSPFilter, iDCDSPFilterControl) = S_OK then
        begin
          Filter.QueryInterface(IID_DCDSPFilterVisual, iDCDSPFilterVisual);
          Filter := nil;
          break;
        end;
        Filter := nil;
      end;
      EnumFilters := nil;
    end;

    if Assigned(iDCDSPFilterControl) then
    begin
      // Disable savings to the Registry when exiting
      iDCDSPFilterControl.set_DisableSaving(True);
      // now remove all DSP Filters
      iDCDSPFilterControl.set_RemoveAllFilters;
      // and add Amplify and Equalizer to it.
      // -1 adds the Filter on the last Position 
      iDCDSPFilterControl.set_AddFilter(-1,ftAmplify);
      iDCDSPFilterControl.set_AddFilter(-1,ftEqualizer);

      // get Filter Classes and enable them
      // Index 0 because it´s the first Filter we added
      iDCDSPFilterControl.get_FilterInterface(0,iAmplify);
      if Assigned(iAmplify) then iAmplify.set_Enabled(True);
      // Index 1 because it´s the second Filter we added
      iDCDSPFilterControl.get_FilterInterface(1,iEqualizer);
      if Assigned(iEqualizer) then iEqualizer.set_Enabled(True);
      tbAmplifyChange(Self);
      OnEqualizerChange(Self);
      tmrVisual.Enabled := True;
    end;
    // finally Run the Graph
    iMC.Run;
  end;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Run;
end;

procedure TfrmMain.SpeedButton2Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Pause;
end;

procedure TfrmMain.SpeedButton3Click(Sender: TObject);
begin
  if Assigned(iMC) then iMC.Stop;
end;

procedure TfrmMain.pnlVideoResize(Sender: TObject);
var
  rcWnd : TRect;
begin
  if Assigned(iVW) then
  begin
    Windows.GetClientRect(pnlVideo.Handle, rcWnd);
    iVW.SetWindowPosition(rcwnd.Left,rcwnd.Top,rcwnd.Right - rcwnd.Left,rcwnd.Bottom - rcwnd.Top);
  end;  
end;

procedure TfrmMain.lbFiltersDblClick(Sender: TObject);
var
  Filter : IBaseFilter;
begin
  if lbFilters.ItemIndex < 0 then Exit;
  if fGraph.FindFilterByName(StringToOleStr(lbFilters.Items[lbFilters.itemindex]),Filter) = S_OK then
  begin
    ShowFilterPropertyPage(Handle,Filter);
    Filter := nil;
  end;
end;

procedure TfrmMain.tbVolumeChange(Sender: TObject);
begin
  if Assigned(iBA) then iBA.put_Volume(SetBasicAudioVolume(tbVolume.Position));
end;

procedure TfrmMain.tbPanChange(Sender: TObject);
begin
  if Assigned(iBA) then iBA.put_Balance(SetBasicAudioPan(tbPan.Position));
end;

procedure TfrmMain.tbPositionChange(Sender: TObject);
var
  Pos : Int64;
begin
  if Assigned(iMS) then
  begin
    Pos := tbPosition.Position;
    Pos := Pos * One_Second;
    iMS.SetPositions(Pos,AM_SEEKING_AbsolutePositioning,Pos,AM_SEEKING_NoPositioning);
  end;
end;

procedure TfrmMain.tbAmplifyChange(Sender: TObject);
begin
  if Assigned(iAmplify) then iAmplify.set_Volume(0, tbAmplify.Max - tbAmplify.Position);
end;

procedure TfrmMain.OnEqualizerChange(Sender: TObject);
const
  NumEQBands = 10;
  EQFreq: array[0..NumEQBands] of Word = (
    0, 3, 9, 16, 29, 48, 100, 141, 280, 559, 1024
  );
var
  z, c : integer;
  tb : TTrackBar;
begin
  if Assigned(iEqualizer) then
  begin
    for c := 0 to NumEQBands -1 do
    begin
      // EQFreq is a default Frequency Band for a 10 Band Equalizer.
      // you can use your own constans to modify the Frequency Range of a Band.
      // Thus you are also able to extend the Equalizer to 20 Bands if you need it.
      tb := FindComponent('eq'+inttostr(c)) as TTrackBar;
      for z := EQFreq[c] to EQFreq[c+1] -1 do
        iEqualizer.set_Band(0,z,0 - tb.Position);
    end;
  end;
end;

procedure TfrmMain.tmrVisualTimer(Sender: TObject);
var
  Stream : PDSStream;
  Buffer : Pointer;
  Size : integer;
begin
  if Assigned(iDCDSPFilterVisual) then
  begin
    if iDCDSPFilterVisual.get_VisualData(Buffer,Size,Stream) = S_OK then
    begin
      DCWaveform.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    end else pbVisual.Position := 0;
  end;
end;

procedure TfrmMain.DCWaveformWaveformData(Sender: TObject;
  Data: PVisualBuffer; MinY, MaxY, NumSamples, Channels: Integer);
var
  i : integer;
  z : integer;
  a : integer;
begin
  z := 0;
  for i := 0 to NumSamples -1 do
  begin
    a := abs(Data[0,i] - 100);
    if a > z then z := a;
  end;
  pbVisual.Position := z;
end;

end.
