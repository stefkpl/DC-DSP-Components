unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSPack, DSPackTDCDSPFilter, ComCtrls, ToolWin, ExtCtrls, ImgList,
  StdCtrls, Buttons, Menus, waVisualWrapper, waDSPWrapper, visWaveform,
  visSpectrum, dmoWavesReverb, dmoParamEQ, dmoI3DL2Reverb, dmoGargle, dmoFlanger,
  dmoEcho, dmoDistortion, dmoCompressor, dmoChorus, dspTrueBass, dspTempo,
  dspSound3D, dspPitchShift, dspPitchScale, DirectShow9, dspPhaser, dspPhaseInvert,
  dspNotch, dspLowpass, dspHighpass, dspFlanger, dspEqualizer, dspEchoDelay,
  dspDynamicAmplify, dspDownMix, dspCompressor, dspChannelOrder, dspBandpass,
  dspAmplify, DSUtil, DirectShowUtils, dspConst, dspTrebleEnhancer,
  dmoBaseFilter, dspParametricEQ;

type
  TVisualType = (vtNone, vtSpectrum, vtWaveform);

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
    Panel2: TPanel;
    pbVisual: TPaintBox;
    DSPPopup: TPopupMenu;
    Equalizer1: TMenuItem;
    Amplify1: TMenuItem;
    OpenDialog: TOpenDialog;
    DCAmplify: TDCAmplify;
    DCBandpass: TDCBandpass;
    DCChannelOrder: TDCChannelOrder;
    DCDownMix: TDCDownMix;
    DCDynamicAmplify: TDCDynamicAmplify;
    DCEchoDelay: TDCEchoDelay;
    DCEqualizer: TDCEqualizer;
    DCFlanger: TDCFlanger;
    DCHighpass: TDCHighpass;
    DCLowpass: TDCLowpass;
    DCNotch: TDCNotch;
    DCPhaseInvert: TDCPhaseInvert;
    DCPhaser: TDCPhaser;
    DCPitchScale: TDCPitchScale;
    DCPitchShift: TDCPitchShift;
    DCSound3D: TDCSound3D;
    DCTempo: TDCTempo;
    DCTrueBass: TDCTrueBass;
    DCDMOChorus: TDCDMOChorus;
    DCDMOCompressor: TDCDMOCompressor;
    DCDMOEcho: TDCDMOEcho;
    DCDMOFlanger: TDCDMOFlanger;
    DCDMOGargle: TDCDMOGargle;
    DCWAVisualWrapper: TDCWAVisualWrapper;
    VisualPopup: TPopupMenu;
    None1: TMenuItem;
    Spectrum1: TMenuItem;
    Waveform1: TMenuItem;
    DMO1: TMenuItem;
    Chorus1: TMenuItem;
    Compressor1: TMenuItem;
    Distortion1: TMenuItem;
    Echo1: TMenuItem;
    Flanger1: TMenuItem;
    Gargle1: TMenuItem;
    I3DL2Reverb1: TMenuItem;
    ParamEQ1: TMenuItem;
    WavesReverb1: TMenuItem;
    Internal1: TMenuItem;
    Bandpass1: TMenuItem;
    ChannelOrder1: TMenuItem;
    Compressor2: TMenuItem;
    DownMixMono1: TMenuItem;
    DynamicAmplify1: TMenuItem;
    EchoDelay1: TMenuItem;
    Flanger2: TMenuItem;
    Highpass1: TMenuItem;
    Lowpass1: TMenuItem;
    Notch1: TMenuItem;
    PhaseInvert1: TMenuItem;
    Phaser1: TMenuItem;
    PitchScale1: TMenuItem;
    PitchShift1: TMenuItem;
    Sound3D1: TMenuItem;
    empo1: TMenuItem;
    rueBass1: TMenuItem;
    Winamp21: TMenuItem;
    VisualPlugins1: TMenuItem;
    DSPPlugins1: TMenuItem;
    tbPan: TTrackBar;
    lbFilters: TListBox;
    DCDMODistortion: TDCDMODistortion;
    DCSpectrum: TDCSpectrum;
    DCWaveform: TDCWaveform;
    DCWADSPWrapper: TDCWADSPWrapper;
    DCDMOParamEQ: TDCDMOParamEQ;
    DCDMOWavesReverb: TDCDMOWavesReverb;
    DCDMOI3DL2Reverb: TDCDMOI3DL2Reverb;
    DCCompressor: TDCCompressor;
    rebleEnhancer1: TMenuItem;
    DCTrebleEnhancer: TDCTrebleEnhancer;
    DCParametricEQ: TDCParametricEQ;
    ParametricEQ1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DSPackDCDSPFilterFlush(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SoundLevelChange(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FilterGraphDSEvent(sender: TComponent; Event, Param1,
      Param2: Integer);
    procedure VisualPopupPopup(Sender: TObject);
    procedure None1Click(Sender: TObject);
    procedure DSTrackBarChange(Sender: TObject);
    procedure Amplify1Click(Sender: TObject);
    procedure Chorus1Click(Sender: TObject);
    procedure Compressor1Click(Sender: TObject);
    procedure Distortion1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DCWaveformWaveformData(Sender: TObject; Data: PVisualBuffer;
      MinY, MaxY, NumSamples, Channels: Integer);
    procedure DCSpectrumSpectrumData(Sender: TObject; Data: PVisualBuffer;
      MinY, MaxY, NumSamples, Channels: Integer);
    procedure pbVisualPaint(Sender: TObject);
    procedure pbVisualClick(Sender: TObject);
    procedure Echo1Click(Sender: TObject);
    procedure Flanger1Click(Sender: TObject);
    procedure Gargle1Click(Sender: TObject);
    procedure I3DL2Reverb1Click(Sender: TObject);
    procedure ParamEQ1Click(Sender: TObject);
    procedure WavesReverb1Click(Sender: TObject);
    procedure Bandpass1Click(Sender: TObject);
    procedure ChannelOrder1Click(Sender: TObject);
    procedure Compressor2Click(Sender: TObject);
    procedure DownMixMono1Click(Sender: TObject);
    procedure DynamicAmplify1Click(Sender: TObject);
    procedure EchoDelay1Click(Sender: TObject);
    procedure Equalizer1Click(Sender: TObject);
    procedure Flanger2Click(Sender: TObject);
    procedure Highpass1Click(Sender: TObject);
    procedure Lowpass1Click(Sender: TObject);
    procedure Notch1Click(Sender: TObject);
    procedure PhaseInvert1Click(Sender: TObject);
    procedure Phaser1Click(Sender: TObject);
    procedure PitchScale1Click(Sender: TObject);
    procedure PitchShift1Click(Sender: TObject);
    procedure Sound3D1Click(Sender: TObject);
    procedure empo1Click(Sender: TObject);
    procedure rueBass1Click(Sender: TObject);
    procedure VisualPlugins1Click(Sender: TObject);
    procedure DSPPlugins1Click(Sender: TObject);
    procedure tbPanChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFiltersDblClick(Sender: TObject);
    procedure DSPackDCDSPFilterPCMData(Sender: TObject; Buffer: Pointer;
      Size: Integer; out NewSize: Integer; Stream: PDSStream);
    procedure DSPackDCDSPFilterVisualData(Sender: TObject; Buffer: Pointer;
      Size: Integer; Stream: PDSStream);
    procedure DSPackDCDSPFilterMediaTypeChanged(Sender: TObject;
      Stream: PDSStream);
    procedure rebleEnhancer1Click(Sender: TObject);
    procedure ParametricEQ1Click(Sender: TObject);
  private
    { Private declarations }
    BackBuffer : TBitmap;
    VisualType : TVisualType;
    Peaks : array[0..255] of integer;
    procedure EnumAllFilters;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  formDMOChorus, formDMOCompressor, formDMODistortion,
  formDMOEcho, formDMOFlanger, formDMOGargle, formDMOI3DL2Reverb,
  formDMOParamEQ, formDMOWavesReverb, formDSPAmplify, formDSPBandpass,
  formDSPChannelOrder, formDSPCompressor, formDSPDownMix,
  formDSPDynamicAmplify, formDSPEchoDelay, formDSPEqualizer,
  formDSPFlanger, formDSPHighpass, formDSPLowpass, formDSPNotch,
  formDSPPhaser, formDSPPitchScale, formDSPPitchShift, formDSPSound3D,
  formDSPTempo, formDSPTrueBass, formDSPPhaseInvert, formWA2VisualPlugins,
  formWA2DSPPlugins, formDSPTrebleEnhancer, formDSPParametricEQ;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  VideoWindow.Align := alClient;
  BackBuffer := TBitmap.Create;
  BackBuffer.Width := pbVisual.Width;
  BackBuffer.Height := pbVisual.Height;
  BackBuffer.PixelFormat := pfDevice;
  BackBuffer.Canvas.Pen.Color := clBlack;
  BackBuffer.Canvas.Brush.Color := clBlack;
  BackBuffer.Canvas.Rectangle(BackBuffer.Canvas.ClipRect);
  VisualType := vtSpectrum;
end;

procedure TfrmMain.SpeedButton5MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
begin
  if (Button = mbLeft) then
  begin
    p := ClientToScreen(Point(SpeedButton5.Left,SpeedButton5.Top));
    DSPPopup.Popup(p.X,p.Y + Panel1.Top - 55);
  end;
end;

procedure TfrmMain.DSPackDCDSPFilterFlush(Sender: TObject);
begin
  // Callback : called when the Filter gets Flushed.
  log.Lines.Add(inttostr(log.Lines.Count+1) + ') DCDSPFilter - Flush');
  DCBandpass.Flush;
  DCEchoDelay.Flush;
  DCEqualizer.Flush;
  DCFlanger.Flush;
  DCHighpass.Flush;
  DCLowpass.Flush;
  DCNotch.Flush;
  DCParametricEQ.Flush;
  DCPhaser.Flush;
  DCPitchScale.Flush;
  DCTempo.Flush;
  DCTrebleEnhancer.Flush;
  DCTrueBass.Flush;

  DCDMOChorus.Flush;
  DCDMOCompressor.Flush;
  DCDMODistortion.Flush;
  DCDMOEcho.Flush;
  DCDMOFlanger.Flush;
  DCDMOGargle.Flush;
  DCDMOI3DL2Reverb.Flush;
  DCDMOParamEQ.Flush;
  DCDMOWavesReverb.Flush;
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
  FilterGraph.Volume := SoundLevel.Position;
end;

procedure TfrmMain.SpeedButton4Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FilterGraph.Stop;
    FilterGraph.ClearGraph;
    FilterGraph.Active := False;
    VideoWindow.FilterGraph := FilterGraph;
    FilterGraph.Active := True;
    log.Clear;
    FilterGraph.RenderFile(OpenDialog.FileName);
    SoundLevel.Position := GetBasicAudioVolume(FilterGraph.Volume - 10000);
    StatusBar.Panels[1].Text := ExtractFileName(OpenDialog.FileName);
    EnumAllFilters;
    FilterGraph.Play;
  end;
end;

procedure TfrmMain.FilterGraphDSEvent(sender: TComponent; Event, Param1,
  Param2: Integer);
begin
  log.Lines.Add(inttostr(log.Lines.Count+1) + ') ' + GetEventCodeDef(Event))
end;

procedure TfrmMain.VisualPopupPopup(Sender: TObject);
begin
  None1.Checked := VisualType = vtNone;
  Spectrum1.Checked := VisualType = vtSpectrum;
  Waveform1.Checked := VisualType = vtWaveform;
end;

procedure TfrmMain.None1Click(Sender: TObject);
begin
  VisualType := TVisualType((Sender as TMenuItem).Tag);
  BackBuffer.Canvas.Pen.Color := clBlack;
  BackBuffer.Canvas.Brush.Color := clBlack;
  BackBuffer.Canvas.Rectangle(BackBuffer.Canvas.ClipRect);
  pbVisual.Canvas.Draw(0,0,BackBuffer);
end;

procedure TfrmMain.DSTrackBarChange(Sender: TObject);
begin
  if FilterGraph.Active then StatusBar.Panels[0].Text := CalcTimeMStoString(DSTrackBar.Position * Int64(DSTrackBar.TimerInterval));
end;

procedure TfrmMain.Amplify1Click(Sender: TObject);
begin
  frmDSPAmplify.ShowModal;
end;

procedure TfrmMain.Chorus1Click(Sender: TObject);
begin
  frmDMOChorus.ShowModal;
end;

procedure TfrmMain.Compressor1Click(Sender: TObject);
begin
  frmDMOCompressor.ShowModal;
end;

procedure TfrmMain.Distortion1Click(Sender: TObject);
begin
  frmDMODistortion.ShowModal;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  BackBuffer.Free;
end;

procedure TfrmMain.DCWaveformWaveformData(Sender: TObject;
  Data: PVisualBuffer; MinY, MaxY, NumSamples, Channels: Integer);
var
  i : integer;
begin
  BackBuffer.Canvas.Pen.Color := clBlack;
  BackBuffer.Canvas.Brush.Color := clBlack;
  BackBuffer.Canvas.Rectangle(BackBuffer.Canvas.ClipRect);

  BackBuffer.Canvas.Pen.Color := clLime;
  BackBuffer.Canvas.Brush.Color := clLime;

  for i := 0 to pbVisual.Width -1 do
  begin
    if i = 0 then BackBuffer.Canvas.MoveTo(i,Data[0,i])
             else BackBuffer.Canvas.LineTo(i,Data[0,i]);
  end;

  pbVisual.Canvas.Draw(0,0,BackBuffer);
end;

procedure TfrmMain.DCSpectrumSpectrumData(Sender: TObject;
  Data: PVisualBuffer; MinY, MaxY, NumSamples, Channels: Integer);
var
  i : integer;
begin
  BackBuffer.Canvas.Pen.Color := clBlack;
  BackBuffer.Canvas.Brush.Color := clBlack;
  BackBuffer.Canvas.Rectangle(BackBuffer.Canvas.ClipRect);

  BackBuffer.Canvas.Pen.Color := clLime;
  BackBuffer.Canvas.Brush.Color := clLime;

  for i := 0 to pbVisual.Width -1 do
  begin
    dec(Peaks[i],4);
    if Data[0,i] > Peaks[i] then Peaks[i] := Data[0,i];
    BackBuffer.Canvas.Pen.Color := clLime + Peaks[i] * 5;
    BackBuffer.Canvas.MoveTo(i,BackBuffer.Height - Peaks[i]);
    BackBuffer.Canvas.LineTo(i,BackBuffer.Height);
  end;

  pbVisual.Canvas.Draw(0,0,BackBuffer);
end;

procedure TfrmMain.pbVisualPaint(Sender: TObject);
begin
  pbVisual.Canvas.Draw(0,0,BackBuffer);
end;

procedure TfrmMain.pbVisualClick(Sender: TObject);
begin
  case VisualType of
    vtNone: None1Click(Spectrum1);
    vtSpectrum: None1Click(Waveform1);
    vtWaveform: None1Click(None1);
  end;
end;

procedure TfrmMain.Echo1Click(Sender: TObject);
begin
  frmDMOEcho.ShowModal;
end;

procedure TfrmMain.Flanger1Click(Sender: TObject);
begin
  frmDMOFlanger.ShowModal;
end;

procedure TfrmMain.Gargle1Click(Sender: TObject);
begin
  frmDMOGargle.ShowModal;
end;

procedure TfrmMain.I3DL2Reverb1Click(Sender: TObject);
begin
  frmDMOI3DL2Reverb.ShowModal;
end;

procedure TfrmMain.ParamEQ1Click(Sender: TObject);
begin
  frmDMOParamEQ.ShowModal;
end;

procedure TfrmMain.WavesReverb1Click(Sender: TObject);
begin
  frmDMOWavesReverb.ShowModal;
end;

procedure TfrmMain.Bandpass1Click(Sender: TObject);
begin
  frmDSPBandPass.ShowModal;
end;

procedure TfrmMain.ChannelOrder1Click(Sender: TObject);
begin
  frmDSPChannelOrder.ShowModal;
end;

procedure TfrmMain.Compressor2Click(Sender: TObject);
begin
  frmDSPCompressor.ShowModal;
end;

procedure TfrmMain.DownMixMono1Click(Sender: TObject);
begin
  frmDSPDownMix.ShowModal;
end;

procedure TfrmMain.DynamicAmplify1Click(Sender: TObject);
begin
  frmDSPDynamicAmplify.ShowModal;
end;

procedure TfrmMain.EchoDelay1Click(Sender: TObject);
begin
  frmDSPEchoDelay.ShowModal;
end;

procedure TfrmMain.Equalizer1Click(Sender: TObject);
begin
  frmDSPEqualizer.ShowModal;
end;

procedure TfrmMain.Flanger2Click(Sender: TObject);
begin
  frmDSPFlanger.ShowModal;
end;

procedure TfrmMain.Highpass1Click(Sender: TObject);
begin
  frmDSPHighpass.ShowModal;
end;

procedure TfrmMain.Lowpass1Click(Sender: TObject);
begin
  frmDSPLowpass.ShowModal;
end;

procedure TfrmMain.Notch1Click(Sender: TObject);
begin
  frmDSPNotch.ShowModal;
end;

procedure TfrmMain.PhaseInvert1Click(Sender: TObject);
begin
  frmDSPPhaseInvert.ShowModal;
end;

procedure TfrmMain.Phaser1Click(Sender: TObject);
begin
  frmDSPPhaser.ShowModal;
end;

procedure TfrmMain.PitchScale1Click(Sender: TObject);
begin
  frmDSPPitchScale.ShowModal;
end;

procedure TfrmMain.PitchShift1Click(Sender: TObject);
begin
  frmDSPPitchShift.ShowModal;
end;

procedure TfrmMain.Sound3D1Click(Sender: TObject);
begin
  frmDSPSound3D.ShowModal;
end;

procedure TfrmMain.empo1Click(Sender: TObject);
begin
  frmDSPTempo.ShowModal;
end;

procedure TfrmMain.rueBass1Click(Sender: TObject);
begin
  frmDSPTrueBass.ShowModal;
end;

procedure TfrmMain.VisualPlugins1Click(Sender: TObject);
begin
  frmWA2VisualPlugins.ShowModal;
end;

procedure TfrmMain.DSPPlugins1Click(Sender: TObject);
begin
  frmWA2DSPPlugins.ShowModal;
end;

procedure TfrmMain.tbPanChange(Sender: TObject);
begin
  FilterGraph.Balance := tbPan.Position;
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

procedure TfrmMain.FormShow(Sender: TObject);
begin
  DCWADSPWrapper.Plugins.OwnerWindow := frmWA2DSPPlugins.Handle;
  DCWAVisualWrapper.Plugins.OwnerWindow := frmWA2VisualPlugins.Handle;
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

procedure TfrmMain.DSPackDCDSPFilterPCMData(Sender: TObject;
  Buffer: Pointer; Size: Integer; out NewSize: Integer; Stream: PDSStream);
begin
  // This is the main Callback for the DiretShow Filters Transform
  // function. Newsize will be the New Buffersize.
  NewSize := Size;

  // Process the internal Filters first
  DCAmplify.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCBandpass.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCChannelOrder.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCCompressor.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDownMix.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCEqualizer.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCDynamicAmplify.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCEchoDelay.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCFlanger.Process(Buffer,NewSize,Stream.Bits,Stream.Float);
  DCHighpass.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCLowpass.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCNotch.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCParametricEQ.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCPhaseInvert.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCPhaser.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCPitchScale.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  NewSize := DCPitchShift.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCSound3D.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  NewSize := DCTempo.Process(Buffer,NewSize,Stream.Bits,Stream.Channels,Stream.Float);
  DCTrebleEnhancer.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCTrueBass.Process(Buffer,NewSize,Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);

  // now Process the DMO Filters
  DCDMOChorus.Process(Buffer,NewSize);
  DCDMOCompressor.Process(Buffer,NewSize);
  DCDMODistortion.Process(Buffer,NewSize);
  DCDMOEcho.Process(Buffer,NewSize);
  DCDMOFlanger.Process(Buffer,NewSize);
  DCDMOGargle.Process(Buffer,NewSize);
  DCDMOI3DL2Reverb.Process(Buffer,NewSize);
  DCDMOParamEQ.Process(Buffer,NewSize);
  DCDMOWavesReverb.Process(Buffer,NewSize);

  // Process Winamp2 DSP Plugin
  NewSize := DCWADSPWrapper.Plugins.Process(Buffer,NewSize);
end;

procedure TfrmMain.DSPackDCDSPFilterVisualData(Sender: TObject;
  Buffer: Pointer; Size: Integer; Stream: PDSStream);
begin
  case VisualType of
    vtSpectrum: DCSpectrum.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
    vtWaveform: DCWaveform.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
  end;
  DCWAVisualWrapper.Plugins.Process(Buffer,Size);
end;

procedure TfrmMain.DSPackDCDSPFilterMediaTypeChanged(Sender: TObject;
  Stream: PDSStream);
begin
  // Callback : called when a Mediatype has changed
  DCEchoDelay.Init(Stream.Frequency,Stream.Bits,Stream.Channels,True);
  DCFlanger.Init(Stream.Channels,Stream.Frequency,0.1);
  DCTempo.Init(Stream.Frequency,Stream.Bits,Stream.Channels);
  DCTrebleEnhancer.Init(Stream.Frequency);
  DCTrueBass.Init(Stream.Frequency);

  DCDMOChorus.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOCompressor.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMODistortion.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOEcho.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOFlanger.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOGargle.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOI3DL2Reverb.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOParamEQ.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);
  DCDMOWavesReverb.Init(Stream.Frequency,Stream.Bits,Stream.Channels,Stream.Float);

  DCWADSPWrapper.Plugins.Init(Stream);
  DCWAVisualWrapper.Plugins.Init(Stream);

  // Flush the Filters to prevent distortion
  DSPackDCDSPFilterFlush(Self);

  log.Lines.Add(inttostr(log.Lines.Count+1) + ') DCDSPFilter - MediaType Changed');
end;

procedure TfrmMain.rebleEnhancer1Click(Sender: TObject);
begin
  frmDSPTrebleEnhancer.ShowModal;
end;

procedure TfrmMain.ParametricEQ1Click(Sender: TObject);
begin
  frmDSPParametricEQ.ShowModal;
end;

end.
