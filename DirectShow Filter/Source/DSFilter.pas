
    (*********************************************************************
     *  DSFilter.pas                                                     *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 24-07-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)

unit DSFilter;

{$I Compiler.inc}

interface

uses
  DirectShow9, BaseClass, ActiveX, Windows, MMSystem,
{$IFDEF WITH_PROPERTYPAGES} PropAbout, PropOptions, PropWinamp, PropWinampVis, PropDSP, {$ENDIF}
{$IFDEF WITH_WINAMP} waDSPThreadedWrapper, WinampVisWrapper, ThdTimer, waConst, Forms, {$ENDIF}
{$IFDEF WITH_CPU_METER} CPUMeter, {$ENDIF}
{$IFDEF WITH_REGISTRY} Registry, {$ENDIF}
{$IFDEF WITH_BITRATE_CONVERSION} dspBitrateConverter, {$ENDIF}
{$IFDEF WITH_INTERNAL_DSP} DynamicFilterList, {$ENDIF}
{$IFDEF WITH_WINAMP} Utils, {$ENDIF}
{$IFDEF DEBUG} {$IFNDEF WITH_WINAMP} Utils, {$ENDIF} {$ENDIF}
  DSUtil, SysUtils, Classes, DCDSPTypes,
{$IFDEF WITH_TRAYICON} TrayIcon, Messages, {$ENDIF}
  dspConst, dmoConst, MatroskaSplitter, Variants;

type
  TDCDSPFilter = class;

  TDCDSPFilterInputPin = class(TBCTransformInputPin)
  public
    fOwner: TDCDSPFilter;
    fIndex: integer;
    FLock: TBCCritSec;
  public
    Blocked: Boolean;
    constructor Create(ObjectName: string; TransformFilter: TDCDSPFilter;
      out hr: HRESULT; Name: WideString; Index: integer; aBlocked: Boolean; Lock: TBCCritSec);
    function Receive(pSample: IMediaSample): HRESULT; override;
    function CompleteConnect(ReceivePin: IPin): HRESULT; override;
    function QueryPinInfo(out pInfo: TPinInfo): HRESULT; override;
  end;

  TDCDSPFilterOutputPin = class(TBCTransformOutputPin)
  public
    fOwner: TDCDSPFilter;
  public
    constructor Create(ObjectName: string; TransformFilter: TDCDSPFilter;
      out hr: HRESULT; Name: WideString);
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult; override;
    function CheckMediaType(mtOut: PAMMediaType): HRESULT; override;
    function SetMediaType(pmt: PAMMediaType): HRESULT; override;
    function Notify(Sendr: IBaseFilter; q: TQuality): HRESULT; override;
    function GetAllocator: IMemAllocator;
  end;

  TDCDSPFilter = class(TBCTransformFilter, IDCDSPFilterInterface,
                       {$IFDEF WITH_PROPERTYPAGES} ISpecifyPropertyPages, {$ENDIF}
                       IPersist, IMediaFilter, IDCDSPFilterVisualInterface, IAMStreamSelect)
  public
    FSwitchLock: TBCCritSec;
    FEnableRunningObjectTable: Boolean;
    FRunningObjectTable: integer;
    fActivePin: TDCDSPFilterInputPin;
    fInitialized: Boolean;
{$IFDEF WITH_PROPERTYPAGES}
    fEnablePropertyPage: Boolean;
{$ENDIF}
{$IFDEF WITH_BITRATE_CONVERSION}
    fBRConv: TDCBitrateConverter;
    fEnableBRConv: Boolean;
    fBRBeforeDSP: Boolean;
{$ENDIF}
{$IFDEF WITH_TRAYICON}
    fBalloonHint: Boolean;
    fTrayIcon: TTrayReg;
{$ENDIF}
    fEOS : Boolean;
    fEOSDelay : Int64;
    fEnableLimitInstance : Boolean;
    fStreamList : TList;
    fPreferedStream : integer;
    fCurrentMediaType : TAMMediaType;
    fNeedMediatypeChange : Boolean;
    fForceMediatypeChange : Boolean;
    fPinList : TList;
    fEnableStreamSwitching : Boolean;
    fEnableStreamSwitchingInterface : Boolean;
    fEnableForceDisconnect: Boolean;
    fEnableForceStopFilter: Boolean;
    FEnableVisualBuffering: Boolean;
    FVisualBufferEnabled: Boolean;
{$IFDEF WITH_WINAMP}
    fWinampVisAutoStart : Boolean;
    fDSPPlugin : TDCWADSPThreadedPluginItem;
    fCurrentVisualPlugin : String;
    fCurrentVisualPluginIndex : integer;
    fLoadedPlugin : String;
    fLoadedPluginIndex : integer;
    fVisTimer : TThreadedTimer;
    fFirstRun : Boolean;
{$ENDIF}
{$IFDEF WITH_CPU_METER}
    fCPUMeter : TCPUMeter;
{$ENDIF}
{$IFDEF WITH_INTERNAL_DSP}
    fFilters : TDCFilterList;
    fPresets : TStringList;
{$ENDIF}
    fEnableDelay : Boolean;
    fDelay : integer;
    fLastType : TDSStream;
    fLastMediaType : TDSStream;
    fThisInstance : integer;
    fFlushing : Boolean;
    fPlaying : Boolean;
    fLocked : Boolean;
    fCanRollOver : Boolean;
    fVisBufferSwap : integer;
    fVisBufferRollOver : integer;
    fVisBufferRollOverPos : integer;
    fStream : TDSStream;
    fRealStream : TDSStream;
    fMediaLocked : Boolean;
    fVISafterDSP : Boolean;
    fPCMDataBeforeMainDSP : Boolean;
    fIsDVD : Boolean;
    fCallback : IDCDSPFilterPCMCallBack;
    fVisBuffer : PChar;
    fVisBufferCount : integer;
    fVisBufferDelay,
    fVisBufferStartTime : TReferenceTime;
    fVisBufferMaxBytes : integer;
    fVisBufferWrap : integer;
    fVisBufferDivider : Double;
    fVisModBytes : integer;
    fFirstVisRun : Boolean;
{$IFDEF WITH_REGISTRY}
    fDisableSaving : Boolean;
    procedure LoadDefaults;
{$ENDIF}
{$IFDEF WITH_WINAMP}
    procedure OnWinampVisTimer(Sender : TObject);
    procedure KillWinampDSPWindows;
{$ENDIF}
    function ApplyVisual(Buffer : Pointer; Length : integer) : HRESULT; stdcall;
    function AllocateVisBuffer : HRESULT; stdcall;
    function GetBufferAtTimeStamp(Time : REFERENCE_TIME; out Bytes : integer) : PChar; stdcall;
    function CheckDVD : Boolean;
{$IFDEF WITH_REGISTRY}
{$IFDEF WITH_INTERNAL_DSP}
    procedure UpdatePresetsList;
{$ENDIF}
{$ENDIF}
{$IFDEF WITH_BITRATE_CONVERSION}
    procedure ChangeMediaType(var pmt: PamMediaType);
{$ENDIF}
    function GetConvertedStream: TDSStream;
    procedure ParseLanguageOggSplitter(Filter: IBaseFilter);
    procedure ParseLanguageITrackInfo(TrackInfo: ITrackInfo);
    function ParseLanguageMorganMSSFile(aName: WideString): Boolean;
    function ParseLanguageMorganMediaPropBag(Media: IPersistMediaPropertyBag): Boolean;
    function ParseLanguageGabestAVIPropBag(Media: IPropertyBag): Boolean;
    function ParseLanguageAVIStreamHeader(Filter: IBaseFilter): Boolean;
    function StartStreaming: HRESULT; override;
    function CheckDownStream: Boolean;
  public
    function GetActivePin: TDCDSPFilterInputPin;
    procedure ClearLanguageList;
    procedure ClearPinList;
    function GetConnectedInputs: integer;
    function GetPinByIndex(index: integer): TDCDSPFilterInputPin;
    function GetLangByIndex(index: integer): PLangItem;
    // --- IBaseFilter
    function InitializeOutputSample(Sample: IMediaSample; out OutSample: IMediaSample): HRESULT; override;
    function Receive(Sample: IMediaSample): HRESULT; override;

{$IFDEF WITH_TRAYICON}
    function JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT; override;
{$ENDIF}
    function CheckConnect(dir: TPinDirection; Pin: IPin) : HRESULT; override;
    function CompleteConnect(direction: TPinDirection; ReceivePin: IPin): HRESULT; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    constructor Create(ObjName: string; unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;
    function Transform(msIn, msOut: IMediaSample): HRESULT; override;
    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT; override;
    function CheckTransform(mtIn, mtOut: PAMMediaType): HRESULT; override;
    function BeginFlush: HRESULT; override;
    function EndFlush: HRESULT; override;
    function DecideBufferSize(Alloc: IMemAllocator; propInputRequest: PALLOCATORPROPERTIES): HRESULT; override;
    function Stop: HRESULT; override;
    function Pause: HRESULT; override;
    function Run(tStart: TReferenceTime): HRESULT; override;
    function NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult; override;  stdcall;
    function GetPin(n: integer): TBCBasePin; override;
    function GetPinCount: Integer; override;
    function EndOfStream: HRESULT; override;
    function Copy(Source, Dest: IMediaSample): HRESULT;

    // ---- IAMStreamSelect
    function Count(out pcStreams: DWORD): HResult; stdcall;
    function Info(lIndex: Longint; out ppmt: PAMMediaType; out pdwFlags: DWORD; out plcid: LCID; out pdwGroup: DWORD; out ppszName: PWCHAR; out ppObject: IUnknown; out ppUnk : IUnknown): HResult; stdcall;
    function Enable(lIndex: Longint; dwFlags: DWORD): HResult; stdcall;

{$IFDEF WITH_PROPERTYPAGES}
    // --- ISpecifyPropertyPages
    function GetPages(out pages: TCAGUID): HResult; stdcall;
{$ENDIF}

    // --- IDCDSPFilterVisualInterface
    function get_VISafterDSP(out AfterDSP : BOOL): HRESULT; stdcall;
    function set_VISafterDSP(AfterDSP : BOOL): HRESULT; stdcall;
    function get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream) : HRESULT; stdcall;

    // --- IDCDSPFilterInterface
    // Callbacks for PCM Data
    function set_CallBackPCM(Callback : IDCDSPFilterPCMCallBack): HRESULT; stdcall;
    function set_PCMDataBeforeMainDSP(Before : BOOL): HRESULT; stdcall;
    // Winamp 2.x DSP Plugins
    function set_DSPPlugin(WindowHandle : hWnd; Path : PChar): HRESULT; stdcall;
    function get_DSPPlugin(out Path : PChar): HRESULT; stdcall;
    function get_DSPCount(out Count : integer): HRESULT; stdcall;
    function get_DSPDescription(out Description : PChar): HRESULT; stdcall;
    function get_DSPSubDescription(index : integer; out Description : PChar): HRESULT; stdcall;
    function set_DSPSubPlugin(index : integer): HRESULT; stdcall;
    function get_DSPSubPlugin(out index : integer): HRESULT; stdcall;
    function set_ShowConfig: HRESULT; stdcall;
    function set_UnloadDSPPlugin: HRESULT; stdcall;
    function get_EnableDSPPlug(out Enable : BOOL): HRESULT; stdcall;
    function set_EnableDSPPlug(Enable : BOOL): HRESULT; stdcall;
    function set_PluginOwnerWindow(Window : hWnd): HRESULT; stdcall;
    // Winamp Vis Plugins
    function get_WinampVisInterval(out Interval : integer) : HRESULT; stdcall;
    function set_WinampVisInterval(Interval : integer) : HRESULT; stdcall;
    function get_WinampVisPlugin(out Plugin : PChar; out Index : integer) : HRESULT; stdcall;
    function set_WinampVisPlugin(Plugin : PChar; Index : integer) : HRESULT; stdcall;
    function get_WinampVisAutostart(out Autostart : BOOL): HRESULT; stdcall;
    function set_WinampVisAutostart(Autostart : BOOL): HRESULT; stdcall;
    function set_StopWinampVisPlugin : HRESULT; stdcall;
    // Delay functions for delaying the Audio Stream through Timestamps
    function get_EnableDelay(out Enabled : BOOL): HRESULT; stdcall;
    function set_EnableDelay(Enabled : BOOL): HRESULT; stdcall;
    function get_Delay(out Delay : integer): HRESULT; stdcall;
    function set_Delay(Delay : integer): HRESULT; stdcall;
    // Misc Settings
    function get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
    function set_DisableSaving(Disable : BOOL) : HRESULT; stdcall;
    function get_FilterVersion(out Version : integer) : HRESULT; stdcall;
    function get_Instance(out Instance : integer) : HRESULT; stdcall;
    // Bitrate Conversion
    function set_EnableBitrateConversion(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableBitrateConversion(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableBitrateConversionBeforeDSP(Before : BOOL) : HRESULT; stdcall;
    function get_EnableBitrateConversionBeforeDSP(out Before : BOOL) : HRESULT; stdcall;
    function set_BitrateConversionBits(Bits : TDCBitRate) : HRESULT; stdcall;
    function get_BitrateConversionBits(out Bits : TDCBitRate) : HRESULT; stdcall;
    // functions to work with DSP Filters
    function get_FilterCount(out Count : integer): HRESULT; stdcall;
    function get_FilterType(Index : integer; out FilterType : TDCFilterType): HRESULT; stdcall;
    function set_AddFilter(Index : integer; FilterType : TDCFilterType): HRESULT; stdcall;
    function get_FilterName(Index : integer; out Name : PChar): HRESULT; stdcall;
    function get_WindowShown(Index : integer; out Shown : BOOL): HRESULT; stdcall;
    function set_WindowShown(Index : integer; Shown : BOOL): HRESULT; stdcall;
    function set_DeleteFilter(Index : integer): HRESULT; stdcall;
    function get_EnableFilter(Index : integer; out Enabled : BOOL): HRESULT; stdcall;
    function set_EnableFilter(Index : integer; Enabled : BOOL): HRESULT; stdcall;
    function set_RemoveAllFilters : HRESULT; stdcall;
    function set_MoveFilter(FromIndex : integer; ToIndex : integer): HRESULT; stdcall;
    function set_ResetShownWindows : HRESULT; stdcall;
    function get_FilterClass(Index : integer; out Filter : TComponent): HRESULT; stdcall;
    function get_FilterInterface(Index : integer; out Intf): HRESULT; stdcall;
    function get_FilterItem(Index : integer; out Item : TDCFilterItem): HRESULT; stdcall;
    function get_PresetCount(out Count : integer) : HRESULT; stdcall;
    function get_PresetExist(Name : PChar; out Exist : BOOL) : HRESULT; stdcall;
    function get_PresetName(Index : integer; out Name : PChar) : HRESULT; stdcall;
    function set_LoadPreset(Name : PChar) : HRESULT; stdcall;
    function set_SavePreset(Name : PChar) : HRESULT; stdcall;
    function set_DeletePreset(Name : PChar) : HRESULT; stdcall;
    function get_CPUUsage(out Usage : Double) : HRESULT; stdcall;
    function get_EnableStreamSwitching(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamSwitching(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableStreamSwitchingInterface(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamSwitchingInterface(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableStreamLimitInstance(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamLimitInstance(Enable : BOOL) : HRESULT; stdcall;
    function get_ForceStreamSwitchingDisconnect(out Enable : BOOL) : HRESULT; stdcall;
    function set_ForceStreamSwitchingDisconnect(Enable : BOOL) : HRESULT; stdcall;
    function get_ForceStreamSwitchingStopFilter(out Enable : BOOL) : HRESULT; stdcall;
    function set_ForceStreamSwitchingStopFilter(Enable : BOOL) : HRESULT; stdcall;
    function get_EnablePropertyPage(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnablePropertyPage(Enable: BOOL) : HRESULT; stdcall;
    function get_TrayiconVisible(out Visible: BOOL) : HRESULT; stdcall;
    function set_TrayiconVisible(Visible: BOOL) : HRESULT; stdcall;
    function get_EnableBalloonHint(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableBalloonHint(Enable: BOOL) : HRESULT; stdcall;
    function get_EnableROT(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableROT(Enable: BOOL) : HRESULT; stdcall;
    function get_EnableVisualBuffering(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableVisualBuffering(Enable: BOOL) : HRESULT; stdcall;
  end;

var
  InstanceCount : integer = 0;

implementation

(*** TDCDSPFilterInputPin *****************************************************)
{$IFDEF EXCEPT_DEBUG}
var
  DebFile: HFILE;

procedure erf(msg: String);
var
  pmsg: PChar;
  written: DWORD;
begin
  if DebFile = 0 then Exit;
  msg := DateToStr(Now) + ' - ' + TimeToStr(Now) + '|  ' + msg + #13#10;
  pmsg := pchar(msg);
  WriteFile(DebFile, pmsg^, Length(msg) * SizeOf(Char), written, nil);
end;

procedure er(msg: String);
begin
  erf(msg);
  OutputDebugString(PChar('DCDSPFILTER ERROR: ' + msg));
  MessageBox(0, pchar(msg), 'DCDSPFILTER ERROR', MB_OK);
end;
{$ENDIF}

constructor TDCDSPFilterInputPin.Create(ObjectName: string; TransformFilter: TDCDSPFilter;
      out hr: HRESULT; Name: WideString; Index: integer; aBlocked: Boolean; Lock: TBCCritSec);
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  fOwner := TransformFilter;
  fIndex := Index;
  Blocked := aBlocked;
  FLock := Lock;
  inherited Create(ObjectName,TransformFilter,hr,Name);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterInputPin.Create'); end; {$ENDIF}
end;

function TDCDSPFilterInputPin.Receive(pSample: IMediaSample): HRESULT;
var
  Pin: IPin;
  PinInfo: TPinInfo;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FLock.Lock;
  try
    if Blocked then
    begin
      if fOwner.fEnableForceStopFilter then
      begin
        ConnectedTo(Pin);
        if Assigned(Pin) then
        begin
          Pin.QueryPinInfo(PinInfo);
          if Assigned(PinInfo.pFilter) then
          begin
            PinInfo.pFilter.Stop;
            PinInfo.pFilter := nil;
          end;
        end;
      end;
      Result := E_FAIL;
    end else
      Result := inherited Receive(pSample);
  finally
    FLock.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterInputPin.Receive'); end; {$ENDIF}
end;

function TDCDSPFilterInputPin.CompleteConnect(ReceivePin: IPin): HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := inherited CompleteConnect(ReceivePin);
  if (Failed(Result))
    then Exit;
  if Blocked or not fOwner.fEnableForceDisconnect then Exit;
  if fOwner.FGraph <> nil then
    fOwner.FGraph.Reconnect(fOwner.Output);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterInputPin.CompleteConnect'); end; {$ENDIF}
end;

function TDCDSPFilterInputPin.QueryPinInfo(out pInfo: TPinInfo): HRESULT; stdcall;
var
  li: PLangItem;
  tmp: WideString;
  z: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := inherited QueryPinInfo(pInfo);
  li := fOwner.GetLangByIndex(fIndex);
  if IsConnected then
  begin
    if (Assigned(li))
      then tmp := li.aName;
    if Assigned(li) and (Length(tmp) > 0) then
    begin
      z := Length(tmp);
      if (z > 127)
        then z := 127;
      CopyMemory(@pInfo.achName, PWideChar(tmp), z*2);
      pInfo.achName[z] := #0;
    end else
    begin
      tmp := DefaultAudioTrack + inttostr(fIndex + 1);
      CopyMemory(@pInfo.achName, PWideChar(tmp), Length(tmp)*2+2);
    end;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterInputPin.QueryPinInfo'); end; {$ENDIF}
end;

(*** TDCDSPFilterOutputPin ****************************************************)

constructor TDCDSPFilterOutputPin.Create(ObjectName: string; TransformFilter: TDCDSPFilter;
      out hr: HRESULT; Name: WideString);
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  fOwner := TransformFilter;
  inherited Create(ObjectName,TransformFilter,hr,Name);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.Create'); end; {$ENDIF}
end;

function TDCDSPFilterOutputPin.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult;
var
  pin: TDCDSPFilterInputPin;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if IsEqualGUID(iid, IID_IMediaPosition) or IsEqualGUID(iid, IID_IMediaSeeking) then
  begin
    if (FPosition = nil) then
    begin
      pin := fOwner.GetActivePin;
      Assert(pin <> nil);
      Result := CreatePosPassThru(GetOwner, FALSE, pin, FPosition);
      if FAILED(Result) then exit;
    end;
    Result := FPosition.QueryInterface(iid, obj);
  end else
    Result := inherited NonDelegatingQueryInterface(iid, obj);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.NonDelegatingQueryInterface'); end; {$ENDIF}
end;

function TDCDSPFilterOutputPin.CheckMediaType(mtOut: PAMMediaType): HRESULT;
var
  pin: TDCDSPFilterInputPin;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  pin := fOwner.GetActivePin;
  Assert(pin <> nil);

  if not pin.IsConnected then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;
  Result := fOwner.CheckTransform(pin.AMMediaType, mtOut);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.CheckMediaType'); end; {$ENDIF}
end;

function TDCDSPFilterOutputPin.SetMediaType(pmt: PAMMediaType): HRESULT;
var
  pin: TDCDSPFilterInputPin;
  mt: TAMMediaType;
  apmt: PAMMediaType;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  pin := fOwner.GetActivePin;

  if not pin.IsConnected then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  Assert(pin <> nil);
  Assert(not IsEqualGUID(pin.AMMediaType.majortype,GUID_NULL));

  apmt := @mt;

  Result := FOwner.GetMediaType(0, apmt);
  if (Result <> S_OK) then Exit;

  if TBCMediaType(pmt).Equal(apmt) then
  begin
    Result := fOwner.CheckInputType(pmt);
    FreeMediaType(apmt);
    if Result <> S_OK then Exit;
  end else
  begin
    FreeMediaType(apmt);
    Result := VFW_E_TYPE_NOT_ACCEPTED;
    Exit;
  end;

  FreeMediaType(@Fmt);
  CopyMediaType(@Fmt, pmt);
  Result := fOwner.SetMediaType(PINDIR_OUTPUT,pmt);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.SetMediaType'); end; {$ENDIF}
end;

function TDCDSPFilterOutputPin.Notify(Sendr: IBaseFilter; q: TQuality): HRESULT;
var
  pin: TDCDSPFilterInputPin;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  pin := fOwner.GetActivePin;
  Result := fOwner.AlterQuality(q);
  if (Result <> S_FALSE) then Exit;
  ASSERT(pin <> nil);
  Result := pin.PassNotify(q);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.Notify'); end; {$ENDIF}
end;

function TDCDSPFilterOutputPin.GetAllocator: IMemAllocator;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := FAllocator;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilterOutputPin.GetAllocator'); end; {$ENDIF}
end;

(*** TDCDSPFilter *************************************************************)

constructor TDCDSPFilter.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var
  hr: HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Create(Factory.Name, Controller, hr);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CreateFromFactory'); end; {$ENDIF}
end;

constructor TDCDSPFilter.Create(ObjName: string; unk: IUnknown; out hr: HRESULT);
{$IFDEF WITH_REGISTRY}
  function GetBoolValue(Reg: TRegistry; Value: String; Default: Boolean): Boolean;
  begin
    if Reg.ValueExists(Value) then Result := Reg.ReadBool(Value)
                              else Result := Default;
  end;

  function GetStrValue(Reg: TRegistry; Value: String; Default: String): String;
  begin
    if Reg.ValueExists(Value) then Result := Reg.ReadString(Value)
                              else Result := Default;
  end;

  function GetIntValue(Reg: TRegistry; Value: String; Default: Integer): Integer;
  begin
    if Reg.ValueExists(Value) then Result := Reg.ReadInteger(Value)
                              else Result := Default;
  end;
var
  Reg : TRegistry;
  i  :integer;
{$IFDEF WITH_INTERNAL_DSP}
  Buf : PChar;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  inherited Create(ObjName, unk, CLSID_DCDSPFilter);
  fThisInstance := InterlockedIncrement(InstanceCount);
  FSwitchLock := TBCCritSec.Create;
{$IFDEF WITH_PROPERTYPAGES}
  fEnablePropertyPage := True;
{$ENDIF}
  fInitialized := False;
  fPreferedStream := -1;
  FRunningObjectTable := 0;
  fActivePin := nil;

  {$IFDEF DEBUG} dbg(fThisInstance,'Create -> Filter created'); {$ENDIF}

{$IFDEF WITH_BITRATE_CONVERSION}
  fBRConv := TDCBitrateConverter.Create(nil);
  fBRConv.Enabled := True;
{$ENDIF}
  fForceMediatypeChange := False;

{$IFDEF WITH_CPU_METER}
  fCPUMeter := TCPUMeter.Create;
{$ENDIF}

{$IFDEF WITH_WINAMP}
  fVisTimer := TThreadedTimer.Create(nil);
  fVisTimer.Enabled := False;
  fDSPPlugin := TDCWADSPThreadedPluginItem.Create(nil);
{$ENDIF}

{$IFDEF WITH_INTERNAL_DSP}
  fFilters := TDCFilterList.Create;
  fPresets := TStringList.Create;
{$ENDIF}

  fStreamList := TList.Create;
  fPCMDataBeforeMainDSP := False;

{$IFDEF WITH_REGISTRY}
  Reg := TRegistry.Create;
  Reg.Rootkey := HKEY_CURRENT_USER;
  if Reg.OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter',False) then
  begin
    try
      i := Reg.ReadInteger('Version');
      if i >= 1000 then
      begin
      {$IFDEF WITH_WINAMP}
        fDSPPlugin.Enabled := GetBoolValue(Reg,'EnableDSPPlugin',False);
        fWinampVisAutoStart := GetBoolValue(Reg,'WinampVisAutostart',False);
        fLoadedPluginIndex := GetIntValue(Reg,'LoadedPluginIndex',0) -1;
        fVisTimer.Interval := GetIntValue(Reg,'VisInterval',40);
        fCurrentVisualPluginIndex := GetIntValue(Reg,'CurrentVisualPluginIndex',0) - 1;
        fCurrentVisualPlugin := GetStrValue(Reg,'CurrentVisualPlugin','');
        fLoadedPlugin := GetStrValue(Reg,'LoadedPlugin','');
      {$ENDIF}
        fVISafterDSP := GetBoolValue(Reg,'VISafterDSP',False);
        fEnableDelay := GetBoolValue(Reg,'EnableDelay',False);
        fEnableStreamSwitching := GetBoolValue(Reg,'EnableStreamSwitching',False);
        fEnableStreamSwitchingInterface := GetBoolValue(Reg,'EnableStreamSwitchingInterface',True);
        fEnableLimitInstance := GetBoolValue(Reg,'EnableLimitInstance',True);
      {$IFDEF WITH_BITRATE_CONVERSION}
        fEnableBRConv := GetBoolValue(Reg,'EnableBitrateConversion',False);
        fBRBeforeDSP := GetBoolValue(Reg,'BitrateConversionBeforeDSP',False);
        fBRConv.BitRate := TDCBitRate(GetIntValue(Reg,'BitrateConversionBits',1));
      {$ENDIF}
        fEnableForceStopFilter := GetBoolValue(Reg,'EnableForceStopFilter',True);
        fEnableForceDisconnect := GetBoolValue(Reg,'EnableForceDisconnect',True);

        FEnableRunningObjectTable := GetBoolValue(Reg,'EnableRunningObjectTable',False);
        FEnableVisualBuffering := GetBoolValue(Reg, 'EnableVisualBuffering', True);
        FVisualBufferEnabled := FEnableVisualBuffering;

        fDelay := GetIntValue(Reg,'Delay',0);
      {$IFDEF WITH_TRAYICON}
        fTrayIcon.Visible := GetBoolValue(Reg, 'ShowTrayIcon', False);
        fBalloonHint :=  GetBoolValue(Reg, 'ShowBalloonHint', False);;
      {$ENDIF}

      {$IFDEF WITH_INTERNAL_DSP}
        if Reg.ValueExists('CurrentDSP') then
        begin
          i := Reg.GetDataSize('CurrentDSP');
          if i > -1 then
          begin
            Buf := AllocMem(i);
            Reg.ReadBinaryData('CurrentDSP',Buf^,i);
            fFilters.ImportSettings(Buf);
            FreeMem(Buf);
          end;
        end;
      {$ENDIF}
        Reg.CloseKey;
      end else
      begin
        Reg.CloseKey;
        LoadDefaults;
      end;
    except
      LoadDefaults;
    end;
  end else LoadDefaults;
{$IFDEF WITH_INTERNAL_DSP}
  UpdatePresetsList;
{$ENDIF}

  Reg.Free;
{$ENDIF}

  fVisBufferStartTime := -1;
  fFirstVisRun := True;
  fVisBufferRollOver := 0;
{$IFDEF WITH_WINAMP}
  fFirstRun := True;
  fDSPPlugin.OwnerWindow := GetDesktopWindow;
  fVisTimer.OnTimer := OnWinampVisTimer;
{$ENDIF}
  fFlushing := False;
  fVisBufferCount := 0;
  fVisBufferStartTime := 0;
  fPinList := TList.Create;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Create'); end; {$ENDIF}
end;

destructor TDCDSPFilter.Destroy;
{$IFDEF WITH_REGISTRY}
var
  Reg : TRegistry;
{$IFDEF WITH_INTERNAL_DSP}
  Buf : PChar;
  Size : integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  {$IFDEF DEBUG} dbg(fThisInstance,'Destroy -> Filter destroyed'); {$ENDIF}

{$IFDEF WITH_TRAYICON}
  if Assigned(fTrayIcon.TrayIcon) then
  begin
    PostThreadMessage(fTrayIcon.ThreadID,WM_QUIT,0,0);
    if(WaitForSingleObject(fTrayIcon.Thread, 10000) <> WAIT_OBJECT_0) then
      TerminateThread(fTrayIcon.Thread, DWORD(-1));
  end;
{$ENDIF}

{$IFDEF WITH_WINAMP}
  fVisTimer.Enabled := False;
{$ENDIF}
{$IFDEF WITH_REGISTRY}
  if not fDisableSaving then
  begin
    Reg := TRegistry.Create;
    Reg.Rootkey := HKEY_CURRENT_USER;
    if Reg.OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter',True) then
    begin
      Reg.WriteBool('EnableRunningObjectTable', FEnableRunningObjectTable);

    {$IFDEF WITH_WINAMP}
      Reg.WriteBool('EnableDSPPlugin',fDSPPlugin.Enabled);
      Reg.WriteInteger('LoadedPluginIndex',fLoadedPluginIndex + 1);
      Reg.WriteString('LoadedPlugin',fLoadedPlugin);
      Reg.WriteInteger('VisInterval',fVisTimer.Interval);
      Reg.WriteBool('WinampVisAutostart',fWinampVisAutoStart);
      Reg.WriteInteger('CurrentVisualPluginIndex',fCurrentVisualPluginIndex + 1);
      Reg.WriteString('CurrentVisualPlugin',fCurrentVisualPlugin);
    {$ENDIF}
      Reg.WriteInteger('Version', FilterVersion);
      Reg.WriteBool('EnableDelay',fEnableDelay);
      Reg.WriteInteger('Delay',fDelay);
      Reg.WriteBool('VISafterDSP',fVISafterDSP);
      Reg.WriteBool('EnableStreamSwitching',fEnableStreamSwitching);
      Reg.WriteBool('EnableStreamSwitchingInterface',fEnableStreamSwitchingInterface);
      Reg.WriteBool('EnableLimitInstance',fEnableLimitInstance);
    {$IFDEF WITH_BITRATE_CONVERSION}
      Reg.WriteBool('BitrateConversionBeforeDSP',fBRBeforeDSP);
      Reg.WriteBool('EnableBitrateConversion',fEnableBRConv);
      Reg.Writeinteger('BitrateConversionBits',integer(fBRConv.BitRate));
    {$ENDIF}
      Reg.WriteBool('EnableForceStopFilter',fEnableForceStopFilter);
      Reg.WriteBool('EnableForceDisconnect',fEnableForceDisconnect);
    {$IFDEF WITH_TRAYICON}
      Reg.WriteBool('ShowTrayIcon', fTrayIcon.Visible);
      Reg.WriteBool('ShowBalloonHint', fBalloonHint);
    {$ENDIF}
      Reg.WriteBool('EnableVisualBuffering', FEnableVisualBuffering);
    {$IFDEF WITH_INTERNAL_DSP}
      if fFilters.ExportSettings(Buf,Size) then
      begin
        Reg.WriteBinaryData('CurrentDSP',Buf^,Size);
      end else
      begin
        if Reg.ValueExists('CurrentDSP') then Reg.DeleteValue('CurrentDSP');
      end;
    {$ENDIF}
      Reg.CloseKey;
    end;
    Reg.Free;
  end;
{$ENDIF}

{$IFDEF WITH_WINAMP}
  set_UnloadDSPPlugin;
  StopVisualPlugin(fThisInstance);
  fVisTimer.Free;
  fDSPPlugin.Free;
{$ENDIF}

  fThisInstance := InterlockedDecrement(InstanceCount);

{$IFDEF WITH_INTERNAL_DSP}
  fFilters.Free;
  fPresets.Free;
{$ENDIF}


{$IFDEF WITH_CPU_METER}
  fCPUMeter.Free;
{$ENDIF}

  if fVisBuffer <> nil then
  begin
    FreeMemory(fVisBuffer);
    fVisBuffer := nil;
  end;

  ClearPinList;
  fPinList.Free;

  FreeMediaType(@fCurrentMediaType);

  ClearLanguageList;
  fStreamList.Free;
  FSwitchLock.Free;
{$IFDEF WITH_BITRATE_CONVERSION}
  fBRConv.Free;
{$ENDIF}
  inherited Destroy;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Destroy'); end; {$ENDIF}
end;
{$IFDEF WITH_PROPERTYPAGES}
(*** Property Pages ************************************************************)
function TDCDSPFilter.GetPages(out pages: TCAGUID): HResult;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Pages.cElems := 5;
  Pages.pElems := CoTaskMemAlloc(sizeof(TGUID) * Pages.cElems);
  if (Pages.pElems = nil) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  Pages.pElems^[0] := CLSID_DCDSPFilterPropertyPageDSP;
  Pages.pElems^[1] := CLSID_DCDSPFilterPropertyPageWinampVis;
  Pages.pElems^[2] := CLSID_DCDSPFilterPropertyPageWinamp;
  Pages.pElems^[3] := CLSID_DCDSPFilterPropertyPageOptions;
  Pages.pElems^[4] := CLSID_DCDSPFilterPropertyPageAbout;
  Result := S_OK;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetPages'); end; {$ENDIF}
end;
{$ENDIF}
{*** Main Filter Functions *****************************************************}
procedure TDCDSPFilter.ClearLanguageList;
var
  i: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  for i := 0 to fStreamList.Count -1 do
    FreeMemory(fStreamList.Items[i]);
  fStreamList.Clear;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ClearLanguageList'); end; {$ENDIF}
end;

procedure TDCDSPFilter.ClearPinList;
var
  i: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  for i := 1 to fPinList.Count -1 do
    TDCDSPFilterInputPin(fPinList.Items[i]).Free;
  fPinList.Clear;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ClearPinList'); end; {$ENDIF}
end;

function TDCDSPFilter.GetActivePin: TDCDSPFilterInputPin;
var
  i: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if Assigned(fActivePin) then
    begin
      Result := fActivePin;
      Exit;
    end;

    for i := 0 to fPinList.Count -1 do
    begin
      if not TDCDSPFilterInputPin(fPinList.Items[i]).Blocked then
      begin
        Result := TDCDSPFilterInputPin(fPinList.Items[i]);
        Exit;
      end;
    end;

    Result := nil;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetActivePin'); end; {$ENDIF}
end;

function TDCDSPFilter.GetConnectedInputs: integer;
var
  i: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := 0;
    for i := 0 to fPinList.Count -1 do
    begin
      if TDCDSPFilterInputPin(fPinList.Items[i]).IsConnected then inc(Result);
    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetConnectedInputs'); end; {$ENDIF}
end;

function TDCDSPFilter.GetPinByIndex(index: integer): TDCDSPFilterInputPin;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if (Index < 0) or (index >= fPinList.Count) then
    begin
      Result := nil;
      Exit;
    end;

    Result := fPinList.Items[index];
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetPinByIndex'); end; {$ENDIF}
end;

function TDCDSPFilter.GetLangByIndex(index: integer): PLangItem;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if (Index < 0) or (index >= fStreamList.Count) then
    begin
      Result := nil;
      Exit;
    end;

    Result := fStreamList.Items[index];
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetLangByIndex'); end; {$ENDIF}
end;

function TDCDSPFilter.GetPinCount: Integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := fPinList.Count + 1;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetPinCount'); end; {$ENDIF}
end;

function TDCDSPFilter.GetPin(n: integer): TBCBasePin;
var
  hr : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  hr := S_OK;

  if(FInput = nil) then
  begin
    FInput := TDCDSPFilterInputPin.Create('Input Pin', Self, hr, 'Input', fPinList.Count, False, FcsReceive);
    if not fEnableStreamSwitching then fActivePin := TDCDSPFilterInputPin(FInput);
    fPinList.Add(FInput);
    ASSERT(SUCCEEDED(hr));
  end;

  if((FInput <> nil) and (FOutput = nil)) then
  begin
    FOutput := TDCDSPFilterOutputPin.Create('Output Pin', self, hr, 'Output');
    ASSERT(SUCCEEDED(hr));
    if(FOutput = nil) then
    begin
      FInput.Free;
      FInput := nil;
    end;
  end;

  case n of
    0: result := FInput;
    1: result := FOutput;
  else
    begin
      if (fPinList.Count = 1) or (n > fPinList.Count) then
        Result := nil
      else
        Result := fPinList.Items[n-1];
    end;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetPin'); end; {$ENDIF}
end;

function TDCDSPFilter.NonDelegatingQueryInterface(const IID: TGUID; out Obj): HResult;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  {$IFDEF DEBUG} dbg(fThisInstance, 'TDCDSPFilter.NonDelegatingQueryInterface'); {$ENDIF}
  if IsEqualGuid(iid, IID_DCDSPFilter) or IsEqualGuid(iid, IPersist) or IsEqualGuid(iid, IID_IMediaFilter) or
     {$IFDEF WITH_PROPERTYPAGES}  IsEqualGuid(iid, IID_ISpecifyPropertyPages) or {$ENDIF}
     IsEqualGuid(iid, IID_DCDSPFilterVisual) or IsEqualGuid(iid, IID_IAMStreamSelect) then
  begin
{$IFDEF WITH_PROPERTYPAGES}
    if IsEqualGuid(iid, IID_ISpecifyPropertyPages) and not fEnablePropertyPage then
    begin
      Result := E_NOINTERFACE;
      Exit;
    end;
{$ENDIF}
    if IsEqualGuid(iid, IID_IAMStreamSelect) and not fEnableStreamSwitching then
    begin
      Result := E_NOINTERFACE;
      Exit;
    end;
    if IsEqualGuid(iid, IID_IAMStreamSelect) and fEnableStreamSwitchingInterface and (GetConnectedInputs <= 1) then
    begin
      Result := E_NOINTERFACE;
      Exit;
    end;
    if GetInterface(IID, Obj) then Result := S_OK
                              else Result := E_NOINTERFACE;
  end else Result := inherited NonDelegatingQueryInterface(iid, Obj);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.NonDelegatingQueryInterface'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.CheckConnect(dir: TPinDirection; Pin: IPin): HRESULT;
const
  CLSID_MMSwitch : TGUID = '{D3CD7858-971A-4838-ACEC-40CA5D529DC8}';
var
  PinInfo : TPinInfo;
  idc : IDCDSPFilterInterface;
  inst : integer;
  ClassID : TGUID;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
//  if fEnableLimitInstance and (fThisInstance > 1) then
  if fEnableLimitInstance and CheckDownStream then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if dir = PINDIR_OUTPUT then
  begin
    if (Pin.QueryPinInfo(PinInfo) = S_OK) and Assigned(PinInfo.pFilter) then
    begin
      if PinInfo.pFilter.GetClassID(ClassID) = S_OK then
      begin
        // don´t connect to the inputpin of the MMSwitch
        if IsEqualGUID(ClassID,CLSID_MMSwitch) then
        begin
          Result := E_FAIL;
          PinInfo.pFilter := nil;
          Exit;
        end;
      end;

      if PinInfo.pFilter.QueryInterface(IID_DCDSPFilter,idc) = S_OK then
      begin
        // We cannot connect to the same Filter !
        idc.get_Instance(inst);
        if inst <> fThisInstance then
        begin
          idc.set_UnloadDSPPlugin;
          idc.set_StopWinampVisPlugin;
        end;
        idc := nil;
        Result := E_FAIL;
        PinInfo.pFilter := nil;
        Exit;
      end;
      PinInfo.pFilter := nil;
    end;

{$IFDEF WITH_TRAYICON}
    if not Assigned(fTrayIcon.TrayIcon) and (fTrayIcon.Thread = 0) then
    begin
      fTrayIcon.Owner := Pointer(Self);
      fTrayIcon.Thread := CreateThread(nil, 0, @TrayIconThreadProc, @fTrayIcon, 0, fTrayIcon.ThreadID);
    end;
{$ENDIF}
    if FEnableRunningObjectTable and (FRunningObjectTable = 0) then
    begin
      AddGraphToRot(Graph, FRunningObjectTable);
    end;
  end;

  Result := inherited CheckConnect(dir,Pin);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CheckConnect'); end; {$ENDIF}
end;

procedure TDCDSPFilter.ParseLanguageOggSplitter(Filter: IBaseFilter);
var
  iSS : IAMStreamSelect;
  cnt : Cardinal;
  pmt : PAMMediaType;
  Flags : Cardinal;
  LCID : Cardinal;
  Group : Cardinal;
  Obj : IUnknown;
  Unk : IUnknown;
  i : integer;
  Name : PWChar;
  lang: PLangItem;
  Buff : array[0..MAX_PATH-1] of Char;
  lName : WideString;
  c1: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if Filter.QueryInterface(IID_IAMStreamSelect,iSS) = S_OK then
  begin
    iSS.Count(cnt);
    for i := 0 to cnt -1 do
    begin
      iSS.Info(i,pmt,Flags,lcid,Group,Name,Obj,Unk);
      if Group = 1 then
      begin
        lang := AllocMem(SizeOf(TLangItem));
        lang.aLCID := lcid;
        if lcid > 0 then
        begin
          GetLocaleInfo(lcid,LOCALE_SLANGUAGE,Buff,MAX_PATH);
          lName := Buff;
          c1 := pos('(', lName);
          if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
        end else
        if Assigned(Name) then
        begin
          lName := Name;
          if Pos('Ton: ', lName) > 0 then delete(lName,1,5);
        end else
        begin
          lName := 'Unknown Language';
        end;
        CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);
        fStreamList.Add(lang);
      end;
      if Assigned(Name) then
      begin
        CoTaskMemFree(Name);
        Name := nil;
      end;
      Obj := nil;
      Unk := nil;
      if Assigned(pmt) then
      begin
        DeleteMediaType(pmt);
        pmt := nil;
      end;
    end;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageOggSplitter'); end; {$ENDIF}
end;

procedure TDCDSPFilter.ParseLanguageITrackInfo(TrackInfo: ITrackInfo);
var
  n: integer;
  i: integer;
  TrackElem : TTrackElement;
  lang: PLangItem;
  lComment : WideString;
  lName : WideString;
  Buff : array[0..MAX_PATH-1] of Char;
  c1: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  n := 0;
  for i := 0 to TrackInfo.GetTrackCount -1 do
  begin
    TrackInfo.GetTrackInfo(i,TrackElem);
    if TrackElem.TrackType = TypeAudio then
    begin
      lang := AllocMem(SizeOf(TLangItem));
      lComment := TrackInfo.GetTrackName(i);
      lang.aLCID := Cvt_ISO_639_2_To_LCID(TrackElem.Language);
      if lang.aLCID > 0 then
      begin
        GetLocaleInfo(lang.aLCID,LOCALE_SLANGUAGE,Buff,MAX_PATH);
        lName := Buff;
        c1 := pos('(', lName);
        if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
      end else lName := Cvt_ISO_639_2_To_Lang(TrackElem.Language);

      if Length(lComment) > 0 then lName := lName + ' [' + lComment + ']';
      CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);

      fStreamList.Add(lang);
      if TrackElem.FlagDefault then fPreferedStream := n;
      inc(n);
    end;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageITrackInfo'); end; {$ENDIF}
end;

function TDCDSPFilter.ParseLanguageMorganMediaPropBag(Media: IPersistMediaPropertyBag): Boolean;
var
  iMP: IMediaPropertyBag;
  vari: OleVariant;
  n : integer;
  c1 : integer;
  i : integer;
  lName : WideString;
  lang: PLangItem;
  Buff : array[0..MAX_PATH-1] of Char;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := False;
  CoCreateInstance(CLSID_MediaPropertyBag, nil, CLSCTX_INPROC, IID_IMediaPropertyBag, iMP);
  if Assigned(iMP) then
  begin
    if Media.Save(iMP,False,False) = S_OK then
    begin
      VariantInit(vari);
      n := -1;
      if iMP.Read('INFO/ICAS', vari,nil) = S_OK then
      begin;
        lName := FindVarData(vari)^.VOleStr;
        n := StrToInt(lName);
        if n > 0 then fPreferedStream := n-1;
      end;

      for i := 1 to 9 do
      begin
        if iMP.Read(PWChar(WideString('INFO/IAS' + inttostr(i))), vari,nil ) = S_OK then
        begin;
          Result := true;
          lName := FindVarData(vari)^.VOleStr;
          if (lName = '') then break;
          lang := AllocMem(SizeOf(TLangItem));
          if (Length(lName) > 2) and (lName[1] = '0') and (LowerCase(lName[2]) = 'x') then
          begin
            StringReplace(lName,'0x', '$', [rfIgnoreCase]);
            lang.aLCID := StrToInt(lName);
            GetLocaleInfo(lang.aLCID,LOCALE_SLANGUAGE,Buff,MAX_PATH);
            lName := Buff;
            if (n = 0) and (lang.aLCID = (GetThreadLocale and $FF)) then
               fPreferedStream := i-1;
            c1 := pos('(', lName);
            if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
          end;
          CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);
          fStreamList.Add(lang);
        end;
      end;
      VariantClear(vari);
    end;
    iMP := nil;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageMorganMediaPropBag'); end; {$ENDIF}
end;

function TDCDSPFilter.ParseLanguageGabestAVIPropBag(Media: IPropertyBag): Boolean;
var
  vari: OleVariant;
  n : integer;
  c1 : integer;
  i : integer;
  lName : WideString;
  lang: PLangItem;
  Buff : array[0..MAX_PATH-1] of Char;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := False;
  VariantInit(vari);
  n := -1;
  if Media.Read('INFO/ICAS', vari,nil) = S_OK then
  begin;
    lName := FindVarData(vari)^.VOleStr;
    n := StrToInt(lName);
    if n > 0 then fPreferedStream := n-1;
  end;

  for i := 1 to 9 do
  begin
    if Media.Read(PWChar(WideString('INFO/IAS' + inttostr(i))), vari,nil ) = S_OK then
    begin;
      Result := true;
      lName := FindVarData(vari)^.VOleStr;
      if (lName = '') then break;
      lang := AllocMem(SizeOf(TLangItem));
      if (Length(lName) > 2) and (lName[1] = '0') and (LowerCase(lName[2]) = 'x') then
      begin
        StringReplace(lName,'0x', '$', [rfIgnoreCase]);
        lang.aLCID := StrToInt(lName);
        GetLocaleInfo(lang.aLCID,LOCALE_SLANGUAGE,Buff,MAX_PATH);
        lName := Buff;
        if (n = 0) and (lang.aLCID = (GetThreadLocale and $FF)) then
           fPreferedStream := i-1;
        c1 := pos('(', lName);
        if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
      end;
      CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);
      fStreamList.Add(lang);
    end;
  end;

  VariantClear(vari);
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageGabestAVIPropBag'); end; {$ENDIF}
end;

function TDCDSPFilter.ParseLanguageMorganMSSFile(aName: WideString): Boolean;
function ReadString(Section, Name, Default, FileName: WideString): WideString;
var
  wBuf: array[0..MAX_PATH -1] of WideChar;
  aBuf: array[0..MAX_PATH -1] of Char;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    GetPrivateProfileStringW(PWChar(Section),PWChar(Name),PWChar(Default),wBuf,MAX_PATH,PWChar(FileName));
    Result := wBuf;
  end else
  begin
    GetPrivateProfileString(PChar(String(Section)),PChar(String(Name)),PChar(String(Default)),aBuf,MAX_PATH,PChar(String(FileName)));
    Result := aBuf;
  end;
end;

function ReadInteger(Section, Name: WideString; Default: integer; FileName: WideString): integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT
    then Result := GetPrivateProfileIntW(PWChar(Section),PWChar(Name),Default,PWChar(FileName))
    else Result := GetPrivateProfileInt(PChar(String(Section)),PChar(String(Name)),Default,PChar(String(FileName)));
end;

var
  lName : WideString;
  path: WideString;
  llName: WideString;
  n: integer;
  i: integer;
  c1: integer;
  lang: PLangItem;
  Buff : array[0..MAX_PATH-1] of Char;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := False;

  lName := aName;
  path := ExtractFileExt(aName);
  delete(lName,Length(lName) - Length(path) + 1,Length(path));
  lName := lName + '.mss';
  if FileExists(lName) then
  begin
    Result := True;
    llName := lName;
    n := ReadInteger('MSS','Audio_def',-1,llName);
    if (n > 0) then fPreferedStream := n-1;
    i := 1;
    while true do
    begin
      lName := ReadString('MSS','Audio_' + inttostr(i),'',llName);
      if (lName = '') then break;
      lang := AllocMem(SizeOf(TLangItem));
      if (Length(lName) > 2) and (lName[1] = '0') and (LowerCase(lName[2]) = 'x') then
      begin
        StringReplace(lName,'0x', '$', [rfIgnoreCase]);
        lang.aLCID := StrToInt(lName);
        GetLocaleInfo(lang.aLCID,LOCALE_SLANGUAGE,Buff,MAX_PATH);
        lName := Buff;
        if (n = 0) and (lang.aLCID = (GetThreadLocale and $FF)) then
           fPreferedStream := i-1;
        c1 := pos('(', lName);
        if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
      end;
      CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);
      fStreamList.Add(lang);
      inc(i);
    end;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageMorganMSSFile'); end; {$ENDIF}
end;

function TDCDSPFilter.ParseLanguageAVIStreamHeader(Filter: IBaseFilter): Boolean;
const
  BUFFER_SIZE = 50000;
var
  Pin: IPin;
  Reader: IAsyncReader;
  buf: PByteArray;
  i: integer;
  strh: ^AVISTREAMHEADER;
  lang: PLangItem;
  Buff : array[0..MAX_PATH-1] of Char;
  lName : WideString;
  c1: integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := False;
  if Filter.FindPin('Output', Pin) = S_OK then
  begin
    if Pin.QueryInterface(IID_IAsyncReader, Reader) = S_OK then
    begin
      buf := AllocMem(BUFFER_SIZE);

      if Reader.SyncRead(0,BUFFER_SIZE,pbyte(buf)) = S_OK then
      begin
        for i := 0 to BUFFER_SIZE -4 do
        begin
          if PDWORD(buf)^ = ckidSTREAMHEADER then
          begin
            PDWORD(strh) := PDWORD(buf);
            if strh.fccType = streamtypeAUDIO then
            begin
              if strh.wLanguage > 0 then
              begin
                Result := True;
                lang := AllocMem(SizeOf(TLangItem));
                lang.aLCID := strh.wLanguage;
                GetLocaleInfo(strh.wLanguage,LOCALE_SLANGUAGE,Buff,MAX_PATH);
                lName := Buff;
                c1 := pos('(', lName);
                if (c1 > 0) then delete(lName, c1 -1, Length(lName) - c1 + 2);
                CopyMemory(@lang.aName, PWideChar(lName), length(lName)*2+2);
                fStreamList.Add(lang);
              end;
            end;
          end;
        end;
      end;

      FreeMem(buf);
      Reader := nil;
    end;
    Pin := nil;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ParseLanguageAVIStreamHeader'); end; {$ENDIF}
end;

function TDCDSPFilter.CompleteConnect(direction: TPinDirection; ReceivePin: IPin): HRESULT;
const
  CLSID_GabestAviSplitter: TGuid = '{9736D831-9D6C-4E72-B6E7-560EF9181001}';
  CLSID_GabestAviSource: TGuid = '{CEA8DEFF-0AF7-4DB9-9A38-FB3C3AEFC0DE}';

var
  i : integer;
  freepin : Boolean;
  EnumFilters : IEnumFilters;
  Filter: IBaseFilter;
  ClassID : TGUID;
  TrackInfo : ITrackInfo;
  hr: HRESULT;
  found: Boolean;
  iFS: IFileSourceFilter;
  pName: PWChar;
  isAviSplitter: Boolean;
  iPP: IPersistMediaPropertyBag;
  iPB: IPropertyBag;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := inherited CompleteConnect(direction,ReceivePin);
  FcsFilter.Lock;
  try
    if fEnableStreamSwitching and (direction = PINDIR_INPUT) and
       (fPreferedStream > -1) and (fPreferedStream < fPinList.Count) then
    begin
      Enable(fPreferedStream,AMSTREAMSELECTENABLE_ENABLE);
      fPreferedStream := -1;
    end;

    if fEnableStreamSwitching and (direction = PINDIR_OUTPUT) and not fInitialized then
    begin
      isAviSplitter := False;
      found := False;
      fInitialized := True;
      fNeedMediatypeChange := True;
      ClearLanguageList;

      if Graph.EnumFilters(EnumFilters) = S_OK then
      begin
        while EnumFilters.Next(1,Filter,nil) = S_OK do
        begin
          Filter.GetClassID(ClassID);
          if IsEqualGUID(ClassID,CLSID_OggSplitter) then
          begin
            found := True;
            ParseLanguageOggSplitter(Filter);
            Filter := nil;
            break;
          end else if Filter.QueryInterface(IID_ITrackInfo,TrackInfo) = S_OK then
          begin
            found := True;
            ParseLanguageITrackInfo(TrackInfo);
            TrackInfo := nil;
            Filter := nil;
            break;
          end else if IsEqualGUID(CLSID_AviSplitter, ClassID) then
          begin
            isAviSplitter := True;
            if Filter.QueryInterface(IID_IPersistMediaPropertyBag, iPP) = S_OK then
            begin
              found := ParseLanguageMorganMediaPropBag(iPP);
              iPP := nil;
            end;
          end else
          if IsEqualGUID(ClassID,CLSID_GabestAviSplitter) or IsEqualGUID(ClassID,CLSID_GabestAviSource) then
          begin
            isAviSplitter := True;
            if Filter.QueryInterface(IID_IPropertyBag,iPB) = S_OK then
            begin
              found := ParseLanguageGabestAVIPropBag(iPB);
              iPB := nil;
            end;
          end;
          Filter := nil;
        end;
        EnumFilters := nil;

        // try to read Morgans "Filename.mss" File
        if not found and isAviSplitter then
        begin
          if Graph.EnumFilters(EnumFilters) = S_OK then
          begin
            while EnumFilters.Next(1,Filter,nil) = S_OK do
            begin
              if Filter.QueryInterface(IID_IFileSourceFilter, iFS) = S_OK then
              begin
                if iFS.GetCurFile(pName,nil) = S_OK then
                begin
                  if not ParseLanguageMorganMSSFile(pName) then
                  begin
                    ParseLanguageAVIStreamHeader(Filter);
                  end;
                end;
                iFS := nil;
                Filter := nil;
                break;
              end;
              Filter := nil;
            end;
            EnumFilters := nil;
          end;
        end;
      end;
    end;

    if fEnableStreamSwitching and (direction = PINDIR_INPUT) then
    begin
      freepin := False;
      for i := 0 to fPinList.Count -1 do
      begin
        if not TDCDSPFilterInputPin(fPinList.Items[i]).IsConnected then
        begin
          freepin := True;
          break;
        end;
      end;
      if not freepin then
        fPinList.Add(TDCDSPFilterInputPin.Create('Input Pin', Self, hr, 'Input', fPinList.Count, True, FcsReceive));
    end;

    if (direction = PINDIR_OUTPUT) and FInput.IsConnected and FOutput.IsConnected then
    begin
      fIsDVD := CheckDVD;
      fFirstVisRun := True;
    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CompleteConnect'); end; {$ENDIF}
end;

function TDCDSPFilter.Stop: HRESULT;
var
  pin: TDCDSPFilterInputPin;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  fPlaying := False;
{$IFDEF WITH_WINAMP}
  fVisTimer.Enabled := fPlaying;
{$ENDIF}
  fVisBufferCount := 0;

  pin := GetActivePin;
  FcsFilter.Lock;
  try
    if(FState = State_Stopped) then
    begin
      result := NOERROR;
      exit;
    end;
    // Succeed the Stop if we are not completely connected
    ASSERT((pin = nil) or (FOutput <> nil));
    if((pin = nil) or (pin.IsConnected = FALSE) or (FOutput.IsConnected = FALSE)) then
    begin
      FState := State_Stopped;
      FEOSDelivered := FALSE;
      result := NOERROR;
      exit;
    end;
    ASSERT(pin <> nil);
    ASSERT(FOutput <> nil);
    // decommit the input pin before locking or we can deadlock
    pin.Inactive;
    // synchronize with Receive calls
    FcsReceive.Lock;
    try
      FOutput.Inactive;
      // allow a class derived from CTransformFilter
      // to know about starting and stopping streaming
      result := StopStreaming;
      if SUCCEEDED(result) then
      begin
        // complete the state transition
        FState := State_Stopped;
        FEOSDelivered := FALSE;
      end;
    finally
      FcsReceive.UnLock;
    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Stop'); end; {$ENDIF}
end;

function TDCDSPFilter.InitializeOutputSample(Sample: IMediaSample; out OutSample: IMediaSample): HRESULT;
var
  Props: PAMSample2Properties;
  Flags: DWORD;
  Start, Stop: PReferenceTime;
  OutSample2: IMediaSample2;
  OutProps: TAMSample2Properties;
  MediaStart, MediaEnd: Int64;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Props := GetActivePin.SampleProps;
  if FSampleSkipped then Flags := AM_GBF_PREVFRAMESKIPPED else Flags := 0;
  if(not BOOL(Props.dwSampleFlags and AM_SAMPLE_SPLICEPOINT)) then Flags := Flags or AM_GBF_NOTASYNCPOINT;

  ASSERT(TDCDSPFilterOutputPin(FOutput).GetAllocator <> nil);
  if  BOOL(Props.dwSampleFlags and AM_SAMPLE_TIMEVALID) then Start := @Props.tStart else Start := nil;
  if  BOOL(Props.dwSampleFlags and AM_SAMPLE_STOPVALID) then Stop := @Props.tStop else Stop := nil;
  result := TDCDSPFilterOutputPin(FOutput).GetAllocator.GetBuffer(OutSample, Start, Stop, Flags);
  if FAILED(result) then exit;
  ASSERT(OutSample <> nil);
  if SUCCEEDED(OutSample.QueryInterface(IID_IMediaSample2, OutSample2)) then
  begin
    ASSERT(SUCCEEDED(OutSample2.GetProperties(4*4, OutProps)));
    OutProps.dwTypeSpecificFlags := Props.dwTypeSpecificFlags;
    OutProps.dwSampleFlags := (OutProps.dwSampleFlags and AM_SAMPLE_TYPECHANGED) or
        (Props.dwSampleFlags and (not AM_SAMPLE_TYPECHANGED));

    OutProps.tStart := Props.tStart;
    OutProps.tStop  := Props.tStop;
    OutProps.cbData := (4*4) + (2*8);

    OutSample2.SetProperties((4*4)+(2*8), OutProps);
    if BOOL(Props.dwSampleFlags and AM_SAMPLE_DATADISCONTINUITY) then FSampleSkipped := FALSE;
    OutSample2 := nil;
  end else
  begin
    if BOOL(Props.dwSampleFlags and AM_SAMPLE_TIMEVALID) then
      OutSample.SetTime(@Props.tStart, @Props.tStop);
    if BOOL(Props.dwSampleFlags and AM_SAMPLE_SPLICEPOINT) then
      OutSample.SetSyncPoint(True);
    if BOOL(Props.dwSampleFlags and AM_SAMPLE_DATADISCONTINUITY) then
    begin
      OutSample.SetDiscontinuity(True);
      FSampleSkipped := FALSE;
    end;
    // Copy the media times
    if (Sample.GetMediaTime(MediaStart,MediaEnd) = NOERROR) then
      OutSample.SetMediaTime(@MediaStart, @MediaEnd);
  end;

  Result := S_OK;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.InitializeOutputSample'); end; {$ENDIF}
end;

function TDCDSPFilter.Receive(Sample: IMediaSample): HRESULT;
var
  Props: PAMSample2Properties;
  OutSample: IMediaSample;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Props := GetActivePin.SampleProps;
  if(Props.dwStreamId <> AM_STREAM_MEDIA) then
  begin
    result := TDCDSPFilterOutputPin(FOutput).FInputPin.Receive(Sample);
    exit;
  end;
  ASSERT(FOutput <> nil) ;
  result := InitializeOutputSample(Sample, OutSample);
  if FAILED(result) then exit;

  result := Transform(Sample, OutSample);
  if FAILED(result) then
  begin
    exit;
  end else
  begin
    if (result = NOERROR) then
    begin
      result := TDCDSPFilterOutputPin(FOutput).FInputPin.Receive(OutSample);
      FSampleSkipped := FALSE;   // last thing no longer dropped
    end else
    begin
      if (result = S_FALSE) then
      begin
        OutSample := nil;
        FSampleSkipped := True;
        if not FQualityChanged then
        begin
          NotifyEvent(EC_QUALITY_CHANGE,0,0);
          FQualityChanged := True;
        end;
        result := NOERROR;
        exit;
      end;
    end;
  end;
  OutSample := nil;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Receive'); end; {$ENDIF}
end;
{$IFDEF WITH_TRAYICON}
function TDCDSPFilter.JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if not Assigned(pGraph) and (FRunningObjectTable <> 0) then
    begin
      RemoveGraphFromRot(FRunningObjectTable);
      FRunningObjectTable := 0;
    end;

    if not Assigned(pGraph) and Assigned(fTrayIcon.TrayIcon) then
    begin
      PostThreadMessage(fTrayIcon.ThreadID,UM_CLOSE_PROPPAGE,0,0);
      PostThreadMessage(fTrayIcon.ThreadID,WM_QUIT,0,0);
      if(WaitForSingleObject(fTrayIcon.Thread, 10000) <> WAIT_OBJECT_0) then
        TerminateThread(fTrayIcon.Thread, DWORD(-1));
      fTrayIcon.TrayIcon := nil;
    end;
  finally
    FcsFilter.UnLock;
    Result := inherited JoinFilterGraph(pGraph, pName);
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.JoinFilterGraph'); end; {$ENDIF}
end;
{$ENDIF}
{$IFDEF WITH_WINAMP}
procedure TDCDSPFilter.KillWinampDSPWindows;
{$IFDEF WITH_REGISTRY}
var
  Reg : TRegistry;
  str : TStringList;
  i : integer;
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
{$IFDEF WITH_REGISTRY}
  Reg := TRegistry.Create ;
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKeyReadOnly('SOFTWARE\DSP-worx\DC-DSP Filter\WADSPWindows') then
  begin
    str := TStringList.Create;
    Reg.GetValueNames(str);
    if str.Count > 0 then
      for i := 0 to str.Count -1 do ShowWinampDSPWindow(str.Strings[i],SW_HIDE);
    str.Free;
    Reg.CloseKey;
  end;
  Reg.Free;
{$ENDIF}
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.KillWinampDSPWindows'); end; {$ENDIF}
end;
{$ENDIF}
function TDCDSPFilter.Pause: HRESULT;
{$IFDEF WITH_WINAMP}
var
  LoadedPlugin : integer;
  LoadedPluginStr : String;
  LoadedVisPlugin : integer;
  LoadedVisPluginStr : String;
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
{$IFDEF WITH_TRAYICON}
    if fFirstRun then
    begin
      if (fBalloonHint and fTrayIcon.Visible and Assigned(fTrayIcon.TrayIcon) and fTrayIcon.TrayIcon.Visible) then
      begin
        if fEnableStreamSwitching and (fPinList.Count > 2) then
        begin
          PostThreadMessage(fTrayIcon.ThreadID,UM_SHOW_BALLOON,0,0);
        end;
      end;
{$IFNDEF WITH_WINAMP}
      fFirstRun := False;
{$ENDIF}
    end;
{$ENDIF}
{$IFDEF WITH_WINAMP}
    if fFirstRun then
    begin
      LoadedPlugin := fLoadedPluginIndex;
      LoadedPluginStr := fLoadedPlugin;
      LoadedVisPlugin := fCurrentVisualPluginIndex;
      LoadedVisPluginStr := fCurrentVisualPlugin;

      set_UnloadDSPPlugin;
      set_StopWinampVisPlugin;
      if (LoadedPluginStr <> '') and (LoadedPlugin > -1) and FileExists(LoadedPluginStr) and (set_DSPPlugin(GetDesktopWindow,PChar(LoadedPluginStr)) = S_OK) then
      begin
        fLoadedPluginIndex := LoadedPlugin;
        set_DSPSubPlugin(LoadedPlugin);
      end;

      KillWinampDSPWindows;

      if fWinampVisAutoStart and FileExists(LoadedVisPluginStr) and (LoadedVisPlugin > -1) then
        set_WinampVisPlugin(PChar(LoadedVisPluginStr),LoadedVisPlugin);

      fFirstRun := False;
    end;
{$ENDIF}

  fPlaying := False;
{$IFDEF WITH_WINAMP}
  fVisTimer.Enabled := fPlaying;
{$ENDIF}
  finally
    FcsFilter.UnLock;
    Result := inherited Pause;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Pause'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.Run(tStart: TReferenceTime): HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fPlaying := True;
{$IFDEF WITH_WINAMP}
    fVisTimer.Enabled := fPlaying;
{$ENDIF}
    fEOS := False;
  finally
    FcsFilter.UnLock;
    Result := inherited Run(tStart);
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Run'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.CheckTransform(mtIn,mtOut: PAMMediaType): HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if not IsEqualGUID (mtIn^.majortype ,MEDIATYPE_Audio) then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckTransform -> Mediatype is not Audio -> Connection dropped'); {$ENDIF}
      Result := VFW_E_INVALIDMEDIATYPE;
      Exit;
    end;

    if (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_PCM)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_DOLBY_AC3_SPDIF)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_NULL)) then // NULL because of SoundForge
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckTransform -> MediaSubtype is not PCM/Float -> Connection dropped'); {$ENDIF}
      Result := VFW_E_INVALIDSUBTYPE;
      Exit;
    end;

    if not IsEqualGUID (mtIn^.formattype,FORMAT_WaveFormatEx) then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckTransform -> Formattype is not WaveformatEx -> Connection dropped'); {$ENDIF}
      Result :=  VFW_E_TYPE_NOT_ACCEPTED;
      Exit;
    end;

    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CheckTransform'); end; {$ENDIF}
end;
{$IFDEF WITH_BITRATE_CONVERSION}
procedure TDCDSPFilter.ChangeMediaType(var pmt: PamMediaType);
var
  fmt: PWaveFormatEx;
  fmtex: PWaveFormatExtensible;
  Mask: Cardinal;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if fEnableBRConv and not fStream.SPDIF and not fStream.DTS then
  begin
    pmt.pbFormat := CoTaskMemRealloc(pmt.pbFormat,SizeOf(TWaveFormatExtensible));
    fmtex := PWaveFormatExtensible(pmt.pbFormat);
    fmt := @fmtex^.Format;
    Mask := 0;
    // temporary store the Channel Mask
    if fmt.wFormatTag = WAVE_FORMAT_EXTENSIBLE then Mask := fmtex.dwChannelMask;

    case fBRConv.BitRate of
      br8BitInteger:
      begin
        if fStream.Channels <= 2 then
        begin
          fmt.wFormatTag := WAVE_FORMAT_PCM;
          fmt.cbSize := 0;
        end else
        begin
          fmt.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
          fmt.cbSize := 22;
        end;
        fmt.nChannels := fStream.Channels;
        fmt.nSamplesPerSec := fStream.Frequency;
        fmt.wBitsPerSample := 8;
        fmt.nBlockAlign := (fmt.wBitsPerSample div 8) * fmt.nChannels;
        fmt.nAvgBytesPerSec := fmt.nSamplesPerSec * fmt.nBlockAlign;

        fmtex.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
        fmtex.Samples.wValidBitsPerSample := 8;
        fmtex.dwChannelMask := Mask;
      end;
      br16BitInteger:
      begin
        if fStream.Channels <= 2 then
        begin
          fmt.wFormatTag := WAVE_FORMAT_PCM;
          fmt.cbSize := 0;
        end else
        begin
          fmt.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
          fmt.cbSize := 22;
        end;
        fmt.nChannels := fStream.Channels;
        fmt.nSamplesPerSec := fStream.Frequency;
        fmt.wBitsPerSample := 16;
        fmt.nBlockAlign := (fmt.wBitsPerSample div 8) * fmt.nChannels;
        fmt.nAvgBytesPerSec := fmt.nSamplesPerSec * fmt.nBlockAlign;

        fmtex.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
        fmtex.Samples.wValidBitsPerSample := 16;
        fmtex.dwChannelMask := Mask;
      end;
      br24BitInteger:
      begin
        fmt.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
        fmt.cbSize := 22;
        fmt.nChannels := fStream.Channels;
        fmt.nSamplesPerSec := fStream.Frequency;
        fmt.wBitsPerSample := 24;
        fmt.nBlockAlign := (fmt.wBitsPerSample div 8) * fmt.nChannels;
        fmt.nAvgBytesPerSec := fmt.nSamplesPerSec * fmt.nBlockAlign;

        fmtex.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
        fmtex.Samples.wValidBitsPerSample := 24;
        fmtex.dwChannelMask := Mask;
      end;
      br32BitInteger:
      begin
        fmt.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
        fmt.cbSize := 22;
        fmt.nChannels := fStream.Channels;
        fmt.nSamplesPerSec := fStream.Frequency;
        fmt.wBitsPerSample := 32;
        fmt.nBlockAlign := (fmt.wBitsPerSample div 8) * fmt.nChannels;
        fmt.nAvgBytesPerSec := fmt.nSamplesPerSec * fmt.nBlockAlign;

        fmtex.SubFormat := KSDATAFORMAT_SUBTYPE_PCM;
        fmtex.Samples.wValidBitsPerSample := 32;
        fmtex.dwChannelMask := Mask;
      end;
      br32BitFloat:
      begin
        fmt.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
        fmt.cbSize := 22;
        fmt.nChannels := fStream.Channels;
        fmt.nSamplesPerSec := fStream.Frequency;
        fmt.wBitsPerSample := 32;
        fmt.nBlockAlign := (fmt.wBitsPerSample div 8) * fmt.nChannels;
        fmt.nAvgBytesPerSec := fmt.nSamplesPerSec * fmt.nBlockAlign;

        fmtex.SubFormat := KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
        fmtex.Samples.wValidBitsPerSample := 32;
        fmtex.dwChannelMask := Mask;
      end;
    end;

    pmt.cbFormat := SizeOf(TWaveFormatEx) + fmt.cbSize;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ChangeMediaType'); end; {$ENDIF}
end;
{$ENDIF}
function TDCDSPFilter.GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT;
var
  i : integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    {$IFDEF DEBUG} dbg(fThisInstance, 'TDCDSPFilter.GetMediaType'); {$ENDIF}
    if not Assigned(@MediaType) or not Assigned(MediaType) then
    begin
      Result := E_POINTER;
      Exit;
    end;

    if not Input.IsConnected then
    begin
      Result := E_UNEXPECTED;
      Exit;
    end;

    if Position < 0 then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;

    if Position > 0 then
    begin
      Result := VFW_S_NO_MORE_ITEMS;
      Exit;
    end;

    for i := 0 to fPinList.Count -1 do
    begin
      if not TDCDSPFilterInputPin(fPinList.Items[i]).Blocked then
      begin
        TDCDSPFilterInputPin(fPinList.Items[i]).ConnectionMediaType(MediaType^);
  {$IFDEF WITH_BITRATE_CONVERSION}
        ChangeMediaType(MediaType);
  {$ENDIF}
        Result := S_OK;
        Exit;
      end;
    end;

    Result := E_UNEXPECTED;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetMediaType'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.DecideBufferSize(Alloc: IMemAllocator; propInputRequest: PALLOCATORPROPERTIES): HRESULT;
var
  hr : HRESULT;
  Actual : TAllocatorProperties;
  max_buffer_size : integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if not Input.IsConnected then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  ASSERT(Alloc <> nil);
  ASSERT(propInputRequest <> nil);
  // way too much buffer, but i don´t know any solution now ...
  // if Winamp DSP Plugins (especially PaceMaker or any other Pitch Plugin)
  // sounds choppy, then increase this Buffer to a double (or 4x) value
  max_buffer_size := 352800 * fStream.Channels * (fRealStream.Bits div 8);
  propInputRequest.cBuffers := 1;
  propInputRequest.cbBuffer := max_buffer_size;
//  propInputRequest.cbAlign := 16;

  ASSERT(propInputRequest.cbBuffer = max_buffer_size);
  ZeroMemory(@Actual, sizeof(TAllocatorProperties));
  hr := Alloc.SetProperties(propInputRequest^,Actual);
  if hr <> S_OK then
  begin
    Result := hr;
    Exit;
  end;

  ASSERT(Actual.cBuffers > 0);

  if ((propInputRequest.cBuffers > Actual.cBuffers) or (propInputRequest.cbBuffer > Actual.cbBuffer)) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  Result := S_OK;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.DecideBufferSize'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    // Warcraft III crashes when the Filter is connected.
    // Just check if "war3.exe" is our main Application.
    // If so, then drop the connection.
    if LowerCase(ExtractFileName(GetModuleName(GetModuleHandle(nil)))) = 'war3.exe' then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> Warcraft III detected. Connection dropped'); {$ENDIF}
      Result := VFW_E_TYPE_NOT_ACCEPTED;
      Exit;
    end;

    if not IsEqualGUID (mtIn^.majortype ,MEDIATYPE_Audio) then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> Mediatype is not Audio -> Connection dropped'); {$ENDIF}
      Result := VFW_E_INVALIDMEDIATYPE;
      Exit;
    end;

    if (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_PCM)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_DOLBY_AC3_SPDIF)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT)) and
       (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_NULL)) then // Null because of SoundForge
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> MediaSubtype is not PCM/Float -> Connection dropped'); {$ENDIF}
      {$IFDEF DEBUG} dbg(fThisInstance, GUIDToString(mtIn^.subtype)); {$ENDIF}

      Result := VFW_E_INVALIDSUBTYPE;
      Exit;
    end;

    if not IsEqualGUID (mtIn^.formattype,FORMAT_WaveFormatEx) then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> Formattype is not WaveformatEx -> Connection dropped'); {$ENDIF}
      Result :=  VFW_E_TYPE_NOT_ACCEPTED;
      Exit;
    end;

    fStream.Frequency := PWaveFormatEx(mtIn^.pbFormat)^.nSamplesPerSec;
    fStream.Channels := PWaveFormatEx(mtIn^.pbFormat)^.nChannels;
    fStream.Bits := PWaveFormatEx(mtIn^.pbFormat)^.wBitsPerSample;
    if fStream.Frequency = 0 then fStream.Frequency := 44100;
    // Last Check to see if we´re dealing with 8,16,24 or 32 Bit Audio.
    // If not then drop the Connection
    if (fStream.Bits <> 32) and (fStream.Bits <> 24) and (fStream.Bits <> 16) and (fStream.Bits <> 8) then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> Stream is not 8/16/24/32 Bit -> Connection dropped'); {$ENDIF}
      Result := VFW_E_TYPE_NOT_ACCEPTED;
      Exit;
    end;

    // allow SPDIF or DTS types to connect, but no DSP or VisualBuffering is done.
    // Needed for dynamic MedayType change with AC3 Filter.
    fStream.SPDIF := (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DOLBY_AC3_SPDIF) or
                     (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DOLBY_AC3) or
                     (IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_DOLBY_AC3_SPDIF));
    fStream.DTS :=   (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DTS) or
                     (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DVD_DTS);

    fStream.Float := IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT) or (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) or
       ((PWaveFormatExtensible(mtIn^.pbFormat)^.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and  IsEqualGUID(PWaveFormatExtensible(mtIn^.pbFormat)^.SubFormat,KSDATAFORMAT_SUBTYPE_IEEE_FLOAT));

    {$IFDEF DEBUG} dbg(fThisInstance,'CheckInputType -> Mediatype checked and is supported'); {$ENDIF}

    fRealStream := GetConvertedStream;

    if Assigned(fCallback) and not CompareMem(@fLastMediaType,@fRealStream,SizeOf(TDSStream))
      then fCallback.MediaTypeChanged(@fRealStream);
    fLastMediaType := fRealStream;
    FreeMediaType(@fCurrentMediaType);
    CopyMediaType(@fCurrentMediaType,mtIn);
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CheckInputType'); end; {$ENDIF}
end;
{$IFDEF WITH_WINAMP}
procedure TDCDSPFilter.OnWinampVisTimer(Sender : TObject);
var
  Buffer : Pointer;
  Size : integer;
  BufferSize : integer;
  Stream : PDSStream;
begin
  try
    if get_VisualData(Buffer,Size,Stream) = S_OK then
    begin
  //    {$IFDEF DEBUG}  dbg(fThisInstance,'OnWinampVisTimer'); {$ENDIF}
      BufferSize := Trunc(fStream.Frequency * (fRealStream.Bits div 8) * fStream.Channels / (1000 div fVisTimer.Interval));
      if BufferSize > Size then BufferSize := Size;
      ProcessVisualData(Buffer,BufferSize,fStream.Frequency,fRealStream.Bits,fStream.Channels,fVisTimer.Interval,fRealStream.Float);
    end;
  except
    {$IFDEF DEBUG}  dbg(fThisInstance,'OnWinampVisTimer crashed !!!'); {$ENDIF}
  end;
end;
{$ENDIF}

const
  IID_ICrossfadingRenderer: TGuid = '{86AADA26-8B99-48EF-8F4D-ABD61B978245}';
type
  ICrossfadingRenderer = interface(IUnknown)
  ['{86AADA26-8B99-48EF-8F4D-ABD61B978245}']
    function GetDevice(out Guid: TGuid): HRESULT; stdcall;
    function SetDevice(Guid: TGuid): HRESULT; stdcall;
    function GetFadingSeekStop(out Fading: Integer): HRESULT; stdcall;
    function SetFadingSeekStop(Fading: Integer): HRESULT; stdcall;
    function GetFadingCross(out Fading: Integer): HRESULT; stdcall;
    function SetFadingCross(Fading: Integer): HRESULT; stdcall;
    function GetAllowVideo(out Allow: Boolean): HRESULT; stdcall;
    function SetAllowVideo(Allow: Boolean): HRESULT; stdcall;
  end;

function TDCDSPFilter.StartStreaming: HRESULT;
begin
{$IFDEF WITH_INTERNAL_DSP}
  if AllocateVisBuffer = S_OK then fFilters.Init(@fRealStream);
{$ELSE}
  AllocateVisBuffer;
{$ENDIF}
  Result := inherited StartStreaming;
end;

function TDCDSPFilter.AllocateVisBuffer : HRESULT;
var
  secs: Integer;
  EnumFilters: IEnumFilters;
  Filter: IBaseFilter;
  Fading: ICrossfadingRenderer;
  s1, s2: Integer;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  Result := S_FALSE;

  if not FVisualBufferEnabled then
  begin
    Result := S_OK;
    Exit;
  end;

  fVisBufferCount := 0;
  fVisBufferStartTime := 0;

  if CompareMem(@fLastType,@fRealStream,SizeOf(TDSStream)) then Exit;
  fLastType := fRealStream;

  // fVisBufferDivider is needed to convert bytes <> SampleTime
  fVisBufferDivider := 10000000 / fStream.Frequency / fStream.Channels / (fRealStream.Bits div 8);
  if fVisBuffer <> nil then
  begin
    FreeMemory(fVisBuffer);
    fVisBuffer := nil;
  end;
  // Allocate Memory for Visual Buffering (4 seconds for every Channel) with some extrabytes for swapping
  secs := 4;
  // try to find Crossfading Renderer to get it´s Buffering Settings
  if Assigned(FGraph) and (FGraph.EnumFilters(EnumFilters) = S_OK) then
  begin
    while (EnumFilters.Next(1, Filter, nil) = S_OK) do
    begin
      if (Filter.QueryInterface(IID_ICrossfadingRenderer, Fading) = S_OK) then
      begin
        Fading.GetFadingSeekStop(s1);
        Fading.GetFadingCross(s2);

        if s1 > s2
          then secs := s1 div 1000 + 1
          else secs := s2 div 1000 + 1;

        Fading := nil;
        Filter := nil;
        break;
      end;
      Filter := nil;
    end;
    EnumFilters := nil;
  end;
  fVisBufferMaxBytes := fStream.Frequency * fStream.Channels * (fRealStream.Bits div 8) * secs;
  fVisBufferSwap := secs * fStream.Channels * (fRealStream.Bits div 8) * 16384;
  fVisBuffer := AllocMem(fVisBufferMaxBytes + fVisBufferSwap * 2);
  fVisModBytes := fStream.Channels * (fRealStream.Bits div 8);
  Result := S_OK;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.OnWinampVisTimer'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.ApplyVisual(Buffer : Pointer; Length : integer) : HRESULT;
begin
  if not FVisualBufferEnabled then
  begin
    Result := S_FALSE;
    Exit;
  end;

  try
    if (fVisBuffer = nil) or fStream.SPDIF or fStream.DTS then
    begin
      Result := S_FALSE;
      Exit;
    end;

    if (fVisBufferCount > fVisBufferMaxBytes) then
    begin
      fVisBufferStartTime := fVisBufferStartTime + Trunc(Length * fVisBufferDivider);
      // Added fVisBufferRollOver to prevent low Buffersizes on the End of the Buffer
      if fCanRollOver and ((fVisBufferRollOver + Length) < fVisBufferSwap) then
      begin
        Move(Buffer^,fVisBuffer[fVisBufferRollOverPos],Length);
        fVisBufferRollOverPos := fVisBufferRollOverPos + Length;
        fVisBufferRollOver := fVisBufferRollOver + Length;
      end else fCanRollOver := False;
      // Instead of using MoveMemory, which is very CPU intensive when moving
      // alot of Data in a Short Time, this Visualazation Model starts adding
      // new Data on a position where Buffer isn´t needed anymore.
      if fVisBufferWrap > fVisBufferMaxBytes then
      begin
        fCanRollOver := True;
        fVisBufferRollOver := Length;
        fVisBufferRollOverPos := fVisBufferWrap + Length;
        Move(Buffer^,fVisBuffer^,Length);
        Move(Buffer^,fVisBuffer[fVisBufferWrap],Length);
        fVisBufferWrap := Length;
        Result := S_OK;
        Exit;
      end;

      Move(Buffer^,fVisBuffer[fVisBufferWrap],Length);
      inc(fVisBufferWrap,Length);
    end else
    begin
      Move(Buffer^,fVisBuffer[fVisBufferCount],Length);
      inc(fVisBufferCount,Length);
      fVisBufferWrap := 0;
      fVisBufferRollOver := 0;
      fVisBufferRollOverPos := 0;
      fCanRollOver := False;
    end;
    Result := S_OK;
  except
    Result := E_FAIL;
  end;
end;
{*******************************************************************************}
function TDCDSPFilter.GetBufferAtTimeStamp(Time : REFERENCE_TIME; out Bytes : integer) : PChar;

  function Checkposition(Position : integer) : integer;
  begin
    // Checks where the Current Requested Buffer is allocated.
    inc(Position,fVisBufferWrap);
    if Position > fVisBufferCount then dec(Position,fVisBufferCount);
    // Check to see if we´re on the right position on the Buffer.
    // Otherwise the Resulting Buffer is Distorted (if higher than 8 Bit)
    // Checks also if we start with the first Channel
    Result := Position - (Position mod fVisModBytes);
  end;

var
  tmp : integer;
begin
  if not FVisualBufferEnabled then
  begin
    Result := nil;
    Bytes := 0;
    Exit;
  end;

  try
    Bytes := 0;
    if fStream.SPDIF or fStream.DTS or (Time < fVisBufferStartTime) then
    begin
      Result := nil;
      Exit;
    end else
    begin
      tmp := CheckPosition(Trunc((Time - fVisBufferStartTime) / fVisBufferDivider));
      // Last check to see if we´re inside the Buffer
      if (tmp > fVisBufferCount) or  (tmp < 0) then
      begin
        Result := nil;
        Exit;
      end;
      Bytes := fStream.Channels * (fRealStream.Bits div 8) * 16384;
      Result := @fVisBuffer[tmp];
    end;
  except
    Result := nil;
    Bytes := 0;
  end;
end;
{*******************************************************************************}
function TDCDSPFilter.Copy(Source, Dest: IMediaSample): HRESULT;
var
  SourceBuffer, DestBuffer: PByte;
  TimeStart, TimeEnd: TReferenceTime;
  MediaStart, MediaEnd: int64;
  MediaType: PAMMediaType;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if (Source.GetTime(TimeStart, TimeEnd) = S_OK) then Dest.SetTime(@TimeStart, @TimeEnd);
  if (Source.GetMediaTime(MediaStart,MediaEnd) = S_OK) then Dest.SetMediaTime(@MediaStart, @MediaEnd);
  if Source.IsDiscontinuity = S_OK then Dest.SetDiscontinuity(True);
  if Source.IsPreroll = S_OK then Dest.SetPreroll(True);
  Source.GetPointer(SourceBuffer);
  Dest.GetPointer(DestBuffer);
  CopyMemory(DestBuffer, SourceBuffer, Source.GetActualDataLength);
  Dest.SetActualDataLength(Source.GetActualDataLength);
  if (Source.GetMediaType(MediaType) = S_OK) and Assigned(MediaType) then
  begin
    Dest.SetMediaType(MediaType);
    DeleteMediaType(MediaType);
  end;
  Result := S_OK;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.ApplyVisual'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.Transform(msIn, msOut: IMediaSample): HRESULT;
var
  pSrc : PByte;
  Length : integer;
  NewSize : integer;
  mt : PAMMediaType;
  hr : HRESULT;
  OldLength : integer;
  mi,mo : TReferenceTime;
  mmi,mmo : Int64;
  {$IFDEF DEBUG}
//  StreamPos : TReferenceTime;
  {$ENDIF}
  i : integer;
  pmt : PAMMediaType;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsReceive.Lock;
  try
    copy(msIn,msOut);
    msOut.GetPointer(pSrc);
    Length := msOut.GetActualDataLength;
    OldLength := Length;

    {$IFDEF DEBUG}
  //    msOut.GetTime(mi,mo);
  //    StreamTime(StreamPos);
  //    dbg(fThisInstance,'Transform -> StreamTime : ' + inttostr(StreamPos));
  //    dbg(fThisInstance,'Transform -> StartTime : ' + inttostr(mi));
  //    dbg(fThisInstance,'Transform -> StopTime : ' + inttostr(mo));
  //    dbg(fThisInstance,'Transform -> Difference : ' + inttostr((mi - StreamPos) div 10000));
    {$ENDIF}

    // Dynamic MediaType change (eg. AC3 Filter)
    if msOut.GetMediaType(mt) = S_OK then
    begin
      {$IFDEF DEBUG} dbg(fThisInstance,'Transform -> Mediatype changed'); {$ENDIF}
      fMediaLocked := True;
      hr := CheckInputType(mt);
      if hr <> S_OK then
      begin
        Result := hr;
        DeleteMediaType(mt);
        fMediaLocked := False;
        Exit;
      end;
  {$IFDEF WITH_INTERNAL_DSP}
      if AllocateVisBuffer = S_OK then fFilters.Init(@fRealStream);
  {$ELSE}
      AllocateVisBuffer;
  {$ENDIF}
      fMediaLocked := False;
      fVisBufferCount := 0;
      fVisBufferStartTime := -1;
      fVisBufferRollOver := 0;
  {$IFDEF WITH_BITRATE_CONVERSION}
      ChangeMediaType(mt);
  {$ENDIF}
      msOut.SetMediaType(mt);
      DeleteMediaType(mt);
      fNeedMediatypeChange := False;
    end;

  {$IFDEF WITH_BITRATE_CONVERSION}
    if fEnableBRConv and fBRBeforeDSP and not fStream.SPDIF and not fStream.DTS then
    begin
      Length := fBRConv.Process(pSrc,Length,fStream.Bits,fStream.Float);
      msOut.SetActualDataLength(Length);
    end;
  {$ENDIF}

    // Visual Buffering. Set new StartTime if needed.
    fVisBufferDelay := Trunc(Length * fVisBufferDivider);
    if not fIsDVD then
    begin
      msOut.GetTime(mi,mo);
      if (fVisBufferStartTime = -1) or (fVisBufferStartTime > mi) then
      begin
        if fFirstVisRun then
        begin
          fVisBufferStartTime := mi;
          fFirstVisRun := False;
        end else
        begin
          fVisBufferStartTime := mi - fVisBufferDelay;
        end;
      end;
    end;

    if fNeedMediatypeChange or fForceMediatypeChange then
    begin
      fNeedMediatypeChange := False;
      for i := 0 to fPinList.Count -1 do
      begin
        if not TDCDSPFilterInputPin(fPinList.Items[i]).Blocked then
        begin
          pmt := TDCDSPFilterInputPin(fPinList.Items[i]).CurrentMediaType.MediaType;
          if not TBCMediaType(pmt).Equal(@fCurrentMediaType) or fForceMediatypeChange then
          begin
            fForceMediatypeChange := False;
            mt := CreateMediaType(TDCDSPFilterInputPin(fPinList.Items[i]).CurrentMediaType.MediaType);
            CheckInputType(mt);
  {$IFDEF WITH_BITRATE_CONVERSION}
            ChangeMediaType(mt);
  {$ENDIF}
            msOut.SetMediaType(mt);
            DeleteMediaType(mt);
  {$IFDEF WITH_INTERNAL_DSP}
            if AllocateVisBuffer = S_OK then fFilters.Init(@fRealStream);
  {$ELSE}
            AllocateVisBuffer;
  {$ENDIF}
            fVisBufferCount := 0;
            fVisBufferStartTime := -1;
            fVisBufferRollOver := 0;
          end;
          break;
        end;
      end;
    end;

  {$IFDEF WITH_CPU_METER}
    fCPUMeter.StartMeasure;
  {$ENDIF}

    if not fVISAfterDSP then
    begin
      fLocked := True;
      ApplyVisual(pSrc,Length);
      fLocked := False;
    end;

    fStream.Size := Length;

    if not fStream.SPDIF and not fStream.DTS and Assigned(fCallback) and fPCMDataBeforeMainDSP then
    begin
      fCallback.PCMDataCB(pSrc,Length,NewSize,@fRealStream);
      if NewSize <> Length then
      begin
        Length := NewSize;
        msOut.SetActualDataLength(Length);
      end;
    end;

    if not fStream.SPDIF and not fStream.DTS then
    begin
    {$IFDEF WITH_INTERNAL_DSP}
      Length := fFilters.Process(pSrc,Length);
    {$ENDIF}
    {$IFDEF WITH_WINAMP}
      Length := fDSPPlugin.Process(pSrc,Length,fStream.Channels,fRealStream.Bits,fStream.Frequency,fRealStream.Float);
    {$ENDIF}
      msOut.SetActualDataLength(Length);
    end;

    if not fStream.SPDIF and not fStream.DTS and Assigned(fCallback) and not fPCMDataBeforeMainDSP then
    begin
      fCallback.PCMDataCB(pSrc,Length,NewSize,@fRealStream);
      if NewSize <> Length then
      begin
        Length := NewSize;
        msOut.SetActualDataLength(Length);
      end;
    end;

  {$IFDEF WITH_CPU_METER}
    fCPUMeter.StopMeasure;
  {$ENDIF}

    // Set new TimeStamps if Buffersize has changed.
    // Seems to be an useless try to keep Audio and Video Data synchronized...
    if (OldLength <> Length) then
    begin
      if(msOut.GetMediaTime(mmi,mmo) = S_OK) then
      begin
        mmo := mmi + (Int64(Length) * One_Second div fStream.Channels div (fRealStream.Bits div 8)div fStream.Frequency);
        msOut.SetMediaTime(@mmi,@mmo);
      end;
      if msOut.GetTime(mi,mo) = S_OK then
      begin
        mo := mi + (Int64(Length) * One_Second div fStream.Channels div (fRealStream.Bits div 8)div fStream.Frequency);
        msOut.SetTime(@mi,@mo);
      end;
    end;
    msOut.SetSyncPoint(True);

    if fEnableDelay then
    begin
      if msOut.GetTime(mi,mo) = S_OK then
      begin
        mo := mo + (fDelay * 10000);
        mi := mi + (fDelay * 10000);
        msOut.SetTime(@mi,@mo);
      end;
    end;

    if fVISAfterDSP then
    begin
      fLocked := True;
      ApplyVisual(pSrc,Length);
      fLocked := False;
    end;

  {$IFDEF WITH_BITRATE_CONVERSION}
    if fEnableBRConv and not fBRBeforeDSP and not fStream.SPDIF and not fStream.DTS then
      msOut.SetActualDataLength(fBRConv.Process(pSrc,Length,fStream.Bits,fStream.Float));
  {$ENDIF}

    Result := S_OK;
  finally
    FcsReceive.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Transform'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.BeginFlush : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  fFlushing := True;
  fVisBufferCount := 0;
  fVisBufferStartTime := -1;
  fVisBufferRollOver := 0;
{$IFDEF WITH_INTERNAL_DSP}
  fFilters.Flush;
{$ENDIF}
  if Assigned(fCallback) then fCallback.Flush;
{$IFDEF WITH_CPU_METER}
  fCPUMeter.reset;
{$ENDIF}
  Result := inherited BeginFlush;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.BeginFlush'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.EndFlush : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  fFlushing := False;
  Result := inherited EndFlush;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.EndFlush'); end; {$ENDIF}
end;
{*** DCDSPFilter Interface ********************************************************}
function TDCDSPFilter.get_VISafterDSP(out AfterDSP : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    AfterDSP := fVISafterDSP;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_VISafterDSP'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_VISafterDSP(AfterDSP : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fVISafterDSP := AfterDSP;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_VISafterDSP'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Stream := @fRealStream;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_StreamInfo'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_CallbackPCM(Callback : IDCDSPFilterPCMCallBack): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fCallback := Callback;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_CallbackPCM'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream): HRESULT; stdcall;
var
  StreamPos : TReferenceTime;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  if not FVisualBufferEnabled then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;

  FcsFilter.Lock;
  try
//    FcsReceive.Lock;
//    try
      Stream := @fRealStream;
      if fMediaLocked or fFlushing or (not fPlaying) or
         (StreamTime(StreamPos) <> S_OK) or (fVisBuffer = nil) or
         (fEOS and (StreamPos >= fEOSDelay))then
      begin
        Size := 0;
        Buffer := nil;
        Result := S_FALSE;
        Exit;
      end;

      if fIsDVD then StreamPos := fVisBufferStartTime + 29200000;
      Buffer := GetBufferAtTimeStamp(StreamPos,Size);
      if Buffer = nil then
      begin
        Size := 0;
        Buffer := nil;
        Result := S_FALSE;
        Exit;
      end;
      Result := S_OK;
//    finally
//      FcsReceive.UnLock;
//    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_VisualData'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_DSPPlugin(WindowHandle : hWnd; Path : PChar): HRESULT; stdcall;
{$IFDEF WITH_WINAMP}
var
  p : String;
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    p := Path;
    set_UnloadDSPPlugin;
    if not FileExists(p) then
    begin
      Result := S_FALSE;
      Exit;
    end;
    fDSPPlugin.Filename := p;
    fLoadedPlugin := p;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_DSPPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_DSPPlugin(out Path : PChar): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Path := PChar(fLoadedPlugin);
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_DSPPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_DSPCount(out Count : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Count := fDSPPlugin.PluginCount;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_DSPCount'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_DSPDescription(out Description : PChar): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Description := PChar(fDSPPlugin.PluginName);
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_DSPDescription'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_DSPSubDescription(index : integer; out Description : PChar): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Description := PChar(fDSPPlugin.SubPluginName[Index]);
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_DSPSubDescription'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_DSPSubPlugin(out index : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    index := fLoadedPluginIndex;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_DSPSubPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_DSPSubPlugin(index : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fDSPPlugin.Init(Index);
    fLoadedPluginIndex := index;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_DSPSubPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_ShowConfig: HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fDSPPlugin.Config;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_ShowConfig'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_UnloadDSPPlugin: HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fDSPPlugin.Quit;
    fLoadedPluginIndex := -1;
    fLoadedPlugin := '';
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_UnloadDSPPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableDSPPlug(out Enable : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Enable := fDSPPlugin.Enabled;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableDSPPlug'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_PluginOwnerWindow(Window : hWnd): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fDSPPlugin.OwnerWindow := Window;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_PluginOwnerWindow'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableDSPPlug(Enable : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fDSPPlugin.Enabled := Enable;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableDSPPlug'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_DisableSaving(Disable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_REGISTRY}
    fDisableSaving := Disable;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_DisableSaving'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_PCMDataBeforeMainDSP(Before : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fPCMDataBeforeMainDSP := Before;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_PCMDataBeforeMainDSP'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterVersion(out Version : integer) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Version := FilterVersion;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterVersion'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_WinampVisInterval(out Interval : integer) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Interval := fVisTimer.Interval;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_WinampVisInterval'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_WinampVisInterval(Interval : integer) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fVisTimer.Interval := Interval;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_WinampVisInterval'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_WinampVisPlugin(out Plugin : PChar; out Index : integer) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Plugin := PChar(fCurrentVisualPlugin);
    Index := fCurrentVisualPluginIndex;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_WinampVisPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_WinampVisPlugin(Plugin : PChar; Index : integer) : HRESULT; stdcall;
{$IFDEF WITH_WINAMP}
//var
//  c : Cardinal;
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    set_StopWinampVisPlugin;
    if not FileExists(StrPas(Plugin)) then
    begin
      fCurrentVisualPlugin := '';
      fCurrentVisualPluginIndex := -1;
      Result := S_FALSE;
      Exit;
    end;
//    c := GetTickCount;
//    sleep(100); // FIXME ???
    SetVisualPlugin(fThisInstance,StrPas(Plugin),Index);
    fCurrentVisualPlugin := StrPas(Plugin);
    fCurrentVisualPluginIndex := Index;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_WinampVisPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_StopWinampVisPlugin : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    StopVisualPlugin(fThisInstance);
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_StopWinampVisPlugin'); end; {$ENDIF}
end;
{*******************************************************************************}
{$IFDEF WITH_REGISTRY}
procedure TDCDSPFilter.LoadDefaults;
var
  Reg : TRegistry;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.KeyExists('SOFTWARE\DSP-worx\DC-DSP Filter') then Reg.DeleteKey('SOFTWARE\DSP-worx\DC-DSP Filter');
    Reg.Free;

  {$IFDEF WITH_WINAMP}
    fWinampVisAutoStart := False;
    fDSPPlugin.Enabled := False;
    fCurrentVisualPlugin := '';
    fCurrentVisualPluginIndex := -1;
    fLoadedPlugin := '';
    fLoadedPluginIndex := -1;
    fVisTimer.Interval := 40;
  {$ENDIF}
    fVISAfterDSP := False;
    fEnableStreamSwitching := False;
    fDelay := 0;
    fEnableDelay := False;
    fEnableLimitInstance := True;
    fEnableStreamSwitchingInterface := True;
  {$IFDEF WITH_BITRATE_CONVERSION}
    fEnableBRConv := False;
    fBRBeforeDSP := False;
    fBRConv.BitRate := br16BitInteger;
  {$ENDIF}
  {$IFDEF WITH_TRAYICON}
    fBalloonHint := False;
  {$ENDIF}
    fEnableForceStopFilter := True;
    fEnableForceDisconnect := True;
    FEnableRunningObjectTable := False;
    FEnableVisualBuffering := True;
    FVisualBufferEnabled := True;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.LoadDefaults'); end; {$ENDIF}
end;
{$ENDIF}
function TDCDSPFilter.get_WinampVisAutostart(out Autostart : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    Autostart := fWinampVisAutoStart;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_WinampVisAutostart'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_WinampVisAutostart(Autostart : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_WINAMP}
    fWinampVisAutoStart := Autostart;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_WinampVisAutostart'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_Instance(out Instance : integer) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Instance := fThisInstance;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_Instance'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableDelay(out Enabled : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Enabled := fEnableDelay;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableDelay'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableDelay(Enabled : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fEnableDelay := Enabled;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableDelay'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_Delay(out Delay : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Delay := fDelay;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_Delay'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_Delay(Delay : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fDelay := Delay;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_Delay'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterCount(out Count : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Count := fFilters.Count;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterCount'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterType(Index : integer; out FilterType : TDCFilterType): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    FilterType := fFilters.Items[Index].FilterType;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterType'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_AddFilter(Index : integer; FilterType : TDCFilterType): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    if Index > fFilters.Count then Index := fFilters.Count;
    if Index = -1 then fFilters.Add(FilterType)
                  else fFilters.Insert(Index,FilterType);
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_AddFilter'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterName(Index : integer; out Name : PChar): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    Name := PChar(fFilters.Name[Index]);
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterName'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_WindowShown(Index : integer; out Shown : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    Shown := fFilters.Items[Index].WindowShown;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_WindowShown'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_WindowShown(Index : integer; Shown : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    fFilters.Items[Index].WindowShown := Shown;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_WindowShown'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_DeleteFilter(Index : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    fFilters.Delete(Index);
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_DeleteFilter'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableFilter(Index : integer; out Enabled : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    Enabled := fFilters.Items[Index].Enabled;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableFilter'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableFilter(Index : integer; Enabled : BOOL): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    fFilters.Items[Index].Enabled := Enabled;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableFilter'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_RemoveAllFilters : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_OK;
    fFilters.Clear;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_RemoveAllFilters'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_MoveFilter(FromIndex : integer; ToIndex : integer): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (FromIndex < 0) or (FromIndex > fFilters.Count -1) then Exit;
    if (ToIndex < 0) or (ToIndex > fFilters.Count -1) then Exit;
    fFilters.Items[FromIndex].Index := ToIndex;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_MoveFilter'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_ResetShownWindows : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_OK;
    fFilters.ResetShownWindows;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_ResetShownWindows'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterClass(Index : integer; out Filter : TComponent): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    Filter := fFilters.Items[Index].Filter;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterClass'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterInterface(Index : integer; out Intf): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;

    if not Assigned(@Intf) then
    begin
      Result := E_POINTER;
      Exit;
    end;
    IUnknown(Intf) := fFilters.GetDSPInterface(Index);
    if Assigned(Pointer(Intf)) then Result := S_OK
                               else Result := E_NOTIMPL;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterInterface'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_FilterItem(Index : integer; out Item : TDCFilterItem): HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fFilters.Count -1) then Exit;
    Item := fFilters.Items[Index];
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_FilterItem'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_PresetCount(out Count : integer) : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Count := fPresets.Count;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_PresetCount'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_PresetExist(Name : PChar; out Exist : BOOL) : HRESULT;
{$IFDEF WITH_INTERNAL_DSP}
var
  i : integer;
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Exist := False;
    Result := S_FALSE;
    if fPresets.Count < 1 then Exit;
    for i := 0 to fPresets.Count -1 do
    begin
      if UpperCase(fPresets[i]) = UpperCase(Name) then
      begin
        Exist := True;
        break;
      end;
    end;
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_PresetExist'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_PresetName(Index : integer; out Name : PChar) : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_FALSE;
    if (Index < 0) or (Index > fPresets.Count -1) then Exit;
    Name := PChar(fPresets[Index]);
    Result := S_OK;
  {$ELSE}
    Result := E_NOTIMPL;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_PresetName'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_LoadPreset(Name : PChar) : HRESULT;
{$IFDEF WITH_REGISTRY}
{$IFDEF WITH_INTERNAL_DSP}
var
  Buf : PChar;
  Size : integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_REGISTRY}
  {$IFDEF WITH_INTERNAL_DSP}
    Result := S_OK;
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly('SOFTWARE\DSP-worx\DC-DSP Filter\DSP Presets') then
      begin
        if ValueExists(Name) then
        begin
          Size := GetDataSize(Name);
          Buf := AllocMem(Size);
          ReadBinaryData(Name,Buf^,Size);
          fFilters.ImportSettings(Buf);
          FreeMem(Buf);
        end;
      end;
      Free;
    end;
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\DSP-Worx\DC-DSP Filter',False) then
      begin
        WriteString('fLastPPPreset',Name);
        CloseKey;
      end;
    end;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_LoadPreset'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_SavePreset(Name : PChar) : HRESULT;
{$IFDEF WITH_REGISTRY}
{$IFDEF WITH_INTERNAL_DSP}
var
  Buf : PChar;
  Size : integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_REGISTRY}
  {$IFDEF WITH_INTERNAL_DSP}
    if fFilters.ExportSettings(Buf,Size) then
    begin
      with TRegistry.Create do
      begin
        Rootkey := HKEY_CURRENT_USER;
        if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\DSP Presets',True) then
          WriteBinaryData(Name,Buf^,Size);
        Free;
      end;
    end;
    UpdatePresetsList;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_SavePreset'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_DeletePreset(Name : PChar) : HRESULT;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_REGISTRY}
  {$IFDEF WITH_INTERNAL_DSP}
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\DSP Presets',False) then
        if ValueExists(Name) then DeleteValue(Name);
      Free;
    end;
    UpdatePresetsList;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_DeletePreset'); end; {$ENDIF}
end;
{*******************************************************************************}
{$IFDEF WITH_REGISTRY}
{$IFDEF WITH_INTERNAL_DSP}
procedure TDCDSPFilter.UpdatePresetsList;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fPresets.Clear;
    with TRegistry.Create do
    begin
      Rootkey := HKEY_CURRENT_USER;
      if OpenKey('SOFTWARE\DSP-worx\DC-DSP Filter\DSP Presets',False) then
        GetValueNames(fPresets);
      Free;
    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.UpdatePresetsList'); end; {$ENDIF}
end;
{$ENDIF}
{$ENDIF}
{*******************************************************************************}
function TDCDSPFilter.get_CPUUsage(out Usage : Double) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_CPU_METER}
    Usage := fCPUMeter.usage;
    Result := S_OK;
  {$ELSE}
    Usage := 0;
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_CPUUsage'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.CheckDVD : Boolean;
var
  EnumFilters : IEnumFilters;
  Filter : IBaseFilter;
  ClassID : TGUID;
const
  CLSID_InterVideoDVDNavigator : TGUID = '{AABC1235-776D-11D2-8010-00104B9B8592}';
  CLSID_CyberlinkDVDNavigator  : TGUID = '{10D5F9E1-0360-11D5-8F2A-0080C84E9C39}';
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := False;
    if Graph = nil then Exit;
    if Graph.EnumFilters(EnumFilters) <> S_OK then Exit;
    while EnumFilters.Next(1,Filter,nil) = S_OK do
    begin
      Filter.GetClassID(ClassID);
      if IsEqualGUID(ClassID,CLSID_DVDNavigator)
      or IsEqualGUID(ClassID,CLSID_InterVideoDVDNavigator)
      or IsEqualGUID(ClassID,CLSID_CyberlinkDVDNavigator) then
      begin
        Result := True;
        break;
      end;
    end;
    EnumFilters := nil;
    Filter := nil;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.CheckDVD'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.Count(out pcStreams: DWORD): HResult; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    pcStreams := GetConnectedInputs;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Count'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.Info(lIndex: Longint; out ppmt: PAMMediaType; out pdwFlags: DWORD; out plcid: LCID; out pdwGroup: DWORD; out ppszName: PWCHAR; out ppObject: IUnknown; out ppUnk : IUnknown): HResult; stdcall;
var
  pin: TDCDSPFilterInputPin;
  lang: PLangItem;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    if (lIndex < 0) or (lIndex >= fPinList.Count) or not fEnableStreamSwitching then
    begin
      Result := S_FALSE;
      Exit;
    end;

    pin := GetPinByIndex(lIndex);
    lang := GetLangByIndex(lIndex);

    if Assigned(@ppmt) then
    begin
      ppmt := nil;
      if pin.IsConnected
        then ppmt := CreateMediaType(pin.CurrentMediaType.MediaType);
    end;

    if Assigned(@pdwGroup)
      then pdwGroup := 1;

    if Assigned(@plcid) then
    begin
      if Assigned(lang) then plcid := lang.aLCID
                        else plcid := 0;
    end;

    if Assigned(@pdwFlags) then
    begin
      if not pin.Blocked then pdwFlags := AMSTREAMSELECTINFO_ENABLED or AMSTREAMSELECTINFO_EXCLUSIVE
                         else pdwFlags := 0;
    end;

    if Assigned(@ppszName) then
    begin
      if Assigned(lang) then
        AMGetWideString(lang.aName, ppszName)
      else
        AMGetWideString(PWChar(WideString('Audio Track ' + inttostr(lIndex + 1))), ppszName);
    end;

    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Info'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.Enable(lIndex: Longint; dwFlags: DWORD): HResult; stdcall;
const
  CLSID_AC3Filter: TGUID = '{A753A1EC-973E-4718-AF8E-A3F554D45C44}';
var
  pin: TDCDSPFilterInputPin;
  i : integer;
  State : TFilterState;
  iMS : IMediaSeeking;
  Pos : Int64;
  oPin: IPin;
  ooPin: IPin;
  iiPin: IPin;
  PinInfo: TPinInfo;
  clsid: TGuid;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
//  FcsFilter.Lock;
  FSwitchLock.Lock;
  try
    Result := S_FALSE;

    if (dwFlags = 0) or BOOL(dwFlags and AMSTREAMSELECTENABLE_ENABLEALL) then
    begin
      Result := E_NOTIMPL;
      Exit;
    end;

    pin := GetPinByIndex(lIndex);

    if not fEnableStreamSwitching or  not Assigned(pin) or not pin.IsConnected or
       not pin.Blocked then Exit;

    State := FState;
    Graph.QueryInterface(IID_IMediaSeeking,iMS);
    if Assigned(iMS) then iMS.GetCurrentPosition(Pos);

    case FState of
      State_Paused,
      State_Running: (Graph as IMediaControl).Stop;
    end;

    for i := 0 to fPinList.Count -1 do
      TDCDSPFilterInputPin(fPinList.Items[i]).Blocked := True;

    pin := TDCDSPFilterInputPin(fPinList.Items[lIndex]);
    pin.Blocked := False;
    fActivePin := pin;
    fNeedMediatypeChange := True;

    if fEnableForceDisconnect then
    begin
      oPin := FOutput.GetConnected;

      oPin.QueryPinInfo(PinInfo);
      PinInfo.pFilter.GetClassID(clsid);
      if IsEqualGUID(clsid, CLSID_AC3Filter) then
      begin
        ooPin := GetOutPin(PinInfo.pFilter, 0);
        ooPin.ConnectedTo(iiPin);
        if Assigned(ooPin) and Assigned(iiPin) then
        begin
          ooPin.Disconnect;
          iiPin.Disconnect;
        end;
      end;

      if Assigned(oPin) then
      begin
        FOutput.Disconnect;
        oPin.Disconnect;

        FGraph.Reconnect(pin);

        FGraph.ConnectDirect(FOutput, oPin, nil);
        if Assigned(ooPin) and Assigned(iiPin)
          then ooPin.Connect(iiPin, nil)
      end;

      ooPin := nil;
      iiPin := nil;

      oPin := nil;
    end;

    if Assigned(iMS) then
    begin
      iMS.SetPositions(Pos,AM_SEEKING_AbsolutePositioning,Pos,AM_SEEKING_NoPositioning);
      iMS := nil;
    end;

    case State of
      State_Paused:  (Graph as IMediaControl).Pause;
      State_Running: (Graph as IMediaControl).Run;
    end;

    Result := S_OK;
  finally
    FSwitchLock.UnLock;
//    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.Enable'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableStreamSwitching(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Enable := fEnableStreamSwitching;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableStreamSwitching'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableStreamSwitching(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fEnableStreamSwitching := Enable;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableStreamSwitching'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableStreamSwitchingInterface(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Enable := fEnableStreamSwitchingInterface;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableStreamSwitchingInterface'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableStreamLimitInstance(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Enable := fEnableLimitInstance;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableStreamLimitInstance'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableStreamLimitInstance(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fEnableLimitInstance := Enable;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableStreamLimitInstance'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableStreamSwitchingInterface(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    fEnableStreamSwitchingInterface := Enable;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableStreamSwitchingInterface'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.EndOfStream: HRESULT;
var
  s : TReferenceTime;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    StreamTime(s);
    fEOSDelay := s + (fVisBufferStartTime + (fVisBufferDelay * 2));
    fEOS := True;
    Result := inherited EndOfStream;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.EndOfStream'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableBitrateConversion(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    fEnableBRConv := Enable;
    fForceMediatypeChange := True;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableBitrateConversion'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableBitrateConversion(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    Enable := fEnableBRConv;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableBitrateConversion'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_EnableBitrateConversionBeforeDSP(Before : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    fBRBeforeDSP := Before;
    fRealStream := GetConvertedStream;
    if fEnableBRConv and (AllocateVisBuffer = S_OK) then
    begin
  {$IFDEF WITH_INTERNAL_DSP}
      fFilters.Init(@fRealStream);
      fFilters.Flush;
  {$ENDIF}
      fVisBufferCount := 0;
      fVisBufferStartTime := -1;
      fVisBufferRollOver := 0;
      if Assigned(fCallback) then fCallback.Flush;
    end;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableBitrateConversionBeforeDSP'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_EnableBitrateConversionBeforeDSP(out Before : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    Before := fBRBeforeDSP;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableBitrateConversionBeforeDSP'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.set_BitrateConversionBits(Bits : TDCBitRate) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    fBRConv.BitRate := Bits;
    if fEnableBRConv then fForceMediatypeChange := True;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_BitrateConversionBits'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.get_BitrateConversionBits(out Bits : TDCBitRate) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    Bits := fBRConv.BitRate;
    Result := S_OK;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_BitrateConversionBits'); end; {$ENDIF}
end;
{*******************************************************************************}
function TDCDSPFilter.GetConvertedStream: TDSStream;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_BITRATE_CONVERSION}
    if fEnableBRConv and fBRBeforeDSP and not fStream.DTS and not fStream.SPDIF then
    begin
      Result := fStream;
      case fBRConv.BitRate of
        br8BitInteger:
        begin
          Result.Bits := 8;
          Result.Float := False;
        end;
        br16BitInteger:
        begin
          Result.Bits := 16;
          Result.Float := False;
        end;
        br24BitInteger:
        begin
          Result.Bits := 24;
          Result.Float := False;
        end;
        br32BitInteger:
        begin
          Result.Bits := 32;
          Result.Float := False;
        end;
        br32BitFloat:
        begin
          Result.Bits := 32;
          Result.Float := True;
        end;
      end;
    end else
    begin
      Result := fStream
    end;
  {$ELSE}
    Result := fStream;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.GetConvertedStream'); end; {$ENDIF}
end;

function TDCDSPFilter.get_ForceStreamSwitchingDisconnect(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := S_OK;
    Enable := fEnableForceDisconnect;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_ForceStreamSwitchingDisconnect'); end; {$ENDIF}
end;

function TDCDSPFilter.set_ForceStreamSwitchingDisconnect(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := S_OK;
    fEnableForceDisconnect := Enable;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_ForceStreamSwitchingDisconnect'); end; {$ENDIF}
end;

function TDCDSPFilter.get_ForceStreamSwitchingStopFilter(out Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := S_OK;
    Enable := fEnableForceStopFilter;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_ForceStreamSwitchingStopFilter'); end; {$ENDIF}
end;

function TDCDSPFilter.set_ForceStreamSwitchingStopFilter(Enable : BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := S_OK;
    fEnableForceStopFilter := Enable;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_ForceStreamSwitchingStopFilter'); end; {$ENDIF}
end;

function TDCDSPFilter.get_EnablePropertyPage(out Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_PROPERTYPAGES}
    Result := S_OK;
    Enable := fEnablePropertyPage;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnablePropertyPage'); end; {$ENDIF}
end;

function TDCDSPFilter.set_EnablePropertyPage(Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_PROPERTYPAGES}
    Result := S_OK;
    fEnablePropertyPage := Enable;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnablePropertyPage'); end; {$ENDIF}
end;

function TDCDSPFilter.get_TrayiconVisible(out Visible: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_TRAYICON}
    Result := S_OK;
    Visible := fTrayIcon.Visible;
  {$ELSE}
    Visible := False;
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_TrayiconVisible'); end; {$ENDIF}
end;

function TDCDSPFilter.set_TrayiconVisible(Visible: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_TRAYICON}
    Result := S_OK;
    fTrayIcon.Visible := Visible;
    if Assigned(fTrayIcon.TrayIcon) then
      fTrayIcon.TrayIcon.Visible := Visible;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_TrayiconVisible'); end; {$ENDIF}
end;

function TDCDSPFilter.get_EnableROT(out Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Enable := FEnableRunningObjectTable;
    Result := S_OK;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableROT'); end; {$ENDIF}
end;

function TDCDSPFilter.set_EnableROT(Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
    Result := S_OK;

    if (Enable = FEnableRunningObjectTable)
      then Exit;

    FEnableRunningObjectTable := Enable;

    if (FEnableRunningObjectTable) then
    begin
      if (FRunningObjectTable = 0) and Assigned(Graph) then
      begin
        AddGraphToRot(Graph, FRunningObjectTable)
      end;
    end else
    begin
      if (FRunningObjectTable <> 0) then
      begin
        RemoveGraphFromRot(FRunningObjectTable);
        FRunningObjectTable := 0;
      end;
    end;
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableROT'); end; {$ENDIF}
end;

function TDCDSPFilter.get_EnableBalloonHint(out Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_TRAYICON}
    Result := S_OK;
    Enable := fBalloonHint;
  {$ELSE}
    Enable := False;
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.get_EnableBalloonHint'); end; {$ENDIF}
end;

function TDCDSPFilter.set_EnableBalloonHint(Enable: BOOL) : HRESULT; stdcall;
begin
{$IFDEF EXCEPT_DEBUG}try{$ENDIF}
  FcsFilter.Lock;
  try
  {$IFDEF WITH_TRAYICON}
    Result := S_OK;
    fBalloonHint := Enable;
  {$ELSE}
    Result := S_FALSE;
  {$ENDIF}
  finally
    FcsFilter.UnLock;
  end;
{$IFDEF EXCEPT_DEBUG} except er('TDCDSPFilter.set_EnableBalloonHint'); end; {$ENDIF}
end;

function TDCDSPFilter.get_EnableVisualBuffering(out Enable: BOOL) : HRESULT;
begin
  Result := S_OK;
  Enable := FEnableVisualBuffering;
end;

function TDCDSPFilter.set_EnableVisualBuffering(Enable: BOOL) : HRESULT;
begin
  Result := S_OK;
  FEnableVisualBuffering := Enable;
end;

function TDCDSPFilter.CheckDownStream: Boolean;

  function CheckDown(APin: IPin): Boolean;
  var
    pin: IPin;
    pin_info: TPinInfo;
    classId: TGuid;
    enum: IEnumPins;
    dir: TPinDirection;
  begin
    Result := False;
    if APin.ConnectedTo(pin) = S_OK then
    begin
      pin.QueryPinInfo(pin_info);
      pin_info.pFilter.GetClassID(classId);
      if IsEqualGUID(classId, CLSID_DCDSPFilter) then
      begin
        Result := True;
        Exit;
      end;

      pin_info.pFilter.EnumPins(enum);
      while enum.Next(1, pin, nil) = S_OK do
      begin
        pin.QueryDirection(dir);
        if (dir = PINDIR_INPUT) and CheckDown(pin) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

var
  i: Integer;
begin
  Result := False;

  for i := 0 to fPinList.Count -1 do
  begin
    if TDCDSPFilterInputPin(fPinList[i]).IsConnected then
    begin
      if CheckDown(TDCDSPFilterInputPin(fPinList[i]) as IPin) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{$IFDEF WITH_REGISTER}
initialization
  TBCClassFactory.CreateFilter(TDCDSPFilter, StringToOleStr('DC-DSP Filter'), CLSID_DCDSPFilter,
    CLSID_LegacyAmFilterCategory, MERIT_PREFERRED + 3, 2, @SudPins);
{$ENDIF}
{$IFNDEF WITH_REGISTER}
initialization
{$ENDIF}
{$IFDEF EXCEPT_DEBUG}
  DebFile := CreateFile(PChar('C:\dcdspdbg.log'), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 	0);
  if (DebFile <> 0) and (Debfile <> $FFFFFFFF) then
  begin
    SetFilePointer(DebFile, 0, nil, FILE_END);
  end;

finalization
  if (DebFile <> 0) and (Debfile <> $FFFFFFFF) then
  begin
    CloseHandle(DebFile);
    DebFile := 0;
  end;
{$ENDIF}
end.
