
    (*********************************************************************
     *  DSPackTDCDSPFilter.pas                                           *
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
{
  @abstract(DirectShow Audiofilter for DSPack.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Jul 07, 2003)
}

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit DSPackTDCDSPFilter;

interface

uses
  Windows, Classes, ExtCtrls, DirectShow9, DSPack, ActiveX, Registry, SyncObjs,
  SysUtils, Math, DSUtil, BaseClass, MMSystem, dmoConst, dspConst, dspUtils;

const
  {@exclude}
  CLSID_DSPackDCDSPFilter : TGUID = '{088C4FF0-A38B-42A7-9116-4EABFA875AB2}';
  {@exclude}
  IID_DSPackDCDSPFilter   : TGUID = '{BECE3169-4149-47E4-8AE8-545C3CC64428}';

type
  {@exclude}
  IDSPackDCDSPFilterCallBack = interface(IUnknown)
    ['{3971C5D4-1FDA-45C1-9131-C817326A4348}']
    function PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT; stdcall;
    function MediaTypeChanged(Stream : PDSStream) : HRESULT; stdcall;
    function Flush : HRESULT; stdcall;
  end;

  {@exclude}
  IDSPackDCDSPFilterVisualInterface = interface(IUnknown)
  ['{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}']
    function get_VISafterDSP(out AfterDSP : Boolean): HRESULT; stdcall;
    function set_VISafterDSP(AfterDSP : Boolean): HRESULT; stdcall;
    function get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream) : HRESULT; stdcall;
  end;

  {@exclude}
  IDSPackDCDSPFilterInterface = interface(IUnknown)
    ['{BECE3169-4149-47E4-8AE8-545C3CC64428}']
    function set_CallBackPCM(Callback : IDSPackDCDSPFilterCallBack): HRESULT; stdcall;
    function get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
    // Delay functions for delaying the Audio Stream through Timestamps
    function get_EnableDelay(out Enabled : Boolean): HRESULT; stdcall;
    function set_EnableDelay(Enabled : Boolean): HRESULT; stdcall;
    function get_Delay(out Delay : integer): HRESULT; stdcall;
    function set_Delay(Delay : integer): HRESULT; stdcall;
  end;

  {@exclude}
  TDSAudioFilter = class(TBCTransInPlaceFilter,IDSPackDCDSPFilterInterface,
                         IPersist,IMediaFilter,IDSPackDCDSPFilterVisualInterface)
  private
    fEOS : Boolean;
    fEOSDelay : Int64;
    fEnableDelay : Boolean;
    fDelay : integer;
    fLastType : TDSStream;
    fLastMediaType : TDSStream;
    fFlushing : Boolean;
    fPlaying : Boolean;
    fLocked : Boolean;
    fCanRollOver : Boolean;
    fVisBufferSwap : integer;
    fVisBufferRollOver : integer;
    fVisBufferRollOverPos : integer;
    fStream : TDSStream;
    fMediaLocked : Boolean;
    fVISafterDSP : Boolean;
    fCallback : IDSPackDCDSPFilterCallBack;
    fVisBuffer : PChar;
    fVisBufferCount : integer;
    fVisBufferDelay,
    fVisBufferStartTime : TReferenceTime;
    fVisBufferMaxBytes : integer;
    fVisBufferWrap : integer;
    fVisBufferDivider : Double;
    fVisModBytes : integer;
    fFirstVisRun : Boolean;
    fIsDVD : Boolean;
    function ApplyVisual(Buffer : Pointer; Length : integer) : HRESULT; stdcall;
    function AllocateVisBuffer : HRESULT; stdcall;
    function GetBufferAtTimeStamp(Time : REFERENCE_TIME; out Bytes : integer) : PChar; stdcall;
    function CheckDVD : Boolean;
  public
    function CompleteConnect(direction: TPinDirection; ReceivePin: IPin): HRESULT; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    constructor Create(ObjName: string; unk: IUnKnown; out hr: HRESULT);
    constructor CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown); override;
    destructor Destroy; override;
    function Transform(msOut: IMediaSample): HRESULT; override;
    function GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT; override;
    function CheckTransform(mtIn, mtOut: PAMMediaType): HRESULT; override;
    function BeginFlush: HRESULT; override;
    function EndFlush: HRESULT; override;
    function DecideBufferSize(Alloc: IMemAllocator; propInputRequest: PALLOCATORPROPERTIES): HRESULT; override;
    function Stop: HRESULT; override;
    function Pause: HRESULT; override;
    function Run(tStart: TReferenceTime): HRESULT; override;
    function EndOfStream: HRESULT; override;
    function get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
    function set_CallbackPCM(Callback : IDSPackDCDSPFilterCallBack): HRESULT; stdcall;
    function get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream): HRESULT; stdcall;
    function get_VISafterDSP(out AfterDSP : Boolean): HRESULT; stdcall;
    function set_VISafterDSP(AfterDSP : Boolean): HRESULT; stdcall;
    function get_EnableDelay(out Enabled : Boolean): HRESULT; stdcall;
    function set_EnableDelay(Enabled : Boolean): HRESULT; stdcall;
    function get_Delay(out Delay : integer): HRESULT; stdcall;
    function set_Delay(Delay : integer): HRESULT; stdcall;
  end;

  {@exclude}
  TPCMNotifyEvent = procedure(Sender : TObject; Buffer : Pointer; Size : integer; out NewSize : integer; Stream : PDSStream) of object;
  {@exclude}
  TVisualNotifyEvent = procedure(Sender : TObject; Buffer : Pointer; Size : integer; Stream : PDSStream) of object;
  {@exclude}
  TMediaTypeNotifyEvent = procedure(Sender : TObject; Stream : PDSStream) of object;

  { The Core of the DSPack Filter. No external Filter is needed to get the Filter
    into a Filtergraph. Thus no external File is needed. }
  TDSPackDCDSPFilter = class(TComponent, IFilter, IDSPackDCDSPFilterCallBack)
  private
    {@exclude}
    fFilter : TDSAudioFilter;
    {@exclude}
    fVisualEnabled : Boolean;
    {@exclude}
    fVisualAfterDSP : Boolean;
    {@exclude}
    fVisualInterval : Word;
    {@exclude}
    fVisualTimer : TTimer;
    {@exclude}
    fDSPEnabled : Boolean;
    {@exclude}
    DSPackAudioFilter : IDSPackDCDSPFilterInterface;
    {@exclude}
    DSPackVisualIntf : IDSPackDCDSPFilterVisualInterface;
    {@exclude}
    fFilterGraph : TFilterGraph;
    {@exclude}
    fBaseFilter : IBaseFilter;
    {@exclude}
    fCriticalSection : TCriticalSection;
    {@exclude}
    fOnVisualData : TVisualNotifyEvent;
    {@exclude}
    fOnPCMData  : TPCMNotifyEvent;
    {@exclude}
    fOnMediaTypeChanged : TMediaTypeNotifyEvent;
    {@exclude}
    fOnFlush : TNotifyEvent;
    {@exclude}
    function GetFilter: IBaseFilter;
    {@exclude}
    function GetName: string;
    {@exclude}
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    {@exclude}
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    {@exclude}
    procedure RefreshSettings;
    {@exclude}
    function PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT; stdcall;
    {@exclude}
    function MediaTypeChanged(Stream : PDSStream) : HRESULT; stdcall;
    {@exclude}
    function Flush : HRESULT; stdcall;
    {@exclude}
    procedure SetVisualEnabled (Enabled : Boolean);
    {@exclude}
    procedure SetVisualAfterDSP (After : Boolean);
    {@exclude}
    procedure SetVisualInterval (Interval : Word);
    {@exclude}
    procedure OnTimer(Sender : TObject);
    {@exclude}
    function GetStreaminfo : TDSStream;
    {@exclude}
    function GetDelay : integer;
    {@exclude}
    procedure SetDelay(Delay : integer);
    {@exclude}
    function GetEnableDelay : Boolean;
    {@exclude}
    procedure SetEnableDelay(Enabled : Boolean);
  public
    { TDSPackDCDSPFilter Constructor. }
    constructor Create(AOwner: TComponent); override;
    { TDSPackDCDSPFilter Destructor. }
    destructor Destroy; override;
    {@exclude}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  published
    { Enabled or Disables the Visual Timer that raises the OnVisualData Event. }
    property VisualEnabled : Boolean read fVisualEnabled write SetVisualEnabled;
    { Specifys the Interval of the Visual Timer. }
    property VisualInterval  : Word read fVisualInterval write SetVisualInterval;
    { Specifys whether the Visual should be rendered before any DSP is done.
      Apprecated is the False Value, because it´s getting better Amplification
      Value over the whole Frequency Bands. }
    property VisualAfterDSP : Boolean read fVisualAfterDSP write SetVisualAfterDSP;
    { Enables or Disables the PCM Callback. }
    property DSPEnabled : Boolean read fDSPEnabled write fDSPEnabled;
    { Enables/Disables the ability to Delay the Audio Stream. }
    property DelayEnabled : Boolean read GetEnableDelay write SetEnableDelay;
    { Get/Set the Delay of the Audio Stream. }
    property Delay : integer read GetDelay write SetDelay;
    { Informs about the current Audiobuffer Type. }
    property StreamInfo : TDSStream read GetStreamInfo;
    {@exclude}
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
    { This is called when new Visual Data is ready. }
    property OnVisualData : TVisualNotifyEvent read fOnVisualData write fOnVisualData;
    { This is called during every Transform Method of the Filter. }
    property OnPCMData : TPCMNotifyEvent read fOnPCMData write fOnPCMData;
    { Notifys when the MediaType has changed. }
    property OnMediaTypeChanged : TMediaTypeNotifyEvent read fOnMediaTypeChanged write fOnMediaTypeChanged;
    { Notifys when the Filter has been Flushed. Usefull to Flush the used DSP Filters. }
    property OnFlush : TNotifyEvent read fOnFlush write fOnFlush;
  end;

  {@exclude}
  TMyFilterGraph = class(TFilterGraph)
  public
    procedure InsertFilter(AFilter: IFilter);
    procedure RemoveFilter(AFilter: IFilter);
  end;

  {@exclude}
  procedure Register;

implementation
{**** DSPack Audio Filter ******************************************************}
function TDSAudioFilter.CompleteConnect(direction: TPinDirection; ReceivePin: IPin): HRESULT;
begin
  Result := inherited CompleteConnect(direction,ReceivePin);
  if (direction = PINDIR_OUTPUT) and FInput.IsConnected and FOutput.IsConnected then
  begin
    fIsDVD := CheckDVD;
    fFirstVisRun := True;
    AllocateVisBuffer;
  end;
end;

function TDSAudioFilter.Stop: HRESULT;
begin
  fPlaying := False;
  fVisBufferCount := 0;
  Result := inherited Stop;
end;

function TDSAudioFilter.Pause: HRESULT;
begin
  fPlaying := False;
  Result := inherited Pause;
end;

function TDSAudioFilter.Run(tStart: TReferenceTime): HRESULT;
begin
  fPlaying := True;
  fEOS := False;
  Result := inherited Run(tStart);
end;

function TDSAudioFilter.CheckTransform(mtIn,mtOut: PAMMediaType): HRESULT;
begin
  if not IsEqualGUID (mtIn^.majortype ,MEDIATYPE_Audio) then
  begin
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;

  if (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_PCM)) and
     (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT)) and
     (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_NULL)) then // NULL because of SoundForge
  begin
    Result := VFW_E_INVALIDSUBTYPE;
    Exit;
  end;

  if not IsEqualGUID (mtIn^.formattype,FORMAT_WaveFormatEx) then
  begin
    Result :=  VFW_E_TYPE_NOT_ACCEPTED;
    Exit;
  end;

  Result := S_OK;
end;

function TDSAudioFilter.GetMediaType(Position: integer; out MediaType: PAMMediaType): HRESULT;
begin
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

  CopyMediaType(MediaType,Input.CurrentMediaType.MediaType);
  Result := S_OK;
end;

function TDSAudioFilter.DecideBufferSize(Alloc: IMemAllocator; propInputRequest: PALLOCATORPROPERTIES): HRESULT;
var
  hr : HRESULT;
  Actual : TAllocatorProperties;
  max_buffer_size : integer;
begin
  if not Input.IsConnected then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  ASSERT(Alloc <> nil);
  ASSERT(propInputRequest <> nil);
  max_buffer_size := 352800 * fStream.Channels * (fStream.Bits div 8);
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
end;

function TDSAudioFilter.CheckInputType(mtIn: PAMMediaType): HRESULT;
begin
  if not IsEqualGUID (mtIn^.majortype ,MEDIATYPE_Audio) then
  begin
    Result := VFW_E_INVALIDMEDIATYPE;
    Exit;
  end;

  if (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_PCM)) and
     (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT)) and
     (not IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_NULL)) then // NULL because of SoundForge
  begin
    Result := VFW_E_INVALIDSUBTYPE;
    Exit;
  end;

  if not IsEqualGUID (mtIn^.formattype,FORMAT_WaveFormatEx) then
  begin
    Result :=  VFW_E_TYPE_NOT_ACCEPTED;
    Exit;
  end;

  fStream.Frequency := PWaveFormatEx(mtIn^.pbFormat)^.nSamplesPerSec;
  fStream.Channels := PWaveFormatEx(mtIn^.pbFormat)^.nChannels;
  fStream.Bits := PWaveFormatEx(mtIn^.pbFormat)^.wBitsPerSample;

  // Last Check to see if we´re dealing with 8,16,24 or 32 Bit Audio.
  // If not then drop the Connection
  if (fStream.Bits <> 32) and (fStream.Bits <> 24) and (fStream.Bits <> 16) and (fStream.Bits <> 8) then
  begin
    Result := VFW_E_TYPE_NOT_ACCEPTED;
    Exit;
  end;

  fStream.Float := IsEqualGUID (mtIn^.subtype ,MEDIASUBTYPE_IEEE_FLOAT) or (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_IEEE_FLOAT) or
     ((PWaveFormatExtensible(mtIn^.pbFormat)^.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and  IsEqualGUID(PWaveFormatExtensible(mtIn^.pbFormat)^.SubFormat,KSDATAFORMAT_SUBTYPE_IEEE_FLOAT));

  // allow SPDIF or DTS types to connect, but no DSP or VisualBuffering is done.
  // Needed for dynamic MedayType change with AC3 Filter.
  fStream.SPDIF := (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DOLBY_AC3_SPDIF) or
                   (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DOLBY_AC3);
  fStream.DTS :=   (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DTS) or
                   (PWaveFormatEx(mtIn^.pbFormat)^.wFormatTag = WAVE_FORMAT_DVD_DTS);

  if Assigned(fCallback) and not CompareMem(@fLastMediaType,@fStream,SizeOf(TDSStream))
    then fCallback.MediaTypeChanged(@fStream);
  fLastMediaType := fStream;
  
  Result := S_OK;
end;

destructor TDSAudioFilter.Destroy;
begin
  if fVisBuffer <> nil then
  begin
    FreeMemory(fVisBuffer);
    fVisBuffer := nil;
  end;
  inherited Destroy;
end;

constructor TDSAudioFilter.Create(ObjName: string; unk: IUnknown; out hr: HRESULT);
begin
  inherited Create(ObjName, unk, CLSID_DSPackDCDSPFilter,hr);
  fVisBufferStartTime := -1;
  fFirstVisRun := True;
  fVisBufferRollOver := 0;
  fFlushing := False;
  fVisBufferCount := 0;
  fVisBufferStartTime := 0;
  fDelay := 0;
  fEnableDelay := False;
end;

constructor TDSAudioFilter.CreateFromFactory(Factory: TBCClassFactory; const Controller: IUnknown);
var
  hr: HRESULT;
begin
  Create(Factory.Name, Controller, hr);
end;

function TDSAudioFilter.AllocateVisBuffer : HRESULT;
begin
  Result := S_FALSE;

  fVisBufferCount := 0;
  fVisBufferStartTime := 0;

  if CompareMem(@fLastType,@fStream,SizeOf(TDSStream)) then Exit;
  fLastType := fStream;

  // fVisBufferDivider is needed to convert bytes <> SampleTime
  fVisBufferDivider := 10000000 / fStream.Frequency / fStream.Channels / (fStream.Bits div 8);
  if fVisBuffer <> nil then
  begin
    FreeMemory(fVisBuffer);
    fVisBuffer := nil;
  end;
  // Allocate Memory for Visual Buffering (4 seconds for every Channel) with some extrabytes for swapping
  fVisBufferMaxBytes := fStream.Frequency * fStream.Channels * (fStream.Bits div 8) * 4;
  fVisBufferSwap := 4 * fStream.Channels * (fStream.Bits div 8) * 16384;
  fVisBuffer := AllocMem(fVisBufferMaxBytes + fVisBufferSwap * 2);
  fVisModBytes := fStream.Channels * (fStream.Bits div 8);
  Result := S_OK;
end;

function TDSAudioFilter.ApplyVisual(Buffer : Pointer; Length : integer) : HRESULT;
begin
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
end;

function TDSAudioFilter.GetBufferAtTimeStamp(Time : REFERENCE_TIME; out Bytes : integer) : PChar;

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
    Bytes := fStream.Channels * (fStream.Bits div 8) * 16384;
    Result := @fVisBuffer[tmp];
  end;
end;

function TDSAudioFilter.Transform(msOut: IMediaSample): HRESULT;
var
  pSrc : PByte;
  Length : integer;
  NewSize : integer;
  mt : PAMMediaType;
  hr : HRESULT;
  OldLength : integer;
  mi,mo : TReferenceTime;
  mmi,mmo : Int64;
begin

  msOut.GetPointer(pSrc);
  Length := msOut.GetActualDataLength;
  OldLength := Length;

  // Dynamic MediaType change (eg. AC3 Filter)
  if msOut.GetMediaType(mt) = S_OK then
  begin
    fMediaLocked := True;
    hr := CheckInputType(mt);
    if hr <> S_OK then
    begin
      Result := hr;
      DeleteMediaType(mt);
      fMediaLocked := False;
      Exit;
    end;
    AllocateVisBuffer;
    DeleteMediaType(mt);
    fMediaLocked := False;
  end;

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

  if not fVISAfterDSP then
  begin
    fLocked := True;
    ApplyVisual(pSrc,Length);
    fLocked := False;
  end;

  fStream.Size := Length;
  if not fStream.SPDIF and not fStream.DTS and Assigned(fCallback) then
  begin
    fCallback.PCMDataCB(pSrc,Length,NewSize,@fStream);
    if NewSize <> Length then
    begin
      Length := NewSize;
      msOut.SetActualDataLength(Length);
    end;
  end;

  // Set new TimeStamps if Buffersize has changed.
  // Seems to be an useless try to keep Audio and Video Data synchronized...
  if (OldLength <> Length) then
  begin
    if(msOut.GetMediaTime(mmi,mmo) = S_OK) then
    begin
      mmo := mmi + (Int64(Length) * 10000000 div fStream.Channels div (fStream.Bits div 8)div fStream.Frequency);
      msOut.SetMediaTime(@mmi,@mmo);
    end;
    if msOut.GetTime(mi,mo) = S_OK then
    begin
      mo := mi + (Int64(Length) * 10000000 div fStream.Channels div (fStream.Bits div 8)div fStream.Frequency);
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
  Result := S_OK;
end;

function TDSAudioFilter.BeginFlush : HRESULT;
begin
  fFlushing := True;
  fVisBufferCount := 0;
  fVisBufferStartTime := -1;
  fVisBufferRollOver := 0;
  if Assigned(fCallback) then fCallback.Flush;
  Result := inherited BeginFlush;
end;

function TDSAudioFilter.EndFlush : HRESULT;
begin
  fFlushing := False;
  Result := inherited EndFlush;
end;

function TDSAudioFilter.get_VISafterDSP(out AfterDSP : Boolean): HRESULT; stdcall;
begin
  AfterDSP := fVISafterDSP;
  Result := S_OK;
end;

function TDSAudioFilter.set_VISafterDSP(AfterDSP : Boolean): HRESULT; stdcall;
begin
  fVISafterDSP := AfterDSP;
  Result := S_OK;
end;

function TDSAudioFilter.get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
begin
  Stream := @fStream;
  Result := S_OK;
end;

function TDSAudioFilter.set_CallbackPCM(Callback : IDSPackDCDSPFilterCallBack): HRESULT; stdcall;
begin
  fCallback := Callback;
  Result := S_OK;
end;

function TDSAudioFilter.get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream): HRESULT; stdcall;
var
  StreamPos : TReferenceTime;
begin
  Stream := @fStream;
  if fMediaLocked or fFlushing or (not fPlaying) or
     (StreamTime(StreamPos) <> S_OK) or (fVisBuffer = nil) or
     (fEOS and (StreamPos >= fEOSDelay))then
  begin
    Size := 0;
    Buffer := nil;
    Result := S_FALSE;
    Exit;
  end;

  while fLocked do
  begin

  end;

  if fIsDVD then StreamPos := fVisBufferStartTime + 36200000;
  Buffer := GetBufferAtTimeStamp(StreamPos,Size);
  if Buffer = nil then
  begin
    Size := 0;
    Buffer := nil;
    Result := S_FALSE;
    Exit;
  end;
  Result := S_OK;
end;

function TDSAudioFilter.get_EnableDelay(out Enabled : Boolean): HRESULT; stdcall;
begin
  Enabled := fEnableDelay;
  Result := S_OK;
end;

function TDSAudioFilter.set_EnableDelay(Enabled : Boolean): HRESULT; stdcall;
begin
  fEnableDelay := Enabled;
  Result := S_OK;
end;

function TDSAudioFilter.get_Delay(out Delay : integer): HRESULT; stdcall;
begin
  Delay := fDelay;
  Result := S_OK;
end;

function TDSAudioFilter.set_Delay(Delay : integer): HRESULT; stdcall;
begin
  fDelay := Delay;
  Result := S_OK;
end;

function TDSAudioFilter.EndOfStream: HRESULT;
var
  s : TReferenceTime;
begin
  StreamTime(s);
  fEOSDelay := s + (fVisBufferStartTime + (fVisBufferDelay * 2));
  fEOS := True;
  Result := inherited EndOfStream;
end;

function TDSAudioFilter.CheckDVD : Boolean;
var
  EnumFilters : IEnumFilters;
  Filter : IBaseFilter;
  ClassID : TGUID;
const
  CLSID_InterVideoDVDNavigator : TGUID = '{AABC1235-776D-11D2-8010-00104B9B8592}';
  CLSID_CyberlinkDVDNavigator  : TGUID = '{10D5F9E1-0360-11D5-8F2A-0080C84E9C39}';
begin
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
end;
{**** DSPack Audio Filter Component ********************************************}
constructor TDSPackDCDSPFilter.Create(AOwner: TComponent);
var
  hr : HRESULT;
begin
  inherited Create(AOwner);
  fFilter := TDSAudioFilter.Create(Name,nil,hr);
  fBaseFilter := fFilter as iBaseFilter;
  DSPackAudioFilter := fFilter as IDSPackDCDSPFilterInterface;
  DSPackVisualIntf := fFilter as IDSPackDCDSPFilterVisualInterface;
  fCriticalSection := TCriticalSection.Create;
  fVisualEnabled := False;
  fVisualAfterDSP := False;
  fVisualInterval := 40;
  fVisualTimer := TTimer.Create(Self);
  fVisualTimer.OnTimer := OnTimer;
  fVisualTimer.Enabled := False;
  fVisualtimer.Interval := fVisualInterval;
  fDSPEnabled := False;
end;

destructor TDSPackDCDSPFilter.Destroy;
begin
  fVisualTimer.Enabled := False;
  fVisualTimer.Free;
  FilterGraph := nil;
  DSPackAudioFilter := nil;
  DSPackVisualIntf := nil;
  FBaseFilter := nil;
  fCriticalSection.Free;
  fFilter := nil;
  inherited Destroy;
end;

procedure TDSPackDCDSPFilter.SetFilterGraph(AFilterGraph: TFilterGraph);
begin
  if AFilterGraph = FFilterGraph then Exit;
  if FFilterGraph <> nil then TMyFilterGraph(FFilterGraph).RemoveFilter(self);
  if AFilterGraph <> nil then TMyFilterGraph(AFilterGraph).InsertFilter(self);
  FFilterGraph := AFilterGraph;
end;

function TDSPackDCDSPFilter.GetFilter: IBaseFilter;
begin
  Result := fBaseFilter;
end;

function TDSPackDCDSPFilter.GetName: string;
begin
  Result := Name;
end;

procedure TDSPackDCDSPFilter.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
begin
  case operation of
    foAdded:
    begin
      RefreshSettings;
    end;
    foRemoving:
    begin
      if fBaseFilter <> nil then fBaseFilter.Stop;
    end;
    foRefresh: RefreshSettings;
  end;
end;

function TDSPackDCDSPFilter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if not Succeeded(Result) then
    if Assigned(fBaseFilter) then
      Result := fBaseFilter.QueryInterface(IID, Obj);
end;

procedure TDSPackDCDSPFilter.RefreshSettings;
begin
  if Assigned(DSPackAudioFilter) then
    DSPackAudioFilter.set_CallBackPCM(Self);
  if Assigned(DSPackVisualIntf) then
    DSPackVisualIntf.set_VISafterDSP(fVisualAfterDSP);
end;

function TDSPackDCDSPFilter.PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT; stdcall;
begin
  if Assigned(fOnPCMData) and fDSPEnabled then
  begin
    fCriticalSection.Enter;
    try
      fOnPCMData(Self,Buffer,Length,NewSize,Stream);
    finally
      fCriticalSection.Leave;
    end;
  end else
  begin
    NewSize := Length;
  end;
  Result := S_OK;
end;

function TDSPackDCDSPFilter.MediaTypeChanged(Stream : PDSStream) : HRESULT; stdcall;
begin
  if Assigned(fOnMediaTypeChanged) then fOnMediaTypeChanged(Self,Stream);
  Result := S_OK;
end;

function TDSPackDCDSPFilter.Flush : HRESULT; stdcall;
begin
  if Assigned(fOnFlush) then fOnFlush(Self);
  Result := S_OK;
end;

procedure TDSPackDCDSPFilter.OnTimer(Sender : TObject);
var
  Buffer : Pointer;
  Stream : PDSStream;
  Size : integer;
begin
  if Assigned(DSPackVisualIntf) and Assigned(fOnVisualData) and
     (DSPackVisualIntf.get_VisualData(Buffer,Size,Stream) = S_OK)
     then fOnVisualData(Sender,Buffer,Size,Stream);
end;

procedure TDSPackDCDSPFilter.SetVisualEnabled (Enabled : Boolean);
begin
  if Enabled = fVisualEnabled then Exit;
  fVisualEnabled := Enabled;
  fVisualTimer.Enabled := fVisualEnabled;
end;

procedure TDSPackDCDSPFilter.SetVisualInterval (Interval : Word);
begin
  if Interval = fVisualInterval then Exit;
  fVisualInterval := Interval;
  fVisualTimer.Interval := fVisualInterval;
end;

procedure TDSPackDCDSPFilter.SetVisualAfterDSP (After : Boolean);
begin
  if After = fVisualAfterDSP then Exit;
  fVisualAfterDSP := After;
  if Assigned(DSPackVisualIntf) then DSPackVisualIntf.set_VISafterDSP(fVisualAfterDSP);
end;

function TDSPackDCDSPFilter.GetStreaminfo : TDSStream;
var
  Stream : PDSStream;
begin
  if Assigned(DSPackAudioFilter) then DSPackAudioFilter.get_StreamInfo(Stream);
  Result := Stream^;
end;

function TDSPackDCDSPFilter.GetDelay : integer;
begin
  Result := 0;
  if Assigned(DSPackAudioFilter) then DSPackAudioFilter.get_Delay(Result);
end;

procedure TDSPackDCDSPFilter.SetDelay(Delay : integer);
begin
  if Assigned(DSPackAudioFilter) then DSPackAudioFilter.set_Delay(Delay);
end;

function TDSPackDCDSPFilter.GetEnableDelay : Boolean;
begin
  Result := False;
  if Assigned(DSPackAudioFilter) then DSPackAudioFilter.get_EnableDelay(Result);
end;

procedure TDSPackDCDSPFilter.SetEnableDelay(Enabled : Boolean);
begin
  if Assigned(DSPackAudioFilter) then DSPackAudioFilter.set_EnableDelay(Enabled);
end;
{**** FilterGraph **************************************************************}
procedure TMyFilterGraph.InsertFilter(AFilter: IFilter);
begin
  inherited InsertFilter(AFilter);
end;

procedure TMyFilterGraph.RemoveFilter(AFilter: IFilter);
begin
  inherited RemoveFilter(AFilter);
end;
{**** Register Component *******************************************************}
procedure Register;
begin
  RegisterComponents('DSPack', [TDSPackDCDSPFilter]);
end;

end.





