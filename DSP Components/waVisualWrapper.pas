
    (*********************************************************************
     *  waVisualWrapper.pas                                              *
     *                                                                   *
     *  This unit is Part of the DC-DSP Component Pack v1.0              *
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
  @abstract(Wrapper Component for Winamp 2 Visual Plugins.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit waVisualWrapper;

interface

uses
  Classes, Windows, waConst, Messages, SysUtils, dspConst, waUtils,
  SyncObjs, dspFastFourier, dspUtils;

type
  {@exclude}
  TDCWAVisualPluginItem = class;

  {@exclude}
  TDCWAVisualPluginItemThread  = class(TThread)
  private
    tBuf : array[0..32768] of integer;
    fFFT : TDCFFT;
    fPlugin : PWinampVisModule;
    fBuffer : Pointer;
    fHasSpectrum : Boolean;
    fHasWaveform : Boolean;
    fLibHandle : hWnd;
    fSubPluginNames : TStringList;
    fPluginName : String;
    fHeader : PWinampVisHeader;
    fPluginCount : integer;
    fCurrentIndex : integer;
    fOwner : TDCWAVisualPluginItem;
    function GetSubPluginName(Index : integer) : String;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TDCWAVisualPluginItem);
    property SubPluginName[Index : integer] : String read GetSubPluginName;
    procedure ProcessFFTBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; Smooth : Boolean; PluginBuffer : Pointer; Float : Boolean);
    procedure ProcessWaveBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; PluginBuffer : Pointer; Float : Boolean);
  published
    property CurrentIndex : integer read fCurrentIndex;
    property PluginCount : integer read fPluginCount;
    property PluginName : String read fPluginName;
    property HasSpectrum : Boolean read fHasSpectrum;
    property HasWaveform : Boolean read fHasWaveform;
  end;

  {@exclude}
  TDCWAVisualPluginItem  = class(TCollectionItem)
  private
    fStream : TDSStream;
    fThreadRunning : Boolean;
    fLoading : Boolean;
    fThread : TDCWAVisualPluginItemThread;
    fOwnerWindow : hWnd;
    fFileName : String;
    fPriority : TThreadPriority;
    function GetPriority : TThreadPriority;
    procedure SetPriority(aPriority : TThreadPriority);
    procedure SetFileName(aFileName : String);
    procedure ClearThread;
    procedure SetOwnerWindow(aOwner : hWnd);
    function GetSubPluginName(Index : integer) : String;
    function GetPluginName : String;
    function GetPluginCount : integer;
    function GetPluginCurrentIndex : integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure InitStream(Stream : PDSStream);
    procedure Init(Index : integer);
    procedure Config(Index : integer);
    procedure Quit;
    procedure Process(Buffer : Pointer; Size : integer);
    property SubPluginName[Index : integer] : String read GetSubPluginName;
    property PluginName : String read GetPluginName;
    property PluginCount : integer read GetPluginCount;
    property PluginCurrentIndex : integer read GetPluginCurrentIndex;
  published
    property Priority : TThreadPriority read GetPriority write SetPriority;
    property Filename : String read fFileName write SetFileName;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

  {@exclude}
  TDCWAVisualPluginList = class(TCollection)
  private
    fDirectory : String;
    fOwnerWindow : hWnd;
    fStream : TDSStream;
    function  GetItem(Index : Integer) : TDCWAVisualPluginItem;
    procedure SetItem(Index : Integer; Value : TDCWAVisualPluginItem);
    procedure SetDirectory(Dir : String);
    procedure SetOwnerWindow(Window : hWnd);
    function IsOurPlugin(Filename : String) : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add : TDCWAVisualPluginItem;
    property Items[Index: Integer]: TDCWAVisualPluginItem read GetItem write SetItem; default;
    procedure Init(Stream : PDSStream);
    procedure Process(Buffer : Pointer; Size : integer);
    procedure Quit;
  published
    property Directory : String read fDirectory write SetDirectory;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

  { TDCWAVisualWrapper - Winamp2 Visual Plugin Wrapper Component. }
  TDCWAVisualWrapper = class(TComponent)
  private
    {@exclude}
    fPlugins : TDCWAVisualPluginList;
  public
    { TDCWAVisualWrapper constructor. }
    constructor Create(AOwner: TComponent); override;
    { TDCWAVisualWrapper destructor }
    destructor Destroy; override;
  published
    { The core of the Wrapper. Use the Plugins Property to Start/Stop ... Plugins. }
    property Plugins : TDCWAVisualPluginList read fPlugins write fPlugins;
  end;

implementation
{*** TDCWAVisualPluginItemThread **********************************************}
constructor TDCWAVisualPluginItemThread.Create(AOwner : TDCWAVisualPluginItem);
begin
  fOwner := AOwner;
  fFFT := TDCFFT.Create(nil);
  fFFT.ReOrder := True;
  fFFT.FFTSize := fts1024;
  fFFT.Scale := False;
  fSubPluginNames := TStringList.Create;
  fPluginCount := 0;
  fCurrentIndex := -1;
  fHasSpectrum := False;
  fHasWaveform := False;
  inherited Create(False);
end;

function TDCWAVisualPluginItemThread.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if (Index < 0) or (Index > fPluginCount -1) or (Index > fSubPluginNames.Count -1) then Exit;
  Result := fSubPluginNames[Index];
end;

procedure TDCWAVisualPluginItemThread.Execute;
var
  aMsg : TMsg;
  aProc : function : Pointer; stdcall;
//  aModule : PWinampVisModule;
begin
  PeekMessage(aMsg,0,0,0,PM_NOREMOVE);

  fOwner.fThreadRunning := True;

  while not Terminated do
  begin
    if not PeekMessage(aMsg,0,0,0,PM_REMOVE) then
    begin
      WaitForSingleObject(Handle,10);
      Continue;
    end;

    case aMsg.message of
      UM_DC_LOAD:
      begin
        fSubPluginNames.Clear;
        fPluginName := '';
        fPluginCount := 0;
        fCurrentIndex := -1;
        fLibHandle := LoadLibrary(PChar(aMsg.wParam));
        if fLibHandle <> 0 then
        begin
          aProc := GetProcAddress(fLibHandle,PChar('winampVisGetHeader'));
          if @aProc <> nil then
          begin
            fHeader := aProc;
            if fHeader.Version <> $101 then
            begin
              FreeLibrary(fLibHandle);
              break;
            end;
            fPluginName := StrPas(fHeader.Description);
            while fHeader.GetModule(fPluginCount) <> nil do
            begin
              fSubPluginNames.Add(fHeader.GetModule(fPluginCount).description);
              inc(fPluginCount);
            end;
          end;
        end;
        fOwner.fLoading := False;
      end;
      UM_DC_FREE:
      begin
        if fLibHandle <> 0 then
        begin
          if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
          fSubPluginNames.Clear;
          fPlugin := nil;
          fCurrentIndex := -1;
          FreeLibrary(fLibHandle);
          fLibHandle := 0;
        end;
        break;
      end;
      UM_DC_INIT:
      begin
        fOwner.fThreadRunning := False;
        if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
        if fLibHandle <> 0 then
        begin
          FreeLibrary(fLibHandle);
          fLibHandle := 0;
        end;

        fLibHandle := LoadLibrary(PChar(fOwner.fFileName));
        if fLibHandle <> 0 then
        begin
          aProc := GetProcAddress(fLibHandle,PChar('winampVisGetHeader'));
          if @aProc <> nil then
          begin
            fHeader := aProc;
            if fHeader.Version <> $101 then
            begin
              FreeLibrary(fLibHandle);
              break;
            end;
          end;
        end;

        fPlugin := fHeader.GetModule(aMsg.wParam);
        fPlugin.hDllInstance := fLibHandle;
        fPlugin.hwndParent := GetDesktopWindow;// fOwner.fOwnerWindow;
        fPlugin.sRate := fOwner.fStream.Frequency;
        fPlugin.nCh := fOwner.fStream.Channels;
        fHasSpectrum := fPlugin.spectrumNch > 0;
        fHasWaveform := fPlugin.waveformNch > 0;

        fPlugin.Init(fPlugin);
        fCurrentIndex := aMsg.wParam;
        fOwner.fThreadRunning := True;
      end;
      UM_DC_QUIT:
      begin
        if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
        fPlugin := nil;
        fHasSpectrum := False;
        fHasWaveform := False;
        fCurrentIndex := -1;
      end;
{      UM_DC_CONFIG:
      begin
        if (aMsg.wParam >= 0) and (aMsg.wParam < fPluginCount) then
        begin
          aModule := fHeader.getModule(aMsg.wParam);
          aModule.hDllInstance := fLibHandle;
          aModule.hwndParent := fOwner.fOwnerWindow;
          aModule.nCh := fOwner.fStream.Channels;
          aModule.sRate := fOwner.fStream.Frequency;
          aModule.Config(aModule);
        end;
      end;}
      UM_DC_RENDER:
      begin
        if fCurrentIndex > -1 then
        begin
          if HasWaveform then ProcessWaveBuffer(fBuffer,fOwner.fStream.Channels * (fOwner.fStream.Bits div 8) * 576 * 2,fOwner.fStream.Channels,fPlugin.waveformNch,fOwner.fStream.Frequency,fOwner.fStream.Bits,576,@fPlugin.waveformData,fOwner.fStream.Float);
          if HasSpectrum then ProcessFFTBuffer(fBuffer,fOwner.fStream.Channels * (fOwner.fStream.Bits div 8) * 576,fOwner.fStream.Channels,fPlugin.spectrumNch,fOwner.fStream.Frequency,fOwner.fStream.Bits,512,False,@fPlugin.spectrumData,fOwner.fStream.Float);
          fPlugin.Render(fPlugin);
        end;
      end;
      WM_QUIT:
      begin
        TranslateMessage(aMsg);
        DispatchMessage(aMsg);
        break;
      end else
      begin
        TranslateMessage(aMsg);
        DispatchMessage(aMsg);
      end;
    end;
  end;

  if (fCurrentIndex > -1) and (fPlugin <> nil) then
  begin
    fPlugin.Quit(fPlugin);
    fPlugin := nil;
    fCurrentIndex := -1;
  end;
  if fLibHandle <> 0 then
  begin
    FreeLibrary(fLibHandle);
    fLibHandle := 0;
  end;

  fFFT.Free;
  FreeAndNil(fSubPluginNames);
  fOwner.fThreadRunning := False;
end;

procedure TDCWAVisualPluginItemThread.ProcessWaveBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; PluginBuffer : Pointer; Float : Boolean);
var
  pBuf8  : PByteArray;
  pBuf16 : PSmallIntArray;
  pBuf24 : PInteger24Array;
  pBuf32 : PFloatArray;
  pBuf32i : PIntegerArray;
  NumSamples : integer;
  SamplesPerChannel : integer;
  i,c,c2 : integer;
  tmp : integer;
  tmp2 : integer;
  Res : PVisBuffer;
begin
  Res := PVisBuffer(PluginBuffer);
  NumSamples := Length div (Bits div 8);
  SamplesPerChannel := Numsamples div WaveChannels;

  if PluginChannels = 1 then
  begin
    case Bits of
      8:
      begin
        pBuf8 := PByteArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      16:
      begin
        pBuf16 := PSmallIntArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      24:
      begin
        pBuf24 := PInteger24Array(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      32:
      begin
        if Float then
        begin
          pBuf32 := PFloatArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end else
        begin
          pBuf32i := PIntegerArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end;
      end;
    end;
    tmp2 := SamplesPerChannel div NumBands;
    if tmp2 < 1 then tmp2 := 1;
    for i := 0 to (NumBands -1) do
    begin
      tmp := 0;
      for c := 0 to tmp2 -1 do
      begin
        tmp := tmp + tBuf[(i * tmp2) + c];
      end;
      tmp := tmp div tmp2;
      if Res <> nil then Res[0][i] := Clip_8(tmp);
    end;
  end else
  begin // PluginChannels > 1 -- WaveChannels = 1
    for c2 := 0 to PluginChannels -1 do
    begin
      if (WaveChannels = 1) then
      begin
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if Float then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          if Res <> nil then Res[0][i] := Clip_8(tmp);
          if Res <> nil then Res[1][i] := Clip_8(tmp);
        end;
        break;
      end else
      begin // PluginChannels > 1 -- WaveChannels > 1
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf8^[(i * WaveChannels) + c2] - 128);
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf16^[(i * WaveChannels) + c2] div 256);
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c2]) div 32768);
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if Float then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := Round(pBuf32^[(i * WaveChannels) + c2] * 256);
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := (pBuf32i^[(i * WaveChannels) + c2] div 8388352);
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[((i * tmp2) + c)];
          end;
          tmp := tmp div tmp2;
          if Res <> nil then Res[c2][i] := Clip_8(tmp);
        end;
      end;
    end;
  end;
end;

procedure TDCWAVisualPluginItemThread.ProcessFFTBuffer(Buffer : Pointer; Length : integer; WaveChannels, PluginChannels : Byte; Frequency : integer; Bits : Byte; NumBands : integer; Smooth : Boolean; PluginBuffer : Pointer; Float : Boolean);
var
  pBuf8  : PByteArray;
  pBuf16 : PSmallIntArray;
  pBuf24 : PInteger24Array;
  pBuf32 : PFloatArray;
  pBuf32i : PIntegerArray;
  NumSamples : integer;
  SamplesPerChannel : integer;
  i,c,c2 : integer;
  tmp : integer;
  tmp2 : integer;
  Res : PVisBuffer;
begin
  Res := PVisBuffer(PluginBuffer);
  NumSamples := Length div (Bits div 8);
  SamplesPerChannel := Numsamples div WaveChannels;
  fFFT.Flush;
  if PluginChannels = 1 then
  begin
    case Bits of
      8:
      begin
        pBuf8 := PByteArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      16:
      begin
        pBuf16 := PSmallIntArray(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      24:
      begin
        pBuf24 := PInteger24Array(Buffer);
        for i := 0 to (SamplesPerChannel -1) do
        begin
          tmp := 0;
          for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
          tmp := tmp div WaveChannels;
          tBuf[i] := tmp;
        end;
      end;
      32:
      begin
        if Float then
        begin
          pBuf32 := PFloatArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end else
        begin
          pBuf32i := PIntegerArray(Buffer);
          for i := 0 to (SamplesPerChannel -1) do
          begin
            tmp := 0;
            for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
            tmp := tmp div WaveChannels;
            tBuf[i] := tmp;
          end;
        end;
      end;
    end;
    tmp2 := SamplesPerChannel div NumBands;
    if tmp2 < 1 then tmp2 := 1;
    for i := 0 to (NumBands -1) do
    begin
      tmp := 0;
      for c := 0 to tmp2 -1 do
      begin
        tmp := tmp + tBuf[(i * tmp2) + c];
      end;
      tmp := tmp div tmp2;
      fFFT.Complex[i].re := tmp;
    end;
    fFFT.FFT;
    for i := 0 to NumBands -1 do
      if Res <> nil then Res[0][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
  end else
  begin // PluginChannels > 1 -- WaveChannels = 1
    for c2 := 0 to PluginChannels -1 do
    begin
      if (WaveChannels = 1) then
      begin
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf8^[(i * WaveChannels) + c] - 128);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf16^[(i * WaveChannels) + c] div 256);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := 0;
              for c := 0 to (WaveChannels -1) do tmp := tmp + (Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c]) div 32768);
              tmp := tmp div WaveChannels;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if Float then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + Round(pBuf32^[(i * WaveChannels) + c] * 256);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := 0;
                for c := 0 to (WaveChannels -1) do tmp := tmp + (pBuf32i^[(i * WaveChannels) + c] div 8388352);
                tmp := tmp div WaveChannels;
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          fFFT.Complex[i].re := tmp;
        end;
        fFFT.FFT;
        for i := 0 to NumBands -1 do
          if Res <> nil then
          begin
            Res[0][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
            Res[1][i] := Res[0][i];
          end;
        break;
      end else
      begin // PluginChannels > 1 -- WaveChannels > 1
        if c2 > 0 then fFFT.Flush;
        case Bits of
          8:
          begin
            pBuf8 := PByteArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := (pBuf8^[(i * WaveChannels) + c2] - 128);
              tBuf[i] := tmp;
            end;
          end;
          16:
          begin
            pBuf16 := PSmallIntArray(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
             tmp := (pBuf16^[(i * WaveChannels) + c2] div 256);
             tBuf[i] := tmp;
            end;
          end;
          24:
          begin
            pBuf24 := PInteger24Array(Buffer);
            for i := 0 to (SamplesPerChannel -1) do
            begin
              tmp := Cvt24BitTo32(pBuf24^[(i * WaveChannels) + c2]) div 32768;
              tBuf[i] := tmp;
            end;
          end;
          32:
          begin
            if Float then
            begin
              pBuf32 := PFloatArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
               tmp := Round(pBuf32^[(i * WaveChannels) + c2] * 256);
               tBuf[i] := tmp;
              end;
            end else
            begin
              pBuf32i := PIntegerArray(Buffer);
              for i := 0 to (SamplesPerChannel -1) do
              begin
                tmp := (pBuf32i^[(i * WaveChannels) + c2] div 8388352);
                tBuf[i] := tmp;
              end;
            end;
          end;
        end;
        tmp2 := SamplesPerChannel div NumBands;
        if tmp2 < 1 then tmp2 := 1;
        for i := 0 to (NumBands -1) do
        begin
          tmp := 0;
          for c := 0 to tmp2 -1 do
          begin
            tmp := tmp + tBuf[(i * tmp2) + c];
          end;
          tmp := tmp div tmp2;
          fFFT.Complex[i].re := tmp;
        end;
        fFFT.FFT;
        for i := 0 to NumBands -1 do
        begin
          if Res <> nil then Res[c2][i] := Clip_8(Round(FFTSum(fFFT.Complex[i].re,fFFT.Complex[i].im) / 20));
        end;
      end;
    end;
  end;
end;

{*** TDCWAVisualPluginItem ****************************************************}
constructor TDCWAVisualPluginItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fPriority := tpNormal;
  fOwnerWindow := GetDesktopWindow;
end;

destructor TDCWAVisualPluginItem.Destroy;
begin
  ClearThread;
  inherited Destroy;
end;

function TDCWAVisualPluginItem.GetPriority : TThreadPriority;
begin
  if fThreadRunning then Result := fThread.Priority
                    else Result := fPriority;
end;

procedure TDCWAVisualPluginItem.SetPriority(aPriority : TThreadPriority);
begin
  fPriority := aPriority;
  if fThreadRunning then fThread.Priority := fPriority;
end;

procedure TDCWAVisualPluginItem.SetFileName(aFileName : String);
begin
  ClearThread;
  if not FileExists(aFileName) then Exit;
  fFileName := aFileName;
  fThread := TDCWAVisualPluginItemThread.Create(Self);
  while not fThreadRunning do;
  fThread.Priority := fPriority;
  fLoading := True;
  PostThreadMessage(fThread.ThreadID,UM_DC_LOAD,integer(PChar(fFileName)),0);
  while fLoading do;
end;

procedure TDCWAVisualPluginItem.ClearThread;
begin
  if fThreadRunning then
  begin
    PostThreadMessage(fThread.ThreadID,UM_DC_FREE,0,0);
    while fThreadRunning do;
  end;
end;

procedure TDCWAVisualPluginItem.SetOwnerWindow(aOwner : hWnd);
begin
  fOwnerWindow := aOwner;
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  fThread.fPlugin.hwndParent := fOwnerWindow;
end;

function TDCWAVisualPluginItem.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if not fThreadRunning then SetFileName(fFilename);
  if not fThreadRunning then Exit;
  Result := fThread.SubPluginName[Index];
end;

function TDCWAVisualPluginItem.GetPluginName : String;
begin
  Result := '';
  if not fThreadRunning then SetFileName(fFilename);
  if not fThreadRunning then Exit;
  Result := fThread.PluginName;
end;

function TDCWAVisualPluginItem.GetPluginCount : integer;
begin
  Result := 0;
  if not fThreadRunning then SetFileName(fFilename);
  if not fThreadRunning then Exit;
  Result := fThread.PluginCount;
end;

function TDCWAVisualPluginItem.GetPluginCurrentIndex : integer;
begin
  Result := -1;
  if not fThreadRunning then SetFileName(fFilename);
  if not fThreadRunning then Exit;
  Result := fThread.CurrentIndex;
end;

procedure TDCWAVisualPluginItem.Init(Index : integer);
begin
  if (Index < 0) or (Index > fThread.PluginCount -1) then Exit;
  Quit;
  SetFileName(fFilename);
  if fThreadRunning then PostThreadMessage(fThread.ThreadID,UM_DC_INIT,Index,0);
end;

procedure TDCWAVisualPluginItem.Config(Index : integer);
var
  aModule : PWinampVisModule;
begin
  if not fThreadRunning then SetFileName(fFilename);
  if not fThreadRunning or (Index < 0) or (Index > fThread.PluginCount -1) then Exit;
  aModule := fThread.fHeader.getModule(Index);
  aModule.hDllInstance := fThread.fLibHandle;
  aModule.hwndParent := fOwnerWindow;
  aModule.nCh := fStream.Channels;
  aModule.sRate := fStream.Frequency;
  aModule.Config(aModule);
//  PostThreadMessage(fThread.ThreadID,UM_DC_CONFIG,Index,0);
end;

procedure TDCWAVisualPluginItem.Quit;
begin
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  PostThreadMessage(fThread.ThreadID,UM_DC_QUIT,0,0);
end;

procedure TDCWAVisualPluginItem.InitStream(Stream : PDSStream);
begin
  fStream := Stream^;
  OutputDebugString(pchar('fdsafdas ' + inttostr(Stream.Channels)));
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  fThread.fPlugin.sRate := fStream.Frequency;
  fThread.fPlugin.nCh := fStream.Channels;
end;

procedure TDCWAVisualPluginItem.Process(Buffer : Pointer; Size : integer);
begin
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  if (fStream.Bits = 0) or (fStream.Channels = 0) then Exit;
  fThread.fBuffer := Buffer;
  PostThreadMessage(fThread.ThreadID,UM_DC_RENDER,0,0);
end;
{*** TDCWAVisualPluginItemThread **********************************************}
constructor TDCWAVisualPluginList.Create;
begin
  inherited Create(TDCWAVisualPluginItem);
end;

destructor TDCWAVisualPluginList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDCWAVisualPluginList.Add : TDCWAVisualPluginItem;
begin
  Result := TDCWAVisualPluginItem(inherited Add);
end;

function TDCWAVisualPluginList.GetItem(Index : Integer) : TDCWAVisualPluginItem;
begin
  Result := TDCWAVisualPluginItem(inherited GetItem(Index));
end;

procedure TDCWAVisualPluginList.SetItem(Index : Integer; Value : TDCWAVisualPluginItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TDCWAVisualPluginList.Process(Buffer : Pointer; Size : integer);
var
  i : integer;
begin
  if Count < 1 then Exit;
  for i := 0 to Count -1 do
  begin
    Items[i].Process(Buffer,Size);
  end;
end;

function TDCWAVisualPluginList.IsOurPlugin(Filename : String) : Boolean;
var
  DLL : hWnd;
  Proc : function : Pointer; stdcall;
  Plugin : PWinampVisHeader;
  NumPlugins : integer;
begin
  Result := False;
  DLL := LoadLibrary(PChar(Filename));
  if DLL <> 0 then
  begin
    Proc := GetProcAddress(DLL,PChar('winampVisGetHeader'));
    if @Proc <> nil then
    begin
      Plugin := Proc;
      NumPlugins := 0;
      while Plugin.GetModule(NumPlugins) <> nil do inc(NumPlugins);
      Result := NumPlugins > 0;
    end;
    FreeLibrary(DLL);
  end;
end;

procedure TDCWAVisualPluginList.SetDirectory(Dir : String);
var
  srh: THandle;
  sr : TWin32FindData;
begin
  Dir := AddBackSlash(Dir);
  fDirectory := Dir;
  Clear;

  srh := FindFirstFile(PChar(fDirectory + 'vis_*.dll'),sr);
  if srh <> INVALID_HANDLE_VALUE then
  repeat
    if IsOurPlugin(fDirectory + sr.cFileName) then
    begin
      Add;
      Items[Count -1].Filename := fDirectory + sr.cFileName;
      Items[Count -1].OwnerWindow := fOwnerWindow;
      Items[Count -1].InitStream(@fStream);
    end;  
  until FindNextFile(srh,sr) = False;
  Windows.FindClose(srh);
end;

procedure TDCWAVisualPluginList.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  fOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].OwnerWindow := fOwnerWindow;
end;

procedure TDCWAVisualPluginList.Init(Stream : PDSStream);
var
  i : integer;
begin
  fStream := Stream^;
  OutputDebugString(pchar('fdsafdas33 ' + inttostr(Stream.Channels)));
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].InitStream(@fStream);
end;

procedure TDCWAVisualPluginList.Quit;
var
  i : integer;
begin
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].Quit;
end;
{*** TDCWAVisualWrapper *******************************************************}
constructor TDCWAVisualWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlugins := TDCWAVisualPluginList.Create;
end;

destructor TDCWAVisualWrapper.Destroy;
begin
  fPlugins.Free;
  inherited Destroy;
end;

end.
