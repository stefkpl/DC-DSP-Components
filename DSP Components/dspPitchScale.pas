
    (*********************************************************************
     *  dspPitchScale.pas                                                *
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
     *  This Code is based on smsPitchScale.c v1.01                      *
     *  COPYRIGHT 1999 Stephan M. Sprenger <sms@dspdimension.com>        *
     *  under The Wide Open License (WOL) http://www.dspguru.com/wol.htm *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)
{
  @abstract(DSP Filter to increase the Pitch without changing the Tempo.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspPitchScale;

interface

uses
  Windows, Classes, dspConst, dspUtils, dspFastFourier, Forms, Math,
  dspInterfaces;

type
  { TDCPitchScale - Component to inc/decrease the Pitch of an Audio Buffer
    without changing the Tempo. The Buffersize isn´t affected, only the Pitch
    of the Voice is changed. Not that the Filter delays the Data for a couple
    of Samples. }
  TDCPitchScale = class(TComponent, IDCPitchScale)
  protected
    {@exclude}
    fQuality : array[0..MaxChannels -1] of Byte;
    {@exclude}
    fPitchBuffer : PFloatArray;
    {@exclude}
    fPitchBufferSize : integer;
    {@exclude}
    fRover : array[0..MaxChannels -1] of integer;
    {@exclude}
    fWindowTable1 : array[0..8191] of Single;
    {@exclude}
    fWindowTable2 : array[0..8191] of Single;
    {@exclude}
    fAnaFreq : array[0..8191] of Single;
    {@exclude}
    fAnaMagn : array[0..8191] of Single;
    {@exclude}
    fSynFreq : array[0..8191] of Single;
    {@exclude}
    fSynMagn : array[0..8191] of Single;
    {@exclude}
    fInFIFO : array[0..MaxChannels -1] of array[0..8192 -1] of Single;
    {@exclude}
    fOutFIFO : array[0..MaxChannels -1] of array[0..8192 -1] of Single;
    {@exclude}
    fLastPhase : array[0..MaxChannels -1] of array[0..8192 div 2 -1] of Single;
    {@exclude}
    fSumPhase : array[0..MaxChannels -1] of array[0..8192 div 2 -1] of Single;
    {@exclude}
    fOutputAccum : array[0..MaxChannels -1] of array[0..8192 * 2 -1] of Single;
    {@exclude}
    fFFT : TDCFFT;
    {@exclude}
    fPitch : array[0..MaxChannels -1] of WORD;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
//    fLastPitch : array[0..MaxChannels -1] of Single;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    procedure PitchScale(pitchScale : Single; numSampsToProcess, fftFrameSize, osamp : integer; sampleRate : single; indata, outdata : PFloatArray; Channel : integer);
    {@exclude}
    procedure SetFFTSize(Size : TDCFFTSize);
    {@exclude}
    function GetFFTSize : TDCFFTSize;
    {@exclude}
    procedure SetQuality(Channel : Byte; Quality : Byte);
    {@exclude}
    function GetQuality(Channel : Byte) : Byte;
    {@exclude}
    procedure SetPitch(Channel : Byte; Pitch : WORD);
    {@exclude}
    function GetPitch(Channel : Byte) : WORD;
  public
    { Creates an Instance of TDCPitchScale. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Specifys the Quality of the Filter. Note that large Values require a lot
      of CPU Time. A Value of 8-32 is recommended. }
    property Quality[Channel : Byte] : Byte read GetQuality write SetQuality;
    { Specifys the Pitch of a Channel. Defualt value is 1000 which means that
      the Pitch isn´t changed. }
    property Pitch[Channel : Byte] : WORD read GetPitch write SetPitch;
    { Flushes the Filter. }
    procedure Flush;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Pitch(aChannel : Byte; out aPitch : WORD): HRESULT; stdcall;
    {@exclude}
    function set_Pitch(aChannel : Byte; aPitch : WORD): HRESULT; stdcall;
    {@exclude}
    function get_Quality(aChannel : Byte; out aQuality : Byte): HRESULT; stdcall;
    {@exclude}
    function set_Quality(aChannel : Byte; aQuality : Byte): HRESULT; stdcall;
    {@exclude}
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    {@exclude}
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
  published
    { Specifys the FFTSize that will be used. The Size must be at last 256.
      A Value of 1024 is recommended. }
    property FFTSize : TDCFFTSize read GetFFTSize write SetFFTSize;
    { Enables or Disables seperate Amplification. If Enabled every Channel will
      be Amplified by it´s own Amplification Value. If Disabled every Channel
      will be Amplified with the Amplification Value of Channel 0. }
    property Seperate : Boolean read fSeperate write fSeperate;
    { Enables or Disables the Filter. }
    property Enabled : Boolean read fEnabled write fEnabled;
    { On large Buffersize Processing the Main Application wouldn´t respond until
      the DSP is done. by using "ProcessMessages" and "SampleSize" the DSP will
      be splitted into N(N = SampleSize) Number of Samples and then Processed.
      After every Process if ProcessMessages is Enabled Application.Processmessages
      will be called. }
    property ProcessMessages : Boolean read fProcessMessages write fProcessMessages;
    { Sets the SampleSize so that the Buffer will be splitted to Process an amount
      of Data only on every Process. Set SampleSize to 0 to disable it. Prevent
      using small size Values. Use at least 1024 or more. }
    property SampleSize : Cardinal read fSampleSize write fSampleSize;
  end;

implementation

uses SysUtils;

constructor TDCPitchScale.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  fFFT := TDCFFT.Create(Self);
  fFFT.ReOrder := True;
  fFFT.Scale := False;
  for i := 0 to MaxChannels -1 do
  begin
    fQuality[i] := 8;
    fPitch[i] := 1000;
  end;
  SetFFTSize(fts1024);
  fPitchBuffer := AllocMem(2);
  fPitchBufferSize := 0;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCPitchScale.Destroy;
begin
  if Assigned(fPitchBuffer) then FreeMemory(fPitchBuffer);
  fFFT.Free;
  inherited Destroy;
end;

procedure TDCPitchScale.Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size,SampleRate,Bits,Channels,Float);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (Bits div 8) * Channels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,SampleRate,Bits,Channels,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCPitchScale.DoDSP(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf32 : PFloatArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  i, k : integer;
  NumSamples : integer;
  pPitch : Single;
  pQuality : Byte;
  lSize : integer;
begin
  lSize := Size div Channels * 4 div (Bits div 8);
  if fPitchBufferSize < lSize then
  begin
    fPitchBufferSize := lSize;
    fPitchBuffer := ReallocMemory(fPitchBuffer,fPitchBufferSize);
  end;
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for i := 0 to Channels -1 do
      begin
        if fSeperate then
        begin
          pPitch := fPitch[i] / 1000;
          pQuality := fQuality[i];
        end else
        begin
          pPitch := fPitch[0] / 1000;
          pQuality := fQuality[0];
        end;
        for k := 0 to NumSamples -1 do fPitchBuffer[k] := (Buf8[k * channels + i] - 128) * 128;
        PitchScale(pPitch,NumSamples,fFFT.FFTSizeInt,pQuality,SampleRate,fPitchBuffer,fPitchBuffer,i);
        for k := 0 to NumSamples -1 do Buf8[k * channels + i] := clip_8(Round(fPitchBuffer[k] / 128)) + 128;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for i := 0 to Channels -1 do
      begin
        if fSeperate then
        begin
          pPitch := fPitch[i] / 1000;
          pQuality := fQuality[i];
        end else
        begin
          pPitch := fPitch[0] / 1000;
          pQuality := fQuality[0];
        end;
        for k := 0 to NumSamples -1 do fPitchBuffer[k] := Buf16[k * channels + i];
        PitchScale(pPitch,NumSamples,fFFT.FFTSizeInt,pQuality,SampleRate,fPitchBuffer,fPitchBuffer,i);
        for k := 0 to NumSamples -1 do Buf16[k * channels + i] := clip_16(Round(fPitchBuffer[k]));
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for i := 0 to Channels -1 do
      begin
        if fSeperate then
        begin
          pPitch := fPitch[i] / 1000;
          pQuality := fQuality[i];
        end else
        begin
          pPitch := fPitch[0] / 1000;
          pQuality := fQuality[0];
        end;
        for k := 0 to NumSamples -1 do fPitchBuffer[k] := Cvt24BitTo32(Buf24[k * channels + i]) / 128;
        PitchScale(pPitch,NumSamples,fFFT.FFTSizeInt,pQuality,SampleRate,fPitchBuffer,fPitchBuffer,i);
        for k := 0 to NumSamples -1 do Buf24[k * channels + i] := Cvt32BitTo24(clip_24(Round(fPitchBuffer[k] * 128)));
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for i := 0 to Channels -1 do
        begin
          if fSeperate then
          begin
            pPitch := fPitch[i] / 1000;
            pQuality := fQuality[i];
          end else
          begin
            pPitch := fPitch[0] / 1000;
            pQuality := fQuality[0];
          end;
          for k := 0 to NumSamples -1 do fPitchBuffer[k] := Buf32[k * channels + i] * 32768;
          PitchScale(pPitch,NumSamples,fFFT.FFTSizeInt,pQuality,SampleRate,fPitchBuffer,fPitchBuffer,i);
          for k := 0 to NumSamples -1 do Buf32[k * channels + i] := fPitchBuffer[k] / 32768;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for i := 0 to Channels -1 do
        begin
          if fSeperate then
          begin
            pPitch := fPitch[i] / 1000;
            pQuality := fQuality[i];
          end else
          begin
            pPitch := fPitch[0] / 1000;
            pQuality := fQuality[0];
          end;
          for k := 0 to NumSamples -1 do fPitchBuffer[k] := Buf32i[k * channels + i] / 32768;
          PitchScale(pPitch,NumSamples,fFFT.FFTSizeInt,pQuality,SampleRate,fPitchBuffer,fPitchBuffer,i);
          for k := 0 to NumSamples -1 do Buf32i[k * channels + i] := clip_32(Int64(Round(fPitchBuffer[k])) * 32768);
        end;
      end;
    end;
  end;
end;

procedure TDCPitchScale.PitchScale(pitchScale : Single; numSampsToProcess, fftFrameSize, osamp : integer; sampleRate : single; indata, outdata : PFloatArray; Channel : integer);
var
  magn, phase, tmp, freqPerBin, expct, fract: Double;
  i,k, qpd, inFifoLatency, index, stepSize, fftFrameSize2, fftFrameSize_Single : integer;
  si, co : Extended;
begin
  fftFrameSize2 := fftFrameSize div 2;
  stepSize := fftFrameSize div osamp;
  freqPerBin := sampleRate / fftFrameSize;
  expct := 2 * PI * stepSize / fftFrameSize;
  inFifoLatency := fftFrameSize - stepSize;
  fftFrameSize_Single := fftFrameSize * SizeOf(Single);
//  if fLastPitch[Channel] <> pitchScale then Flush;
//  fLastPitch[Channel] := pitchScale;

  if (fRover[Channel] = 0) then fRover[Channel] := inFifoLatency;

  for i := 0 to numSampsToProcess -1 do
  begin
    fInFIFO[Channel][fRover[Channel]] := indata[i];
    outdata[i] := fOutFIFO[Channel][fRover[Channel]-inFifoLatency];
    inc(fRover[Channel]);
    if (fRover[Channel] >= fftFrameSize) then
    begin
      fRover[Channel] := inFifoLatency;
      for k := 0 to fftFrameSize -1 do
      begin
        fFFT.Complex[k].re := fInFIFO[Channel][k] * fWindowTable1[k];
        fFFT.Complex[k].im := 0;
      end;
      fFFT.FFT;
      for k := 0 to fftFrameSize2 do
      begin
        fAnaMagn[k] := 2*sqrt(sqr(fFFT.Complex[k].re) + sqr(fFFT.Complex[k].im));
        phase := arctan2(-fFFT.Complex[k].im,fFFT.Complex[k].re);
        tmp := (phase - fLastPhase[Channel][k]) - (k*expct);
        fLastPhase[Channel][k] := phase;
        qpd := Trunc(tmp / PI);
        if (qpd >= 0) then qpd := qpd + (qpd and 1)
                      else qpd := qpd - (qpd and 1);
        fAnaFreq[k] := k*freqPerBin + (osamp*(tmp - (PI*qpd))/Two_Pi)*freqPerBin;
      end;
      ZeroMemory(@fSynMagn, fftFrameSize_Single);
      ZeroMemory(@fSynFreq, fftFrameSize_Single);
      for k := 0 to fftFrameSize2 do
      begin
        index := Round(k/pitchScale);
        if (index <= fftFrameSize2) then
        begin
          if (fAnaMagn[index] > fSynMagn[k]) then
          begin
            fSynMagn[k] := fAnaMagn[index];
            fSynFreq[k] := fAnaFreq[index] * pitchScale;
          end;
          if ((fSynFreq[k] = 0) and (k > 0)) then
          begin
            fSynFreq[k] := fSynFreq[k-1];
            fSynMagn[k] := fSynMagn[k-1];
          end;
        end;
      end;
      for k := 0 to fftFrameSize2 do
      begin
        fSumPhase[Channel][k] := fSumPhase[Channel][k] + (Two_Pi*((fSynFreq[k] - (k*freqPerBin)) / freqPerBin)/osamp) + (k*expct);
        SinCos(fSumPhase[Channel][k],si,co);
        fFFT.Complex[k].re := fSynMagn[k] * co;
        fFFT.Complex[k].im := fSynMagn[k] * -si;
      end;
      for k := fftFrameSize2 +1 to fftFrameSize -1 do
      begin
        fFFT.Complex[k].re := 0;
        fFFT.Complex[k].im := 0;
      end;
      fFFT.IFFT;
      for k := 0 to fftFrameSize -1 do fOutputAccum[Channel][k] := fOutputAccum[Channel][k] +
        (fWindowTable2[k] / osamp * fFFT.Complex[k].re);
      Move(fOutputAccum[Channel][0],fOutFIFO[Channel][0],stepSize * SizeOf(Single));
      Move(fOutputAccum[Channel][stepSize], fOutputAccum[Channel][0], fftFrameSize_Single);
      Move(fInFIFO[Channel][stepSize],fInFIFO[Channel][0],inFifoLatency *  SizeOf(Single));
    end;
  end;
end;

procedure TDCPitchScale.Flush;
begin
  FillChar(fRover,MaxChannels * SizeOf(integer),0);
  FillChar(fInFIFO, 8192*sizeof(single)*MaxChannels,0);
  FillChar(fOutFIFO, 8192*sizeof(single)*MaxChannels,0);
  FillChar(fLastPhase, 8192*sizeof(single) div 2*MaxChannels,0);
  FillChar(fSumPhase, 8192*sizeof(single) div 2*MaxChannels,0);
  FillChar(fOutputAccum, 2 * 8192*sizeof(single)*MaxChannels,0);
  FillChar(fAnaFreq, 8192*sizeof(single),0);
  FillChar(fAnaMagn, 8192*sizeof(single),0);
end;

procedure TDCPitchScale.SetFFTSize(Size : TDCFFTSize);
var
  i : integer;
begin
  Flush;
  fFFT.FFTSize := Size;
  for i := 0 to fFFT.FFTSizeInt -1 do
  begin
    fWindowTable1[i] := GetWindowingValue(1,i,fFFT.FFTSizeInt,wmBartlett);
    fWindowTable2[i] := fWindowTable1[i] * 2 / (fFFT.FFTSizeInt / 2) / 1.1;
  end;
end;

function TDCPitchScale.GetFFTSize : TDCFFTSize;
begin
  Result := fFFT.FFTSize;
end;

procedure TDCPitchScale.SetQuality(Channel : Byte; Quality : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fQuality[Channel] := Quality;
end;

function TDCPitchScale.GetQuality(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fQuality[Channel];
end;

procedure TDCPitchScale.SetPitch(Channel : Byte; Pitch : WORD);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fPitch[Channel] := Pitch;
end;

function TDCPitchScale.GetPitch(Channel : Byte) : WORD;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fPitch[Channel];
end;
(*** IDCPitchScale ************************************************************)
function TDCPitchScale.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCPitchScale.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCPitchScale.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCPitchScale.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCPitchScale.get_Pitch(aChannel : Byte; out aPitch : WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  aPitch := GetPitch(aChannel);
end;

function TDCPitchScale.set_Pitch(aChannel : Byte; aPitch : WORD): HRESULT; stdcall;
begin
  Result := S_OK;
  SetPitch(aChannel,aPitch);
end;

function TDCPitchScale.get_Quality(aChannel : Byte; out aQuality : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aQuality := GetQuality(aChannel);
end;

function TDCPitchScale.set_Quality(aChannel : Byte; aQuality : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetQuality(aChannel,aQuality);
end;

function TDCPitchScale.get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
begin
  Result := S_OK;
  aFFTSize := GetFFTSize;
end;

function TDCPitchScale.set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFFTSize(aFFTSize);
end;

end.
