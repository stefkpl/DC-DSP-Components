
    (*********************************************************************
     *  dspDynamicAmplify.pas                                            *
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
  @abstract(DSP Filter to keep the Amplification at the Maximum Range.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspDynamicAmplify;

interface

uses
  Classes, dspConst, dspUtils, Math, Forms, dspInterfaces, Windows;

type
  { TDCDynamicAmplify - DSP Filter to automiticaly keep the Volum eat a specific
    maximum Value. If the Volume is silent for a specific Time, the Amplification
    is raised to a specific Value. If the current Amplification would break the
    Range of the Audiodata, the Amplification is faded out to the maximum possible
    Amplification. }
  TDCDynamicAmplify = class(TComponent, IDCDynamicAmplify)
  protected
    {@exclude}
    fLastAmp : Single;
    {@exclude}
    fAmpWait : integer;
    {@exclude}
    fAmpWaitPos : integer;
    {@exclude}
    fReleaseTime : Cardinal;
    {@exclude}
    fAttackTime : Cardinal;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fMaxAmplification : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCDynamicAmplify. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. Buffer must be aligned on a 16 Byte
      boundary. Use the 'GetAlignedMemory' to create such a Buffer. Float is needed
      for 32 Bit Buffers, to determane whether the Buffer is Float or Integer. }
    procedure Process(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_AttackTime(out aAttackTime: Cardinal): HRESULT; stdcall;
    {@exclude}
    function set_AttackTime(aAttackTime: Cardinal): HRESULT; stdcall;
    {@exclude}
    function get_ReleaseTime(out aReleaseTime: Cardinal): HRESULT; stdcall;
    {@exclude}
    function set_ReleaseTime(aReleaseTime: Cardinal): HRESULT; stdcall;
    {@exclude}
    function get_MaxAmplification(out aMaxAmplification: Cardinal): HRESULT; stdcall;
    {@exclude}
    function set_MaxAmplification(aMaxAmplification: Cardinal): HRESULT; stdcall;
    {@exclude}
    function get_CurrentAmplification(out aCurrentAmplification: Single): HRESULT; stdcall;
  published
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
    { Specifys the Amplification Value than will be used when Amplifying. }
    property AttackTime : Cardinal read fAttackTime write fAttackTime;
    { Specifys the Time in MilliSeconds that the DSP will wait to continue
      Amplification, after the Maximum has been reached. }
    property ReleaseTime : Cardinal read fReleaseTime write fReleaseTime;
    { Sets the Amplification for a Channel. Default Value is 10000, which means
      that no Amplification occours. A Value of 20000 raises the Amplification
      by 2. }
    property MaxAmplification : Cardinal read fMaxAmplification write fMaxAmplification;
    { Retrieves the Current Amplification Factor. }
    property CurrentAmplification : Single read fLastAmp;
  end;

implementation

constructor TDCDynamicAmplify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLastAmp := 1.0;
  fAttackTime := 1000;
  fReleaseTime := 3000;
  fMaxAmplification := 10000;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCDynamicAmplify.Destroy;
begin
  inherited Destroy;
end;

procedure TDCDynamicAmplify.Process(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size,Samplerate,Bits,Channels,Float);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (Bits div 8) * Channels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,Samplerate,Bits,Channels,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCDynamicAmplify.DoDSP(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24  : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i,c : integer;
  LoudestSample : integer;
  LoudestSample64 : Int64;
  AmpFactor,
  AmpFactorS : Single;
  tmp : integer;
  sLastAmp : Single;
  SmoothAmp : Single;
begin
  LoudestSample := 0;
  LoudestSample64 := 0;
  NumSamples := Size div (Bits div 8);
  AmpFactorS := fMaxAmplification / 1000;
  AmpFactor := 1.0;
  fAmpWait := Round((Samplerate * Channels * (Bits div 8) * Int64(fReleaseTime)) / 1000);

  Buf32 := nil;
  Buf16 := nil;
  Buf24 := nil;
  Buf32i := nil;
  Buf8 := nil;

  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for i := 0 to (NumSamples) -1 do
        // Check for the Loudest Sample in this Chunk
        if abs(Buf8^[i] - 128) > LoudestSample then LoudestSample := abs(Buf8^[i] - 128);
      AmpFactor := 128 / LoudestSample;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for i := 0 to (NumSamples) -1 do
        // Check for the Loudest Sample in this Chunk
        if abs(Buf16^[i]) > LoudestSample then LoudestSample := abs(Buf16^[i]);
      AmpFactor := 32768 / LoudestSample;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for i := 0 to (NumSamples) -1 do
        // Check for the Loudest Sample in this Chunk
        if abs(Cvt24BitTo32(Buf24^[i])) > LoudestSample then LoudestSample := abs(Cvt24BitTo32(Buf24^[i]));
      AmpFactor := 8388608 / LoudestSample;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for i := 0 to (NumSamples) -1 do
        begin
          // Check for the Loudest Sample in this Chunk
          tmp := Round(abs(Buf32^[i]) * 32767);
          if tmp > LoudestSample then LoudestSample := tmp;
        end;
        AmpFactor := 32768 / LoudestSample;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for i := 0 to (NumSamples) -1 do
          // Check for the Loudest Sample in this Chunk
          if abs(Buf32i^[i]) > LoudestSample64 then LoudestSample64 := abs(Buf32i^[i]);
        AmpFactor := 2147483648 / LoudestSample64;
      end;
    end;
  end;

  sLastAmp := fLastAmp;
  if AmpFactor > AmpFactorS then AmpFactor := AmpFactorS;

  if AmpFactor < fLastAmp then
  begin
    fAmpWaitPos := 0;
    fLastAmp := AmpFactor;
  end else
  begin
    if fAmpWaitPos <= fAmpWait then
    begin
      inc(fAmpWaitPos,Size);
      AmpFactor := fLastAmp;
    end else
    begin
      fLastAmp := fLastAmp + ((Size * (fAttackTime / 1000) / Samplerate / Channels / (Bits div 8)));
      AmpFactor := fLastAmp;
      if AmpFactor > AmpFactorS then
      begin
        AmpFactor := AmpFactorS;
        fLastAmp := AmpFactor;
      end;
    end;
  end;

  NumSamples := NumSamples div Channels;
  case Bits of
    8:
    begin
      for c := 0 to Channels -1 do
      begin
        SmoothAmp := (sLastAmp - AmpFactor) / NumSamples;
        for i := 0 to (NumSamples -1) do Buf8^[i * Channels + c] := Clip_8(Trunc((Buf8^[i * Channels + c] - 128) * (sLastAmp - (SmoothAmp * i)))) + 128;
      end;
    end;
    16:
    begin
      for c := 0 to Channels -1 do
      begin
        SmoothAmp := (sLastAmp - AmpFactor) / NumSamples;
        for i := 0 to (NumSamples -1) do Buf16^[i * Channels + c] := Clip_16(Trunc(Buf16^[i * Channels + c] * (sLastAmp - (SmoothAmp * i))));
      end;
    end;
    24:
    begin
      for c := 0 to Channels -1 do
      begin
        SmoothAmp := (sLastAmp - AmpFactor) / NumSamples;
        for i := 0 to (NumSamples -1) do Buf24^[i * Channels + c] := Cvt32BitTo24(Clip_24(Trunc(Cvt24BitTo32(Buf24^[i * Channels + c]) * (sLastAmp - (SmoothAmp * i)))));
      end;
    end;
    32:
    begin
      if Float then
      begin
        for c := 0 to Channels -1 do
        begin
          SmoothAmp := (sLastAmp - AmpFactor) / NumSamples;
          for i := 0 to (NumSamples -1) do Buf32^[i * Channels + c] := Buf32^[i * Channels + c] * (sLastAmp - (SmoothAmp * i));
        end;
      end else
      begin
        for c := 0 to Channels -1 do
        begin
          SmoothAmp := (sLastAmp - AmpFactor) / NumSamples;
          for i := 0 to (NumSamples -1) do Buf32i^[i * Channels + c] := Clip_32(Trunc(Int64(Buf32i^[i * Channels + c]) * (sLastAmp - (SmoothAmp * i))));
        end;
      end;
    end;
  end;
end;
(*** IDCDynamicAmplify ********************************************************)
function TDCDynamicAmplify.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCDynamicAmplify.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCDynamicAmplify.get_AttackTime(out aAttackTime: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  aAttackTime := fAttackTime;
end;

function TDCDynamicAmplify.set_AttackTime(aAttackTime: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  fAttackTime := aAttackTime;
end;

function TDCDynamicAmplify.get_ReleaseTime(out aReleaseTime: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  aReleaseTime := fReleaseTime;
end;

function TDCDynamicAmplify.set_ReleaseTime(aReleaseTime: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  fReleaseTime := aReleaseTime;
end;

function TDCDynamicAmplify.get_MaxAmplification(out aMaxAmplification: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  aMaxAmplification := fMaxAmplification;
end;

function TDCDynamicAmplify.set_MaxAmplification(aMaxAmplification: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  fMaxAmplification := aMaxAmplification;
end;

function TDCDynamicAmplify.get_CurrentAmplification(out aCurrentAmplification: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aCurrentAmplification := fLastAmp;
end;

end.

