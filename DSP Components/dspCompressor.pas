
    (*********************************************************************
     *  dspCompressor.pas                                                *
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
     *  based on the C++ Audacity Compressor Cpde.                       *
     *  (C) The Audacity Project under the Terms of the GPL !!!          *
     *  http://audacity.sourceforge.net/                                 *
     *                                                                   *
     *  Delphi Conversion:                                               *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *  further improvements are done by                                 *
     *  Jason Huang <jasonh77.tw@yahoo.com.tw>                           *
     *                                                                   *
     *********************************************************************)
{
  @abstract(DSP Filter to Compress (Amplify) Audio Data.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspCompressor;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math, dspInterfaces, Windows;

type
  { TDCCompressor - DSP Compressor Filter. }
  TDCCompressor = class(TComponent, IDCCompressor)
  protected
    {@exclude}
    fAttackTime : Single;
    {@exclude}
    fDecayTime : Single;
    {@exclude}
    fThresholdDB : Single;
    {@exclude}
    fRatio : Single;
    {@exclude}
    fGainDB : Single;
    {@exclude}
    fCircleSize : integer;
    {@exclude}
    fCirclePos : integer;
    {@exclude}
    fRMSSum : Double;
    {@exclude}
    fThreshold : Double;
    {@exclude}
    fGain : Double;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fCircle: PDoubleArray;
    {@exclude}
    fLevelCircle: PDoubleArray;
//++++++++++++++++++++++++++++++
    {@exclude}
    fRMSWindow: Single;
    {@exclude}
    fSamplePeriod: Double;
    {@exclude}
    fAchiement : Single;
    {@exclude}
    fAttackFactor: Double;
    {@exclude}
    fDecayFactor: Double;
    {@exclude}
    fCurrentGain: Double;
    {@exclude}
    fAboveThrs: Boolean;
    {@exclude}
    fInputLevel: Single;
    {@exclude}
    fGainReduction: Single;
//------------------------------
    {@exclude}
    fBufferSize: integer;
    {@exclude}
    fBuffer: PDoubleArray;
    {@exclude}
    function DoCompression(value: Single): single;
    {@exclude}
    function AvgCircle(value: Single): Single;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    procedure InitVars(Samplerate : integer; Channels: integer);
  public
    { Creates an Instance of TDCAmplify. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Samplerate : integer;Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_AttackTime(out aAttackTime: Single): HRESULT; stdcall;
    {@exclude}
    function set_AttackTime(aAttackTime: Single): HRESULT; stdcall;
    {@exclude}
    function get_DecayTime(out aDecayTime: Single): HRESULT; stdcall;
    {@exclude}
    function set_DecayTime(aDecayTime: Single): HRESULT; stdcall;
    {@exclude}
    function get_ThresholdDB(out aThresholdDB: Single): HRESULT; stdcall;
    {@exclude}
    function set_ThresholdDB(aThresholdDB: Single): HRESULT; stdcall;
    {@exclude}
    function get_Ratio(out aRatio: Single): HRESULT; stdcall;
    {@exclude}
    function set_Ratio(aRatio: Single): HRESULT; stdcall;
    {@exclude}
    function get_GainDB(out aGainDB: Single): HRESULT; stdcall;
    {@exclude}
    function set_GainDB(aGainDB: Single): HRESULT; stdcall;
//++++++++++++++++++++++++++++++
    {@exclude}
    function get_AboveThrs(out aAboveThrs: Boolean): HRESULT; stdcall;
    {@exclude}
    function get_InputLevel(out aInputLevel: Single): HRESULT; stdcall;
    {@exclude}
    function get_GainReduction(out aGainReduction: Single): HRESULT; stdcall;
//------------------------------
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
    { Specifys the Attack Time in MilliSeconds. }
    property AttackTime : Single read fAttackTime write fAttackTime;
    { Specifys the Release Time in Milli Seconds. }
    property DecayTime : Single read fDecayTime write fDecayTime;
    { Specifys the Threshold in Dezibel. }
    property ThresholdDB : Single read fThresholdDB write fThresholdDB;
    { Specifys the Amplification Ratio. }
    property Ratio : Single read fRatio write fRatio;
    { Specifys the maximum Amplification in Dezibel. }
    property GainDB : Single read fGainDB write fGainDB;
//++++++++++++++++++++++++++++++
    property AboveThrs : Boolean read fAboveThrs;
    property InputLevel : Single read fInputLevel;
    property GainReduction : Single read fGainReduction;
//------------------------------
  end;

implementation

uses SysUtils;

constructor TDCCompressor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fBufferSize := 2;
  fBuffer := AllocMem(fBufferSize);

//++++++++++++++++++++++++++++++
  fCircleSize := 2;
//------------------------------
  fCircle := AllocMem(fCircleSize * SizeOf(Double));
  fLevelCircle := AllocMem(fCircleSize * SizeOf(Double));

  fAttackTime := 0.05;
  fDecayTime := 0.5;
  fThresholdDB := -12.0;
  fRatio := 2.0;
  fGainDB := 0.0;
//++++++++++++++++++++++++++++++
  fRMSWindow := 2.0; // Window width for RMS caculating in ms
  fAchiement := 0.9; // Achiement of the target gain at the end of attack-time or release-time
  fCurrentGain := 1.0;
  fAboveThrs := false;
  fInputLevel := 0.0;
  fGainReduction := 1.0;
//------------------------------

  fCirclePos := 0;
  fRMSSum := 0.0;

  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCCompressor.Destroy;
begin
  FreeMemory(fLevelCircle);
  FreeMemory(fCircle);
  FreeMemory(fBuffer);
  inherited Destroy;
end;

procedure TDCCompressor.InitVars(Samplerate : integer; Channels: integer);
begin
//++++++++++++++++++++++++++++++
  fSamplePeriod := 1.0 / SampleRate;
  fThreshold := power(10.0, fThresholdDB / 20); // factor of 20 because it's amplitude
  fAttackFactor := 1.0 - 1.0 / exp(fSamplePeriod / (fAttackTime / (-ln(1.0 - fAchiement))));
  fDecayFactor := 1.0 - 1.0 / exp(fSamplePeriod / (fDecayTime / (-ln(1.0 - fAchiement))));
//------------------------------
  fGain := power(10.0, fGainDB/20); // factor of 20 because it's amplitude
end;

procedure TDCCompressor.Process(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
  i : integer;
  RMSWindowSize : integer;
begin
  if not fEnabled then Exit;
//++++++++++++++++++++++++++++++
  RMSWindowSize := Trunc(Samplerate * fRMSWindow / 1000 + 0.5);
  if fCircleSize <> RMSWindowSize then
  begin
    fCircleSize := RMSWindowSize;
    fCircle := ReallocMemory(fCircle, fCircleSize * SizeOf(Double));
    fLevelCircle := ReallocMemory(fLevelCircle, fCircleSize * SizeOf(Double));

    for i := 0 to fCircleSize -1 do
    begin
      fCircle[i] := 0.0;
      fLevelCircle[i] := 0.0;
    end;
  end;
//------------------------------
  InitVars(SampleRate, Channels);
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
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,Samplerate,Bits,Channels,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

function TDCCompressor.AvgCircle(value: Single): Single;
var
  level: Single;
begin
  fRMSSum := fRMSSum - fCircle[fCirclePos];
  fCircle[fCirclePos] := value*value;
  fRMSSum := fRMSSum + fCircle[fCirclePos];
//++++++++++++++++++++++++++++++
  level := sqrt(fRMSSum / fCircleSize * 2); // factor of 2 because we define the RMS value of full scale sine wave as 0dB
//------------------------------
  fLevelCircle[fCirclePos] := level;
  fCirclePos := (fCirclePos+1) mod (fCircleSize);

  Result := level;
end;

function TDCCompressor.DoCompression(value: Single): single;
begin
//++++++++++++++++++++++++++++++
  fInputLevel := AvgCircle(value);
  if (fInputLevel > fThreshold) then
    begin
      fAboveThrs := true;
      fCurrentGain := fCurrentGain + fAttackFactor * (power(fThreshold / fInputLevel, 1.0 - 1.0 / fRatio) - fCurrentGain);
    end else
    begin
      fAboveThrs := false;
      fCurrentGain := fCurrentGain + fDecayFactor * (1.0 - fCurrentGain);
    end;
  fGainReduction := fGain * fCurrentGain;
  Result := Clip_32F(value * fGainReduction);
//------------------------------
end;

procedure TDCCompressor.DoDSP(Buffer : Pointer; Size : Integer; Samplerate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  NumSamples : integer;
  i : integer;
begin
  Buf8 := PByteArray(Buffer);
  Buf16 := PSmallIntArray(Buffer);
  Buf24 := PInteger24Array(Buffer);
  Buf32 := PFloatArray(Buffer);
  Buf32i := PIntegerArray(Buffer);

  NumSamples := Size div (Bits div 8);// div Channels;
  if fBufferSize < (NumSamples * SizeOf(Double)) then
  begin
    fBufferSize := NumSamples * SizeOf(Double);
    fBuffer := ReallocMemory(fBuffer,fBufferSize);
  end;

  for i := 0 to NumSamples -1 do
  begin
    case Bits of
      8:  fBuffer[i] := (Buf8[i] - 128) / 128;
      16: fBuffer[i] := Buf16[i] / 32768;
      24: fBuffer[i] := Cvt24BitTo32(Buf24[i]) / 8388608;
      32: if Float then fBuffer[i] := Buf32[i];
                   else fBuffer[i] := Buf32i[i] / 2147483648;
    end;
  end;

  for i := 0 to NumSamples -1 do
     fBuffer[i] := DoCompression(fBuffer[i]);

  for i := 0 to NumSamples -1 do
  begin
    case Bits of
      8:  Buf8[i] := Clip_8(Round(fBuffer[i] * 128)) + 128;
      16: Buf16[i] := Clip_16(Round(fBuffer[i] * 32768));
      24: Buf24[i] := Cvt32BitTo24(Clip_24(Round(fBuffer[i] * 8388608)));
      32: if Float then Buf32[i] := fBuffer[i]
                   else Buf32i[i] := Clip_32(Int64(Round(fBuffer[i] * 2147483648)));
    end;
  end;
end;

(*** IDCCompressor ************************************************************)
function TDCCompressor.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCCompressor.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCCompressor.get_AttackTime(out aAttackTime: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aAttackTime := fAttackTime;
end;

function TDCCompressor.set_AttackTime(aAttackTime: Single): HRESULT; stdcall;
begin
  Result := S_OK;
//++++++++++++++++++++++++++++++
  fAttackTime := aAttackTime;
  fAttackFactor := 1.0 - 1.0 / exp(fSamplePeriod / (fAttackTime / (-ln(1.0 - fAchiement))));
//------------------------------
end;

function TDCCompressor.get_DecayTime(out aDecayTime: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aDecayTime := fDecayTime;
end;

function TDCCompressor.set_DecayTime(aDecayTime: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  fDecayTime := aDecayTime;
//++++++++++++++++++++++++++++++
  fDecayFactor := 1.0 - 1.0 / exp(fSamplePeriod / (fDecayTime / (-ln(1.0 - fAchiement))));
//------------------------------
end;

function TDCCompressor.get_ThresholdDB(out aThresholdDB: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aThresholdDB := fThresholdDB;
end;

function TDCCompressor.set_ThresholdDB(aThresholdDB: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  fThresholdDB := aThresholdDB;
end;

function TDCCompressor.get_Ratio(out aRatio: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aRatio := fRatio;
end;

function TDCCompressor.set_Ratio(aRatio: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  fRatio := aRatio;
end;

function TDCCompressor.get_GainDB(out aGainDB: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aGainDB := fGainDB;
end;

function TDCCompressor.set_GainDB(aGainDB: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  fGainDB := aGainDB;
end;

//++++++++++++++++++++++++++++++
function TDCCompressor.get_AboveThrs(out aAboveThrs: Boolean): HRESULT; stdcall;
begin
  Result := S_OK;
  aAboveThrs := fAboveThrs;
end;

function TDCCompressor.get_InputLevel(out aInputLevel: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aInputLevel := fInputLevel;
end;

function TDCCompressor.get_GainReduction(out aGainReduction: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aGainReduction := fGainReduction;
end;
//------------------------------
end.