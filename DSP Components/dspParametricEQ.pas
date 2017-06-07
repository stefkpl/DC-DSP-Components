
    (*********************************************************************
     *  dspParametricEQ.pas                                              *
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
     *  based on Jonas Ekeroot's Document                                *
     *  "Implementing a parametric EQ plug-in in C++ using the           *
     *   multi-platform VST specification"                               *
     *  http://epubl.luth.se/1402-1773/2003/044/LTU-CUPP-03044-SE.pdf    *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)
{
  @abstract(Parametric Equalizer Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(May 23, 2004)
  @lastmod(May 23, 2004)
}

unit dspParametricEQ;

interface

uses
  Classes, dspConst, dspUtils, Math, Forms, dspInterfaces, Windows, SysUtils;

type
  { TDCParametricEQ - Parametric Equalizer Filter. Every Channel can be seperate
    controlled. }
  TDCParametricEQ = class(TComponent, IDCParametricEQ)
  private
    {@exclude}
    fNeedCoeff: Boolean;
    {@exclude}
    fNeedAmp: Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fProcessMessages: Boolean;
    {@exclude}
    fSampleSize: Cardinal;
    {@exclude}
    fSampleRate: integer;
    {@exclude}
    fChannels: integer;
    {@exclude}
    fAlpha: array[0..MaxChannels -1] of Double;
    {@exclude}
    fFrequency: array[0..MaxChannels -1] of Single;
    {@exclude}
    fGainDB: array[0..MaxChannels -1] of Single;
    {@exclude}
    fQ: array[0..MaxChannels -1] of Single;
    {@exclude}
    x: array[0..MaxChannels -1,0..1] of Double;
    {@exclude}
    y: array[0..MaxChannels -1,0..1] of Double;
    {@exclude}
    a: array[0..MaxChannels -1,0..1] of Double;
    {@exclude}
    b: array[0..MaxChannels -1,0..2] of Double;
    {@exclude}
    procedure SetGainDB(aChannel: Byte; Value: Single);
    {@exclude}
    procedure SetFrequency(aChannel: Byte; Value: Single);
    {@exclude}
    procedure SetQ(aChannel: Byte; Value: Single);
    {@exclude}
    function GetGainDB(aChannel: Byte): Single;
    {@exclude}
    function GetFrequency(aChannel: Byte): Single;
    {@exclude}
    function GetQ(aChannel: Byte): Single;
    {@exclude}
    procedure CalcAmplitude;
    {@exclude}
    procedure CalcCoefficients;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCParametricEQ. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Sets the temporary stored Samples to Zero. }
    procedure Flush;
    { Specifys the center Frequency of the Filter. }
    property Frequency[Channel: Byte]: Single read GetFrequency write SetFrequency;
    { Specifys the gain in DB of the Filter. }
    property GainDB[Channel: Byte]: Single read GetGainDB write SetGainDB;
    { Specifys the Q (Bandwidth) of the Filter. }
    property Q[Channel: Byte]: Single read GetQ write SetQ;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT; stdcall;
    {@exclude}
    function set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT; stdcall;
    {@exclude}
    function get_Gain(aChannel : Byte; out aGain: Single): HRESULT; stdcall;
    {@exclude}
    function set_Gain(aChannel : Byte; aGain: Single): HRESULT; stdcall;
    {@exclude}
    function get_Q(aChannel : Byte; out aQ: Single): HRESULT; stdcall;
    {@exclude}
    function set_Q(aChannel : Byte; aQ: Single): HRESULT; stdcall;
  published
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

constructor TDCParametricEQ.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);

  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
  fSeperate := False;
  fEnabled := False;

  fNeedAmp := False;
  fNeedCoeff := True;
  fSampleRate := 0;
  fChannels := 0;

  for i := 0 to MaxChannels -1 do
  begin
    fFrequency[i] := 8000;
    fQ[i] := 1.0;
    fGainDB[i] := 0.0;
  end;
end;

destructor TDCParametricEQ.Destroy;
begin
  inherited Destroy;
end;

procedure TDCParametricEQ.CalcCoefficients;
var
  omega: Double;
  freq: Double;
  i: integer;
begin
  for i := 0 to fChannels -1 do
  begin
    if fFrequency[i] < 1
      then freq := 1
      else if Round(fFrequency[i] * 2) >= Round(fSamplerate)
        then freq := 0//fSamplerate / 2
        else freq := fFrequency[i];

    omega := (2 * PI * freq) / fSamplerate;
    fAlpha[i] := sin(omega) / (2.0 * fQ[i]);
    b[i,1] := -2 * cos(omega);
  end;
  CalcAmplitude;  
end;

procedure TDCParametricEQ.CalcAmplitude;
var
  i: integer;
  amp: Double;
begin
  for i := 0 to fChannels -1 do
  begin
    amp := power(10, fGainDB[i] / 40.0);
    b[i,0] := 1 + (fAlpha[i] * amp);
    b[i,2] := 1 - (fAlpha[i] * amp);
    a[i,0] := 1 / (1 + (fAlpha[i] / amp));
    a[i,1] := 1 - (fAlpha[i] / amp);
  end;
end;

procedure TDCParametricEQ.Flush;
begin
  FillChar(x, MaxChannels * SizeOf(Double) * 2, 0);
  FillChar(y, MaxChannels * SizeOf(Double) * 2, 0);
end;

procedure TDCParametricEQ.Process(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;

  if fNeedCoeff or (fSampleRate <> Samplerate) or (fChannels <> Channels) then
  begin
    fNeedCoeff := False;
    fSampleRate := Samplerate;
    fChannels := Channels;
    CalcCoefficients;
    Flush;
  end;

  if (fNeedAmp) then
  begin
    CalcAmplitude;
    fNeedAmp := False;
  end;

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

procedure TDCParametricEQ.DoDSP(Buffer : Pointer; Size, Samplerate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8: PByte;
  Buf16: PSmallInt;
  Buf24: P24BitSample;
  Buf32: PSingle;
  Buf32i: PInteger;
  c,i,z: integer;
  NumSamples : integer;
  so, si: Single;
begin
  NumSamples := Size div Channels div (Bits div 8);

  case Bits of
    8:
    begin
      for c := 0 to Channels -1 do
      begin
        Buf8 := PByte(@PByteArray(Buffer)[c]);
        for i := 0 to NumSamples -1 do
        begin
          if fSeperate then z := c
                       else z := 0;
          si := (Buf8^ - 128) / 128;

          so := (b[z,0]*si + b[z,1]*x[c,0] + b[z,2]*x[c,1] - b[z,1]*y[c,0] - a[z,1]*y[c,1]) * a[z,0];

          x[c,1] := x[c,0];
          x[c,0] := si;
          y[c,1] := y[c,0];
          y[c,0] := so;

          Buf8^ := Clip_8(Round(so * 128)) + 128;
          inc(Buf8, Channels);
        end;
      end;
    end;
    16:
    begin
      for c := 0 to Channels -1 do
      begin
        Buf16 := PSmallInt(@PSmallIntArray(Buffer)[c]);
        for i := 0 to NumSamples -1 do
        begin
          if fSeperate then z := c
                       else z := 0;
          si := Buf16^ / 32768;
          so := (b[z,0]*si + b[z,1]*x[c,0] + b[z,2]*x[c,1] - b[z,1]*y[c,0] - a[z,1]*y[c,1]) * a[z,0];

          x[c,1] := x[c,0];
          x[c,0] := si;
          y[c,1] := y[c,0];
          y[c,0] := so;

          Buf16^ := Clip_16(Round(so * 32768));
          inc(Buf16, Channels);
        end;
      end;
    end;
    24:
    begin
      for c := 0 to Channels -1 do
      begin
        Buf24 := P24BitSample(@PInteger24Array(Buffer)[c]);
        for i := 0 to NumSamples -1 do
        begin
          if fSeperate then z := c
                       else z := 0;
          si := Cvt24BitTo32(Buf24^) / 8388608;

          so := (b[z,0]*si + b[z,1]*x[c,0] + b[z,2]*x[c,1] - b[z,1]*y[c,0] - a[z,1]*y[c,1]) * a[z,0];

          x[c,1] := x[c,0];
          x[c,0] := si;
          y[c,1] := y[c,0];
          y[c,0] := so;

          Buf24^ := Cvt32BitTo24(Clip_24(Round(so * 8388608)));
          inc(Buf24, Channels);
        end;
      end;
    end;
    32:
    begin
      if Float then
      begin
        for c := 0 to Channels -1 do
        begin
          Buf32 := PSingle(@PFloatArray(Buffer)[c]);
          for i := 0 to NumSamples -1 do
          begin
            if fSeperate then z := c
                         else z := 0;
            si := Buf32^;
            so := (b[z,0]*si + b[z,1]*x[c,0] + b[z,2]*x[c,1] - b[z,1]*y[c,0] - a[z,1]*y[c,1]) * a[z,0];

            x[c,1] := x[c,0];
            x[c,0] := Buf32^;
            y[c,1] := y[c,0];
            y[c,0] := so;

            Buf32^ := Clip_32F(so);
            inc(Buf32, Channels);
          end;
        end;
      end else
      begin
        for c := 0 to Channels -1 do
        begin
          Buf32i := PInteger(@PIntegerArray(Buffer)[c]);
          for i := 0 to NumSamples -1 do
          begin
            if fSeperate then z := c
                         else z := 0;
            si := Buf32i^ / 2147483648;

            so := (b[z,0]*si + b[z,1]*x[c,0] + b[z,2]*x[c,1] - b[z,1]*y[c,0] - a[z,1]*y[c,1]) * a[z,0];

            x[c,1] := x[c,0];
            x[c,0] := si;
            y[c,1] := y[c,0];
            y[c,0] := so;

            Buf32i^ := Clip_32(Round(so * 2147483648));
            inc(Buf32i, Channels);
          end;
        end;
      end;
    end;
  end;
end;

procedure TDCParametricEQ.SetGainDB(aChannel: Byte; Value: Single);
begin
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  fGainDB[aChannel] := Value;
  fNeedAmp := True;
end;

procedure TDCParametricEQ.SetFrequency(aChannel: Byte; Value: Single);
begin
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  fFrequency[aChannel] := Value;
  fNeedCoeff := True;
end;

procedure TDCParametricEQ.SetQ(aChannel: Byte; Value: Single);
begin
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  fQ[aChannel] := Value;
  fNeedCoeff := True;
end;

function TDCParametricEQ.GetGainDB(aChannel: Byte): Single;
begin
  Result := 0;
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  Result := fGainDB[aChannel];
end;

function TDCParametricEQ.GetFrequency(aChannel: Byte): Single;
begin
  Result := 0;
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  Result := fFrequency[aChannel];
end;

function TDCParametricEQ.GetQ(aChannel: Byte): Single;
begin
  Result := 0;
  if not InRange(aChannel,0,MaxChannels -1) then Exit;
  Result := fQ[aChannel];
end;

function TDCParametricEQ.get_Enabled(out aEnabled: BOOL): HRESULT;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCParametricEQ.set_Enabled(aEnabled: BOOL): HRESULT;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCParametricEQ.get_Seperate(out aSeperate: BOOL): HRESULT;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCParametricEQ.set_Seperate(aSeperate: BOOL): HRESULT;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCParametricEQ.get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT;
begin
  Result := S_OK;
  aFrequency := GetFrequency(aChannel);
end;

function TDCParametricEQ.set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT;
begin
  Result := S_OK;
  SetFrequency(aChannel,aFrequency);
end;

function TDCParametricEQ.get_Gain(aChannel : Byte; out aGain: Single): HRESULT;
begin
  Result := S_OK;
  aGain := GetGainDB(aChannel);
end;

function TDCParametricEQ.set_Gain(aChannel : Byte; aGain: Single): HRESULT;
begin
  Result := S_OK;
  SetGainDB(aChannel,aGain);
end;

function TDCParametricEQ.get_Q(aChannel : Byte; out aQ: Single): HRESULT;
begin
  Result := S_OK;
  aQ := GetQ(aChannel);
end;

function TDCParametricEQ.set_Q(aChannel : Byte; aQ: Single): HRESULT;
begin
  Result := S_OK;
  SetQ(aChannel,aQ);
end;

end.
