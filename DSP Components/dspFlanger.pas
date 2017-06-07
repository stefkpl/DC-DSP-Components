
    (*********************************************************************
     *  dspFlanger.pas                                                   *
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
  @abstract(DSP Filter to flange Audiodata.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspFlanger;

interface

uses
  Windows, Classes, dspConst, dspUtils, Forms, Math, dspInterfaces;

type
  { TDCFlanger - Component to flange Audiodata. Every Channel can be seperate
    controlled. }
  TDCFlanger = class(TComponent, IDCFlanger)
  protected
    {@exclude}
    fSampleRate : integer;
    {@exclude}
    fChannels : Byte;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fPosition : array[0..MaxChannels -1] of integer;
    {@exclude}
    fFrequency : array[0..MaxChannels -1] of Single;
    {@exclude}
    fDoFrequency : array[0..MaxChannels -1] of Single;
    {@exclude}
    fSweep : array[0..MaxChannels -1] of Single;
    {@exclude}
    fDelay : array[0..MaxChannels -1] of Single;
    {@exclude}
    fSweepLength : array[0..MaxChannels -1] of integer;
    {@exclude}
    fPhaseInvert : array[0..MaxChannels -1] of Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fPreviousOutput : array[0..9] of PFloatArray;
    {@exclude}
    procedure SetFrequency(Channel : Byte; Frequency : Single);
    {@exclude}
    function GetFrequency(Channel : Byte) : Single;
    {@exclude}
    procedure SetDelay(Channel : Byte; Delay : Single);
    {@exclude}
    function GetDelay(Channel : Byte) : Single;
    {@exclude}
    procedure SetPhaseInvert(Channel : Byte; Invert : Boolean);
    {@exclude}
    function GetPhaseInvert(Channel : Byte) : Boolean;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCFlanger. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean);
    { Sets the temporary stored Samples to Zero. }
    procedure Flush;
    { Call this to initialize the Filter. }
    procedure Init(Channels : Byte; SampleRate : integer; MaxDelayBufferSeconds : Single);
    { Specifys the Flanger Frequency. }
    property Frequency[Channel : Byte] : Single read GetFrequency write SetFrequency;
    { Specifys the Delay Time in Seconds. }
    property Delay[Channel : Byte] : Single read GetDelay write SetDelay;
    { Specifys whether the added Signal should be Inverted. }
    property PhaseInvert[Channel : Byte] : Boolean read GetPhaseInvert write SetPhaseInvert;
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
    function get_Delay(aChannel : Byte; out aDelay: Single): HRESULT; stdcall;
    {@exclude}
    function set_Delay(aChannel : Byte; aDelay: Single): HRESULT; stdcall;
    {@exclude}
    function get_PhaseInvert(aChannel: Byte; out aPhaseInvert: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_PhaseInvert(aChannel: Byte; aPhaseInvert: BOOL): HRESULT; stdcall;
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

uses SysUtils;

constructor TDCFlanger.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);

  for i := 0 to MaxChannels -1 do
  begin
    fPosition[i] := 0;
    fFrequency[i] := 0.125;
    fDoFrequency[i] := fFrequency[i];
    fDelay[i] := 0.005;
    fPhaseInvert[i] := False;
    fPreviousOutput[i] := nil;
  end;

  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCFlanger.Destroy;
var
  i : integer;
begin
  for i := 0 to MaxChannels -1 do
    if fPreviousOutput[i] <> nil then
    begin
      FreeMemory(fPreviousOutput[i]);
      fPreviousOutput[i] := nil;
    end;
  inherited Destroy;
end;

procedure TDCFlanger.Flush;
var
  i : integer;
begin
  if fChannels = 0 then Exit;
  for i := 0 to fChannels -1 do
    if fPreviousOutput[i] <> nil then FillChar(fPreviousOutput[i]^,fSweepLength[i] * SizeOf(Single),0);
end;

procedure TDCFlanger.Init(Channels : Byte; SampleRate : integer; MaxDelayBufferSeconds : Single);
var
  i : integer;
begin
  fSampleRate := SampleRate;
  fChannels := EnsureRange(Channels,0,MaxChannels -1);
  if fChannels = 0 then Exit;
  for i := 0 to fChannels -1 do
  begin
    if fPreviousOutput[i] <> nil then
    begin
      FreeMemory(fPreviousOutput[i]);
      fPreviousOutput[i] := nil;
    end;
    fSweepLength[i] := Round(fDelay[i] * fSampleRate);
    fPosition[i] := fSweepLength[i] div 2;
    fPreviousOutput[i] := ReallocMemory(fPreviousOutput[i],Round(MaxDelayBufferSeconds * SizeOf(Single) * fSampleRate));
  end;
  Flush;
end;

procedure TDCFlanger.SetFrequency(Channel : Byte; Frequency : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fFrequency[Channel] := Frequency;
  if fDoFrequency[Channel] > 0 then fDoFrequency[Channel] := fFrequency[Channel]
                               else fDoFrequency[Channel] := fFrequency[Channel] * -1;
end;

function TDCFlanger.GetFrequency(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fFrequency[Channel];
end;

procedure TDCFlanger.SetDelay(Channel : Byte; Delay : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fDelay[Channel] := Delay;
  fSweepLength[Channel] := Round(fDelay[Channel] * fSampleRate);
  fPosition[Channel] := fSweepLength[Channel] div 2;
end;

function TDCFlanger.GetDelay(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fDelay[Channel];
end;

procedure TDCFlanger.SetPhaseInvert(Channel : Byte; Invert : Boolean);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fPhaseInvert[Channel] := Invert;
end;

function TDCFlanger.GetPhaseInvert(Channel : Byte) : Boolean;
begin
  Result := False;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fPhaseInvert[Channel];
end;

procedure TDCFlanger.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size,Bits,Float);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (Bits div 8) * fChannels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,Bits,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCFlanger.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  lc : integer;
  i, c : integer;
  p1 : integer;
  f  :Single;
  s : integer;
  Pos : integer;
  sl : integer;
  sw : Single;
  inv : integer;
begin
  NumSamples := Size div (Bits div 8) div fChannels;
  Buf8 := PByteArray(Buffer);
  Buf16 := PSmallIntArray(Buffer);
  Buf24 := PInteger24Array(Buffer);
  Buf32i := PIntegerArray(Buffer);
  Buf32 := PFloatArray(Buffer);
  lc := 0;
  p1 := 0;
  f := 0;
  Pos := 0;
  sl := 0;
  inv := 1;

  for i := 0 to NumSamples -1 do
  begin
    for c := 0 to fChannels -1 do
    begin
      if fSeperate then
      begin
        Pos := fPosition[c];
        sw := fSweep[c];
        sl := fSweepLength[c];
        p1 := (Pos + Trunc(sw)) mod sl;
        f := sw - (1.0 * Trunc(sw / 1.0));           
        if fPhaseInvert[c] then inv := -1
                           else inv := 1;
      end else
      if c = 0 then
      begin
        Pos := fPosition[0];
        sw := fSweep[0];
        sl := fSweepLength[0];
        p1 := (Pos + Trunc(sw)) mod sl;
        f := sw - (1.0 * Trunc(sw / 1.0));
        if fPhaseInvert[0] then inv := -1
                           else inv := 1;
      end;

      case Bits of
        8:  lc := Buf8^[i * fChannels + c] - 128;
        16: lc := Buf16^[i * fChannels + c];
        24: lc := Cvt24BitTo32(Buf24^[i * fChannels + c]);
        32: if Float then lc := Round(Buf32^[i * fChannels + c] * 32768)
                     else lc := Buf32i^[i * fChannels + c] div 32768;
      end;

      s := (lc + (Trunc(((1.0-f) * fPreviousOutput[c][p1]) + (f * fPreviousOutput[c][(p1 + 1) mod sl])) * Inv)) div 2;
      fPreviousOutput[c][Pos] := lc;
      case Bits of
        8:  Buf8^[i * fChannels + c] := Clip_8(s) + 128;
        16: Buf16^[i * fChannels + c] := Clip_16(s);
        24: Buf24^[i * fChannels + c] := Cvt32BitTo24(Clip_24(s));
        32: if Float then Buf32^[i * fChannels + c] := s / 32768
                     else Buf32i^[i * fChannels + c] := Clip_32(Int64(s) * 32768);
      end;

      if fSeperate then
      begin
        Inc(fPosition[c]);
        if (fPosition[c] >= fSweepLength[c]) then fPosition[c] := 0;
        fSweep[c] := fSweep[c] + (fDoFrequency[c] / 100);
        if (fSweep[c] < 0) or (fSweep[c] > fSweepLength[c]) then
        begin
          fDoFrequency[c] := -fDoFrequency[c];
          if (fSweep[c] > fSweepLength[c]) then fSweep[c] := fSweepLength[c];
        end;
      end else
      if (c = fChannels -1) then
      begin
        Inc(fPosition[0]);
        if (fPosition[0] >= fSweepLength[0]) then fPosition[0] := 0;
        fSweep[0] := fSweep[0] + (fDoFrequency[0] / (1 / fDelay[0])) ;
        if (fSweep[0] < 0) or (fSweep[0] > fSweepLength[0]) then
        begin
          fDoFrequency[0] := -fDoFrequency[0];
          if (fSweep[0] > fSweepLength[0]) then fSweep[0] := fSweepLength[0];
        end;
      end;
    end;
  end;
end;
(*** IDCFlanger ***************************************************************)
function TDCFlanger.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCFlanger.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCFlanger.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCFlanger.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCFlanger.get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aFrequency := GetFrequency(aChannel);
end;

function TDCFlanger.set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFrequency(aChannel,aFrequency);
end;

function TDCFlanger.get_Delay(aChannel : Byte; out aDelay: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aDelay := GetDelay(aChannel);
end;

function TDCFlanger.set_Delay(aChannel : Byte; aDelay: Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetDelay(aChannel,aDelay);
end;

function TDCFlanger.get_PhaseInvert(aChannel: Byte; out aPhaseInvert: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aPhaseInvert := GetPhaseInvert(aChannel);
end;

function TDCFlanger.set_PhaseInvert(aChannel: Byte; aPhaseInvert: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  SetPhaseInvert(aChannel,aPhaseInvert);
end;

end.

