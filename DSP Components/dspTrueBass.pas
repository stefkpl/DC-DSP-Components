
    (*********************************************************************
     *  dspTrueBass.pas                                                  *
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
  @abstract(DSP Filter to Amplify Low Frequencys.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspTrueBass;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math, dspInterfaces, Windows;

type

  { TDCTrueBass - DSP Component to Amplify Low Frequencys of Audio Data. The
    Frequencyrange can be adjusted. }
  TDCTrueBass = class(TComponent, IDCTrueBass)
  private
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fPreviousOutput : TPass;
    {@exclude}
    fVolume : array[0..MaxChannels -1] of Word;
    {@exclude}
    fSampleRate : integer;
    {@exclude}
    fFrequency : integer;
    {@exclude}
    fWorkFreq : Single;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    procedure SetVolume(Channel : Byte; Volume : Word);
    {@exclude}
    function GetVolume(Channel : Byte) : Word;
    {@exclude}
    procedure SetFrequency(Frequency : integer);
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { TDCTrueBass constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCTrueBass destructor }
    destructor Destroy; override;
    { Initializes the Filter. }
    procedure Init(SampleRate : integer);
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Resets previously stored Samples. Use this on Seek. }
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
    function get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
    {@exclude}
    function set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
    {@exclude}
    function get_Frequency(out aFrequency: integer): HRESULT; stdcall;
    {@exclude}
    function set_Frequency(aFrequency: integer): HRESULT; stdcall;
    { Sets the Amplification for a Channel. Default Value is 0, which means
      that no Amplification occours. The Value shouldn´t go over 10000. If
      Seperate is True, then every Channel will be Amplified by its own
      Channel Amplification Value. If Seperate is False, then every Channel is
      Amplified with the Value of Channel 0. }
    property Volume[Channel : Byte] : Word read GetVolume write SetVolume;
  published
    { Enables or Disables seperate Amplification. If Enabled every Channel will
      be Amplified by it´s own Amplification Value. If Disabled every Channel
      will be Amplified with the Amplification Value of Channel 0. }
    property Seperate : Boolean read fSeperate write fSeperate;
    { Enables or Disables the Filter. }
    property Enabled : Boolean read fEnabled write fEnabled;
    { Specifys the Frequency Range that will be used to Amplify. Range is 0 to
      Frequency. }
    property Frequency : integer read fFrequency write SetFrequency;
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

constructor TDCTrueBass.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fVolume[i] := 0;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
  fSampleRate := 44100;
  fFrequency := 200;
  SetFrequency(fFrequency);
end;

destructor TDCTrueBass.Destroy;
begin
  inherited Destroy;
end;

procedure TDCTrueBass.SetVolume(Channel : Byte; Volume : Word);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fVolume[Channel] := Volume;
end;

function TDCTrueBass.GetVolume(Channel : Byte) : Word;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fVolume[Channel];
end;

procedure TDCTrueBass.Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if SampleRate <> fSampleRate then Init(SampleRate);
  if fSampleSize = 0 then
  begin
    DoDSP(Buffer,Size,Bits,Channels,Float);
  end else
  begin
    SplitBuffer := Buffer;
    SplitSize := fSampleSize * (Bits div 8) * Channels;
    SizeLeft := Size;
    while SizeLeft > 0 do
    begin
      if SizeLeft > SplitSize then CurrentSize := SplitSize
                              else CurrentSize := SizeLeft;
      DoDSP(@SplitBuffer[Size - SizeLeft],CurrentSize,Bits,Channels,Float);
      if fProcessMessages then Application.ProcessMessages;
      dec(SizeLeft,SplitSize);
    end;
  end;
end;

procedure TDCTrueBass.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  NumSamples : integer;
  i,
  c : integer;
  CurrSample,
  Vol : Double;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Vol := Volume[c] / 400 else Vol := Volume[0] / 400;
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf8^[i * Channels + c] - 128;
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + fWorkFreq * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := fWorkFreq * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf8^[i * Channels + c] := Clip_8(Round(fPreviousOutput[0][c] * Vol) + Buf8^[i * Channels + c] - 128) + 128;
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Vol := Volume[c] / 400 else Vol := Volume[0] / 400;
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf16^[i * Channels + c];
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + fWorkFreq * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := fWorkFreq * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf16^[i * Channels + c] := Clip_16(Round(fPreviousOutput[0][c] * Vol) + Buf16^[i * Channels + c]);
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Vol := Volume[c] / 400 else Vol := Volume[0] / 400;
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Cvt24BitTo32(Buf24^[i * Channels + c]);
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + fWorkFreq * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := fWorkFreq * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf24^[i * Channels + c] := Cvt32BitTo24(Clip_24(Round(fPreviousOutput[0][c] * Vol) + Cvt24BitTo32(Buf24^[i * Channels + c])));
        end;
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for c := 0 to (Channels - 1) do
        begin
          if Seperate then Vol := Volume[c] / 400 else Vol := Volume[0] / 400;
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32^[i * Channels + c];
            fPreviousOutput[0][c] := fPreviousOutput[0][c] + fWorkFreq * fPreviousOutput[1][c];
            fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
            fPreviousOutput[1][c] := fWorkFreq * fPreviousOutput[2][c] + fPreviousOutput[1][c];
            Buf32^[i * Channels + c] := fPreviousOutput[0][c] * Vol + Buf32^[i * Channels + c];
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to (Channels - 1) do
        begin
          if Seperate then Vol := Volume[c] / 400 else Vol := Volume[0] / 400;
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32i^[i * Channels + c] / 32768;
            fPreviousOutput[0][c] := fPreviousOutput[0][c] + fWorkFreq * fPreviousOutput[1][c];
            fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
            fPreviousOutput[1][c] := fWorkFreq * fPreviousOutput[2][c] + fPreviousOutput[1][c];
            Buf32i^[i * Channels + c] := Clip_32(Int64(Round(fPreviousOutput[0][c] * Vol) * 32768) + Buf32i^[i * Channels + c]);
          end;
        end;
      end;
    end;
  end;
end;

procedure TDCTrueBass.SetFrequency(Frequency : integer);
begin
  if Frequency > (fSampleRate div 2) then Frequency := fSampleRate div 2
  else if Frequency < 0 then Frequency := 0;
  fFrequency := Frequency;
  fWorkFreq := sin(PI * fFrequency / fSampleRate);
end;

procedure TDCTrueBass.Init(SampleRate : integer);
begin
  if SampleRate < 1 then SampleRate := 1;
  fSampleRate := SampleRate;
  SetFrequency(fFrequency);
end;

procedure TDCTrueBass.Flush;
begin
  FillChar(fPreviousOutput,SizeOf(TPass),0);
end;
(*** IDCTrueBass **************************************************************)
function TDCTrueBass.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCTrueBass.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCTrueBass.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCTrueBass.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCTrueBass.get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
begin
  Result := S_OK;
  aVolume := GetVolume(aChannel);
end;

function TDCTrueBass.set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
begin
  Result := S_OK;
  SetVolume(aChannel,aVolume);
end;

function TDCTrueBass.get_Frequency(out aFrequency: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  aFrequency := fFrequency;
end;

function TDCTrueBass.set_Frequency(aFrequency: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFrequency(aFrequency);
end;

end.
