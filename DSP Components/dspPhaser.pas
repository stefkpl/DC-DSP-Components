
    (*********************************************************************
     *  dspPhaser.pas                                                    *
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
  @abstract(DSP Filter for phasing Audio Data.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspPhaser;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math, dspInterfaces, Windows;

type
  { TDCPhaser - Class for phasing Amplify Audiodata. Phasing can be done seperate
    on each Channel. }
  TDCPhaser = class(TComponent, IDCPhaser)
  protected
    {@exclude}
    fPreviousOutput : array[0..MaxChannels -1] of Single;
    {@exclude}
    fSkipCount : array[0..MaxChannels -1] of Cardinal;
    {@exclude}
    fOldOutput :  array[0..MaxChannels -1] of Array[0..255] of Single;
    {@exclude}
    fLastGain : array[0..MaxChannels -1] of Single;
    {@exclude}
    fStartPhase : array[0..MaxChannels -1] of Single;
    {@exclude}
    fFrequency : array[0..MaxChannels -1] of Single;
    {@exclude}
    fFeedback : array[0..MaxChannels -1] of Byte;
    {@exclude}
    fDepth : array[0..MaxChannels -1] of Byte;
    {@exclude}
    fStages : array[0..MaxChannels -1] of Byte;
    {@exclude}
    fDryWetRatio : array[0..MaxChannels -1] of Byte;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    procedure SetDryWetRatio(Channel : Byte; Ratio : Byte);
    {@exclude}
    function  GetDryWetRatio(Channel : Byte) : Byte;
    {@exclude}
    procedure SetFeedback(Channel : Byte; Feedback : Byte);
    {@exclude}
    function  GetFeedback(Channel : Byte) : Byte;
    {@exclude}
    procedure SetStages(Channel : Byte; Stages : Byte);
    {@exclude}
    function  GetStages(Channel : Byte) : Byte;
    {@exclude}
    procedure SetDepth(Channel : Byte; Depth : Byte);
    {@exclude}
    function  GetDepth(Channel : Byte) : Byte;
    {@exclude}
    procedure SetStartPhase(Channel : Byte; Start : Single);
    {@exclude}
    function  GetStartPhase(Channel : Byte) : Single;
    {@exclude}
    procedure SetFrequency(Channel : Byte; Freq : Single);
    {@exclude}
    function  GetFrequency(Channel : Byte) : Single;
  public
    { TDCPhaser Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCPhaser Destructor }
    destructor Destroy; override;
    { Call this to Process an amount of Data. }
    procedure Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Resets the temporary stored Samples that has been Processed last. Use this
      if a seek occours. }
    procedure Flush;
    { Sets Dry-Wet Mix Ratio. 0 = Dry, 128 = dry=wet, 255 = wet. }
    property DryWetRatio[Channel : Byte] : Byte read GetDryWetRatio write SetDryWetRatio;
    { Sets Phaser FeedBack. 0 = no feedback, 100 = 100% Feedback, -100 = -100%
      FeedBack. }
    property Feedback[Channel : Byte] : Byte read GetFeedback write SetFeedback;
    { Sets Phaser stages. Recomended from 2 to 24. }
    property Stages[Channel : Byte] : Byte read GetStages write SetStages;
    { Sets Phaser depth.  0 = no depth, 255 = max depth. }
    property Depth[Channel : Byte] : Byte read GetDepth write SetDepth;
    { Sets Phaser's LFO Startphase in Radians. Needed for stereo Phasers. }
    property StartPhase[Channel : Byte] : Single read GetStartPhase write SetStartPhase;
    { Sets Phaser's LFO frequency. }
    property Frequency[Channel : Byte] : Single read GetFrequency write SetFrequency;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_DryWetRatio(aChannel : Byte; out aDryWetRatio : Byte): HRESULT; stdcall;
    {@exclude}
    function set_DryWetRatio(aChannel : Byte; aDryWetRatio : Byte): HRESULT; stdcall;
    {@exclude}
    function get_Feedback(aChannel : Byte; out aFeedback : Byte): HRESULT; stdcall;
    {@exclude}
    function set_Feedback(aChannel : Byte; aFeedback : Byte): HRESULT; stdcall;
    {@exclude}
    function get_Stages(aChannel : Byte; out aStages : Byte): HRESULT; stdcall;
    {@exclude}
    function set_Stages(aChannel : Byte; aStages : Byte): HRESULT; stdcall;
    {@exclude}
    function get_Depth(aChannel : Byte; out aDepth : Byte): HRESULT; stdcall;
    {@exclude}
    function set_Depth(aChannel : Byte; aDepth : Byte): HRESULT; stdcall;
    {@exclude}
    function get_StartPhase(aChannel : Byte; out aStartPhase : Single): HRESULT; stdcall;
    {@exclude}
    function set_StartPhase(aChannel : Byte; aStartPhase : Single): HRESULT; stdcall;
    {@exclude}
    function get_Frequency(aChannel : Byte; out aFrequency : Single): HRESULT; stdcall;
    {@exclude}
    function set_Frequency(aChannel : Byte; aFrequency : Single): HRESULT; stdcall;
  published
    { Enables or Disables seperate phasing. If Enabled every Channel will be
      phased by it´s own Phasing Value. If Disabled every Channel will be phased
      with the Phasing Value of Channel 0. }
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

constructor TDCPhaser.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
  for i := 0 to MaxChannels -1 do fStages[i] := 24;
  for i := 0 to MaxChannels -1 do fDryWetRatio[i] := 128;
  for i := 0 to MaxChannels -1 do fDepth[i] := 128;
  for i := 0 to MaxChannels -1 do fStartPhase[i] := 34;
  for i := 0 to MaxChannels -1 do fFrequency[i] := 1.0;
  for i := 0 to MaxChannels -1 do fFeedback[i] := 40;
  Flush;
end;

destructor TDCPhaser.Destroy;
begin
  inherited Destroy;
end;

procedure TDCPhaser.Process(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCPhaser.DoDSP(Buffer : Pointer; Size : Integer; SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i, c, z, stages,drywet : integer;
  s1,s2, m, lfoskip,phase,depth,tmp : Single;
  fb : Byte;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  s1 := 0;
  Buf8 := nil;
  Buf16 := nil;
  Buf24 := nil;
  Buf32 := nil;
  Buf32i := nil;

  case Bits of
    8:  Buf8  := PByteArray(Buffer);
    16: Buf16 := PSmallIntArray(Buffer);
    24: Buf24 := PInteger24Array(Buffer);
    32:
    begin
      if Float then Buf32 := PFloatArray(Buffer)
               else Buf32i := PIntegerArray(Buffer);
    end;
  end;

  for c := 0 to Channels -1 do
  begin

    if fSeperate then
    begin
      drywet := fDryWetRatio[c];
      fb := fFeedback[c];
      stages := fStages[c];
      depth := fDepth[c];
      phase := fStartPhase[c];
      lfoskip := 2 * pi * fFrequency[c] / SampleRate;
    end else
    begin
      drywet := fDryWetRatio[0];
      fb := fFeedback[0];
      stages := fStages[0];
      depth := fDepth[0];
      phase := fStartPhase[0];
      lfoskip := 2 * pi * fFrequency[0] / SampleRate;
    end;

    for i := 0 to NumSamples -1 do
    begin
      case Bits of
        8:  s1 := (Buf8^[i * Channels + c] - 128) * 256;
        16: s1 := Buf16^[i * Channels + c];
        24: s1 := Cvt24BitTo32(Buf24^[i * Channels + c]) / 256;
        32:
        begin
          if Float then s1 := Buf32^[i * Channels + c]
                   else s1 := Buf32i^[i * Channels + c] / 32768;
        end;
      end;

      m := s1 + fPreviousOutput[c] * fb / 100;
      if ((fSkipCount[c] mod PhaserLFOSkipSamples) = 0) then
      begin
         fLastGain[c] := (1 + cos(fSkipCount[c] * lfoskip + phase)) / 2;
         fLastGain[c] := (exp(fLastGain[c] * PhaserLFOShape) - 1) / (exp(PhaserLFOShape)-1);
         fLastGain[c] := 1 - fLastGain[c] / 255 * depth;
      end;
      inc(fSkipCount[c]);
      for z := 0 to stages -1 do
      begin
         tmp := fOldOutput[c,z];
         fOldOutput[c,z] := fLastGain[c] * tmp + m;
         m := tmp - fLastGain[c] * fOldOutput[c,z];
      end;
      fPreviousOutput[c] := m;
      s2 := (m * drywet + s1 * (255 - drywet)) / 255;
      
      case Bits of
        8:  Buf8^[i * Channels + c] := clip_8(round(s2 / 256)) + 128;
        16: Buf16^[i * Channels + c] := clip_16(round(s2));
        24: Buf24^[i * Channels + c] := Cvt32BitTo24(clip_24(round(s2 * 256)));
        32:
        begin
          if Float then Buf32^[i * Channels + c] := s2
                   else Buf32i^[i * Channels + c] := Clip_32(Int64(Round(s2 * 32768)));
        end;
      end;
    end;
  end;
end;

procedure TDCPhaser.Flush;
begin
  FillChar(fLastGain,MaxChannels * SizeOf(Single),0);
  FillChar(fPreviousOutput,MaxChannels * SizeOf(Single),0);
  FillChar(fOldOutput,MaxChannels * SizeOf(Single) * 256,0);
  FillChar(fSkipCount,MaxChannels * SizeOf(Cardinal),0);  
end;

procedure TDCPhaser.SetDryWetRatio(Channel : Byte; Ratio : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fDryWetRatio[Channel] := Ratio;
end;

function TDCPhaser.GetDryWetRatio(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fDryWetRatio[Channel];
end;

procedure TDCPhaser.SetFeedback(Channel : Byte; Feedback : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fFeedback[Channel] := Feedback;
end;

function TDCPhaser.GetFeedback(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fFeedback[Channel];
end;

procedure TDCPhaser.SetStages(Channel : Byte; Stages : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fStages[Channel] := Stages;
end;

function TDCPhaser.GetStages(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fStages[Channel];
end;

procedure TDCPhaser.SetDepth(Channel : Byte; Depth : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fDepth[Channel] := Depth;
end;

function TDCPhaser.GetDepth(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fDepth[Channel];
end;

procedure TDCPhaser.SetStartPhase(Channel : Byte; Start : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fStartPhase[Channel] := Start;
end;

function TDCPhaser.GetStartPhase(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fStartPhase[Channel];
end;

procedure TDCPhaser.SetFrequency(Channel : Byte; Freq : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fFrequency[Channel] := Freq;
end;

function TDCPhaser.GetFrequency(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fFrequency[Channel];
end;
(*** IDCPhaser ****************************************************************)
function TDCPhaser.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCPhaser.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCPhaser.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCPhaser.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCPhaser.get_DryWetRatio(aChannel : Byte; out aDryWetRatio : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aDryWetRatio := GetDryWetRatio(aChannel);
end;

function TDCPhaser.set_DryWetRatio(aChannel : Byte; aDryWetRatio : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetDryWetRatio(aChannel,aDryWetRatio);
end;

function TDCPhaser.get_Feedback(aChannel : Byte; out aFeedback : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aFeedback := GetFeedback(aChannel);
end;

function TDCPhaser.set_Feedback(aChannel : Byte; aFeedback : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFeedback(aChannel,aFeedback);
end;

function TDCPhaser.get_Stages(aChannel : Byte; out aStages : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aStages := GetStages(aChannel);
end;

function TDCPhaser.set_Stages(aChannel : Byte; aStages : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetStages(aChannel,aStages);
end;

function TDCPhaser.get_Depth(aChannel : Byte; out aDepth : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aDepth := GetDepth(aChannel);
end;

function TDCPhaser.set_Depth(aChannel : Byte; aDepth : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetDepth(aChannel,aDepth);
end;

function TDCPhaser.get_StartPhase(aChannel : Byte; out aStartPhase : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aStartPhase := GetStartPhase(aChannel);
end;

function TDCPhaser.set_StartPhase(aChannel : Byte; aStartPhase : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetStartPhase(aChannel,aStartPhase);
end;

function TDCPhaser.get_Frequency(aChannel : Byte; out aFrequency : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aFrequency := GetFrequency(aChannel);
end;

function TDCPhaser.set_Frequency(aChannel : Byte; aFrequency : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFrequency(aChannel,aFrequency);
end;

end.
