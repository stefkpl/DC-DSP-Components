
    (*********************************************************************
     *  dspBandpass.pas                                                  *
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
  @abstract(DSP Filter that cuts a certain Frequency Range.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspBandpass;

interface

uses
  Classes, dspConst, dspUtils, Math, Forms, dspInterfaces, Windows;

type

  { TDCBandpass - DSP Component to cutoff a certain Frequency Range. Each Channel
    can seperate cutoff the Frequency range. }
  TDCBandpass = class(TComponent, IDCBandPass)
  private
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fPreviousOutputLow : TPass;
    {@exclude}
    fPreviousOutputHigh : TPass;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fCutoffLow : array[0..MaxChannels -1] of Single;
    {@exclude}
    fCutoffHigh : array[0..MaxChannels -1] of Single;
    {@exclude}
    procedure SetCutoffLow(Channel : Byte; Cutoff : Single);
    {@exclude}
    function GetCutoffLow(Channel : Byte) : Single;
    {@exclude}
    procedure SetCutoffHigh(Channel : Byte; Cutoff : Single);
    {@exclude}
    function GetCutoffHigh(Channel : Byte) : Single;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCBandpass. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this to Process an amount of Data. }
    procedure Process(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Specifys the Low Cutoff Frequency for a specific Channel. }
    property CutoffLow[Channel : Byte] : Single read GetCutoffLow write SetCutoffLow;
    { Specifys the HighCutoff Frequency for a specific Channel. }
    property CutoffHigh[Channel : Byte] : Single read GetCutoffHigh write SetCutoffHigh;
    { Resets the temporary stored Samples that has been Processed last. Use this
      if a seek occours. }
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
    function get_CutoffLow(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    {@exclude}
    function set_CutoffLow(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
    {@exclude}
    function get_CutoffHigh(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    {@exclude}
    function set_CutoffHigh(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
  published
    { Enables or Disables seperate Cutoff. If Enabled every Channel will
      be Processed by it´s own Cutoff Value. If Disabled every Channel
      will be Processed with the Cutoff Value of Channel 0. }
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

constructor TDCBandpass.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fCutoffLow[i] := 10;
  for i := 0 to MaxChannels -1 do fCutoffHigh[i] := 1000;
  fSeperate := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCBandpass.Destroy;
begin
  inherited Destroy;
end;

procedure TDCBandpass.SetCutoffLow(Channel : Byte; Cutoff : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fCutoffLow[Channel] := Cutoff;
end;

function TDCBandpass.GetCutoffLow(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fCutoffLow[Channel];
end;

procedure TDCBandpass.SetCutoffHigh(Channel : Byte; Cutoff : Single);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fCutoffHigh[Channel] := Cutoff;
end;

function TDCBandpass.GetCutoffHigh(Channel : Byte) : Single;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fCutoffHigh[Channel];
end;

procedure TDCBandpass.Process(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCBandpass.DoDSP(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i,
  c : integer;
  CutLow,
  CutHigh,
  CurrSample,
  fl, fh : Single;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then
        begin
          CutLow := fCutOffLow[c];
          CutHigh := fCutOffHigh[c];
        end else
        begin
          CutLow := fCutOffLow[0];
          Cuthigh := fCutOffHigh[0];
        end;
        if CutLow < 1 then CutLow := 1
        else if CutLow > (SampleRate div 2) then CutLow := SampleRate / 2;
        if CutHigh < 1 then CutHigh := 1
        else if CutHigh > (SampleRate div 2) then CutHigh := SampleRate / 2;
        fl := sin(pi * CutHigh / SampleRate);
        fh := sin(pi * CutLow / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf8^[i * Channels + c] - 128;
          fPreviousOutputLow[0][c] := fPreviousOutputLow[0][c] + fl * fPreviousOutputLow[1][c];
          fPreviousOutputLow[2][c] := CurrSample - fPreviousOutputLow[0][c] - fPreviousOutputLow[1][c];
          fPreviousOutputLow[1][c] := fl * fPreviousOutputLow[2][c] + fPreviousOutputLow[1][c];
          fPreviousOutputHigh[0][c] := fPreviousOutputHigh[0][c] + fh * fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[2][c] := fPreviousOutputLow[0][c] - fPreviousOutputHigh[0][c] - fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[1][c] := fh * fPreviousOutputHigh[2][c] + fPreviousOutputHigh[1][c];
          Buf8^[i * Channels + c] := Clip_8(Round(fPreviousOutputHigh[2][c])) + 128;
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then
        begin
          CutLow := fCutOffLow[c];
          CutHigh := fCutOffHigh[c];
        end else
        begin
          CutLow := fCutOffLow[0];
          Cuthigh := fCutOffHigh[0];
        end;
        if CutLow < 1 then CutLow := 1
        else if CutLow > (SampleRate div 2) then CutLow := SampleRate / 2;
        if CutHigh < 1 then CutHigh := 1
        else if CutHigh > (SampleRate div 2) then CutHigh := SampleRate / 2;
        fl := sin(pi * CutHigh / SampleRate);
        fh := sin(pi * CutLow / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf16^[i * Channels + c];
          fPreviousOutputLow[0][c] := fPreviousOutputLow[0][c] + fl * fPreviousOutputLow[1][c];
          fPreviousOutputLow[2][c] := CurrSample - fPreviousOutputLow[0][c] - fPreviousOutputLow[1][c];
          fPreviousOutputLow[1][c] := fl * fPreviousOutputLow[2][c] + fPreviousOutputLow[1][c];
          fPreviousOutputHigh[0][c] := fPreviousOutputHigh[0][c] + fh * fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[2][c] := fPreviousOutputLow[0][c] - fPreviousOutputHigh[0][c] - fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[1][c] := fh * fPreviousOutputHigh[2][c] + fPreviousOutputHigh[1][c];
          Buf16^[i * Channels + c] := Clip_16(Round(fPreviousOutputHigh[2][c]));
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then
        begin
          CutLow := fCutOffLow[c];
          CutHigh := fCutOffHigh[c];
        end else
        begin
          CutLow := fCutOffLow[0];
          Cuthigh := fCutOffHigh[0];
        end;
        if CutLow < 1 then CutLow := 1
        else if CutLow > (SampleRate div 2) then CutLow := SampleRate / 2;
        if CutHigh < 1 then CutHigh := 1
        else if CutHigh > (SampleRate div 2) then CutHigh := SampleRate / 2;
        fl := sin(pi * CutHigh / SampleRate);
        fh := sin(pi * CutLow / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Cvt24BitTo32(Buf24^[i * Channels + c]);
          fPreviousOutputLow[0][c] := fPreviousOutputLow[0][c] + fl * fPreviousOutputLow[1][c];
          fPreviousOutputLow[2][c] := CurrSample - fPreviousOutputLow[0][c] - fPreviousOutputLow[1][c];
          fPreviousOutputLow[1][c] := fl * fPreviousOutputLow[2][c] + fPreviousOutputLow[1][c];
          fPreviousOutputHigh[0][c] := fPreviousOutputHigh[0][c] + fh * fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[2][c] := fPreviousOutputLow[0][c] - fPreviousOutputHigh[0][c] - fPreviousOutputHigh[1][c];
          fPreviousOutputHigh[1][c] := fh * fPreviousOutputHigh[2][c] + fPreviousOutputHigh[1][c];
          Buf24^[i * Channels + c] := Cvt32BitTo24(Clip_24(Round(fPreviousOutputHigh[2][c])));
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
          if Seperate then
          begin
            CutLow := fCutOffLow[c];
            CutHigh := fCutOffHigh[c];
          end else
          begin
            CutLow := fCutOffLow[0];
            Cuthigh := fCutOffHigh[0];
          end;
          if CutLow < 0 then CutLow := 0
          else if CutLow > (SampleRate div 2) then CutLow := SampleRate / 2;
          if CutHigh < 1 then CutHigh := 1
          else if CutHigh > (SampleRate div 2) then CutHigh := SampleRate / 2;
          fl := sin(pi * CutHigh / SampleRate);
          fh := sin(pi * CutLow / SampleRate);
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32^[i * Channels + c];
            fPreviousOutputLow[0][c] := fPreviousOutputLow[0][c] + fl * fPreviousOutputLow[1][c];
            fPreviousOutputLow[2][c] := CurrSample - fPreviousOutputLow[0][c] - fPreviousOutputLow[1][c];
            fPreviousOutputLow[1][c] := fl * fPreviousOutputLow[2][c] + fPreviousOutputLow[1][c];
            fPreviousOutputHigh[0][c] := fPreviousOutputHigh[0][c] + fh * fPreviousOutputHigh[1][c];
            fPreviousOutputHigh[2][c] := fPreviousOutputLow[0][c] - fPreviousOutputHigh[0][c] - fPreviousOutputHigh[1][c];
            fPreviousOutputHigh[1][c] := fh * fPreviousOutputHigh[2][c] + fPreviousOutputHigh[1][c];
            Buf32^[i * Channels + c] := fPreviousOutputHigh[2][c];
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to (Channels - 1) do
        begin
          if Seperate then
          begin
            CutLow := fCutOffLow[c];
            CutHigh := fCutOffHigh[c];
          end else
          begin
            CutLow := fCutOffLow[0];
            Cuthigh := fCutOffHigh[0];
          end;
          if CutLow < 1 then CutLow := 1
          else if CutLow > (SampleRate div 2) then CutLow := SampleRate / 2;
          if CutHigh < 1 then CutHigh := 1
          else if CutHigh > (SampleRate div 2) then CutHigh := SampleRate / 2;
          fl := sin(pi * CutHigh / SampleRate);
          fh := sin(pi * CutLow / SampleRate);
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32i^[i * Channels + c] / 32768;
            fPreviousOutputLow[0][c] := fPreviousOutputLow[0][c] + fl * fPreviousOutputLow[1][c];
            fPreviousOutputLow[2][c] := CurrSample - fPreviousOutputLow[0][c] - fPreviousOutputLow[1][c];
            fPreviousOutputLow[1][c] := fl * fPreviousOutputLow[2][c] + fPreviousOutputLow[1][c];
            fPreviousOutputHigh[0][c] := fPreviousOutputHigh[0][c] + fh * fPreviousOutputHigh[1][c];
            fPreviousOutputHigh[2][c] := fPreviousOutputLow[0][c] - fPreviousOutputHigh[0][c] - fPreviousOutputHigh[1][c];
            fPreviousOutputHigh[1][c] := fh * fPreviousOutputHigh[2][c] + fPreviousOutputHigh[1][c];
            Buf32i^[i * Channels + c] := Clip_32(Int64(Round(fPreviousOutputHigh[2][c]) * 32768));
          end;
        end;
      end;
    end;
  end;
end;

procedure TDCBandpass.Flush;
begin
  FillChar(fPreviousOutputLow,SizeOf(TPass),0);
  FillChar(fPreviousOutputHigh,SizeOf(TPass),0);
end;
(*** IDCBandPass **************************************************************)
function TDCBandpass.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCBandpass.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCBandpass.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCBandpass.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCBandpass.get_CutoffLow(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aCutoff := GetCutoffLow(aChannel);
end;

function TDCBandpass.set_CutoffLow(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetCutoffLow(aChannel,aCutoff);
end;

function TDCBandpass.get_CutoffHigh(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  aCutoff := GetCutoffHigh(aChannel);
end;

function TDCBandpass.set_CutoffHigh(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
begin
  Result := S_OK;
  SetCutoffHigh(aChannel,aCutoff);
end;

end.
