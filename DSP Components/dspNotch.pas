
    (*********************************************************************
     *  dspNotch.pas                                                     *
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
  @abstract(DSP Filter to Amplify a certain Frequency Range.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspNotch;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math, dspInterfaces, Windows;

type
  { TDCNotch - DSP Component that cuts Frequencys in a Widerange of specified
    Frequency. Each Channel can be seperate Processed with different Cutoff
    Values. }
  TDCNotch = class(TComponent, IDCNotch)
  private
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fPreviousOutput : TPass;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fCutoff : array[0..MaxChannels -1] of Cardinal;
    {@exclude}
    procedure SetCutoff(Channel : Byte; Cutoff : Cardinal);
    {@exclude}
    function GetCutoff(Channel : Byte) : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCNotch. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this to Process an amount of Data. }
    procedure Process(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Specifys the Cutoff Frequency for a specific Channel at which the Cutoff
      will have it´s maximum Amplification. }
    property Cutoff[Channel : Byte] : Cardinal read GetCutoff write SetCutoff;
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
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    {@exclude}
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
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

constructor TDCNotch.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fCutoff[i] := 10000;
  fSeperate := false;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCNotch.Destroy;
begin
  inherited Destroy;
end;

procedure TDCNotch.SetCutoff(Channel : Byte; Cutoff : Cardinal);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fCutoff[Channel] := Cutoff;
end;

function TDCNotch.GetCutoff(Channel : Byte) : Cardinal;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fCutoff[Channel];
end;

procedure TDCNotch.Process(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCNotch.DoDSP(Buffer : Pointer; Size,SampleRate : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i,
  c,
  Cut : integer;
  CurrSample,
  f : Double;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Cut := CutOff[c] else Cut := CutOff[0];
        if Cut < 1 then Cut := 1
        else if Cut > (SampleRate div 2) then Cut := SampleRate div 2;
        f := sin(pi * Cut / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf8^[i * Channels + c] - 128;
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + f * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := f * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf8^[i * Channels + c] := Clip_8(Round(fPreviousOutput[1][c])) + 128;
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Cut := CutOff[c] else Cut := CutOff[0];
        if Cut < 1 then Cut := 1
        else if Cut > (SampleRate div 2) then Cut := SampleRate div 2;
        f := sin(pi * Cut / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Buf16^[i * Channels + c];
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + f * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := f * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf16^[i * Channels + c] := Clip_16(Round(fPreviousOutput[1][c]));
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to (Channels - 1) do
      begin
        if Seperate then Cut := CutOff[c] else Cut := CutOff[0];
        if Cut < 1 then Cut := 1
        else if Cut > (SampleRate div 2) then Cut := SampleRate div 2;
        f := sin(pi * Cut / SampleRate);
        for i := 0 to NumSamples -1 do
        begin
          CurrSample := Cvt24BitTo32(Buf24^[i * Channels + c]);
          fPreviousOutput[0][c] := fPreviousOutput[0][c] + f * fPreviousOutput[1][c];
          fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
          fPreviousOutput[1][c] := f * fPreviousOutput[2][c] + fPreviousOutput[1][c];
          Buf24^[i * Channels + c] := Cvt32BitTo24(Clip_24(Round(fPreviousOutput[1][c])));
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
          if Seperate then Cut := CutOff[c] else Cut := CutOff[0];
          if Cut < 0 then Cut := 0
          else if Cut > (SampleRate div 2) then Cut := SampleRate div 2;
          f := sin(pi * Cut / SampleRate);
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32^[i * Channels + c];
            fPreviousOutput[0][c] := fPreviousOutput[0][c] + f * fPreviousOutput[1][c];
            fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
            fPreviousOutput[1][c] := f * fPreviousOutput[2][c] + fPreviousOutput[1][c];
            Buf32^[i * Channels + c] := fPreviousOutput[1][c];
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to (Channels - 1) do
        begin
          if Seperate then Cut := CutOff[c] else Cut := CutOff[0];
          if Cut < 1 then Cut := 1
          else if Cut > (SampleRate div 2) then Cut := SampleRate div 2;
          f := sin(pi * Cut / SampleRate);
          for i := 0 to NumSamples -1 do
          begin
            CurrSample := Buf32i^[i * Channels + c] / 32768;
            fPreviousOutput[0][c] := fPreviousOutput[0][c] + f * fPreviousOutput[1][c];
            fPreviousOutput[2][c] := CurrSample - fPreviousOutput[0][c] - fPreviousOutput[1][c];
            fPreviousOutput[1][c] := f * fPreviousOutput[2][c] + fPreviousOutput[1][c];
            Buf32i^[i * Channels + c] := Clip_32(Int64(Round(fPreviousOutput[1][c]) * 32768));
          end;
        end;
      end;
    end;
  end;
end;

procedure TDCNotch.Flush;
begin
  FillChar(fPreviousOutput,SizeOf(TPass),0);
end;
(*** IDCNotch *****************************************************************)
function TDCNotch.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCNotch.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCNotch.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCNotch.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCNotch.get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  aCutoff := GetCutoff(aChannel);
end;

function TDCNotch.set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  SetCutoff(aChannel,aCutoff);
end;

end.
