
    (*********************************************************************
     *  dspPitchShift.pas                                                *
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
  @abstract(DSP Filter to increase or decrease the Pitch.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspPitchShift;

interface

uses
  Windows, Classes, dspConst, dspUtils, Forms, dspInterfaces;

type
  { TDCPitchShift - Pitch Shift Filter to increase Speed and Pitch of Audio
    Data. }
  TDCPitchShift = class(TComponent, IDCPitchShift)
  protected
    {@exclude}
    fCurrentSize : integer;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : integer;
    {@exclude}
    fPitchShiftBuffer : Pointer;
    {@exclude}
    fPitch : Cardinal;
    {@exclude}
    function DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
  public
    { TDCPitchShift Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCPitchShift Destructor }
    destructor Destroy; override;
    { Call this to Process an amount of Data. }
    function Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Pitch(out aPitch: Cardinal): HRESULT; stdcall;
    {@exclude}
    function set_Pitch(aPitch: Cardinal): HRESULT; stdcall;
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
    property SampleSize : integer read fSampleSize write fSampleSize;
    { Specifys the Pitch. Default Value is 1000, which means that no Pitch is
      done. A Value of 2000 increases the Playback by 200% and so on. }
    property Pitch : Cardinal read fPitch write fPitch;
  end;

implementation

uses SysUtils;

constructor TDCPitchShift.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPitch := 1000;
  fPitchShiftBuffer := AllocMem(2);
  fCurrentSize := 0;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCPitchShift.Destroy;
begin
  if Assigned(fPitchShiftBuffer) then FreeMemory(fPitchShiftBuffer);
  inherited Destroy;
end;

function TDCPitchShift.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
begin
  Result := Size;
  if not fEnabled or (fPitch = 10000) then Exit;
  if fCurrentSize < Size then
  begin
    fPitchShiftBuffer := ReallocMemory(fPitchShiftBuffer,Size);
    fCurrentSize := Size;
  end;
  Move(Buffer^,fPitchShiftBuffer^,Size);
  Result := DoDSP(Buffer,Size,Bits,Channels,Float);
end;

function TDCPitchShift.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
var
  TBuf8,
  Buf8  : PByteArray;
  TBuf16,
  Buf16 : PSmallIntArray;
  TBuf24,
  Buf24 : PInteger24Array;
  TBuf32,
  Buf32 : PFloatArray;
  NumSamples : integer;
  i,c : integer;
  j,pPitch : Single;
  NewSize : integer;
begin
  if fPitch < 1 then pPitch := 1 else pPitch := fPitch;
  pPitch := pPitch / 1000;
  if pPitch = 1 then
  begin
    Result := Size;
    Exit;
  end;
  NumSamples := Size div (Bits div 8) div Channels;
  NewSize := Round(NumSamples/pPitch);
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      TBuf8 := PByteArray(fPitchShiftBuffer);
      for c := 0 to Channels -1 do
      begin
        j := 0;
        for i := 0 to NewSize do
        begin
          Buf8^[i * Channels + c] := TBuf8^[Trunc(j) * Channels + c];
          j := j + pPitch;
          if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      TBuf16 := PSmallIntArray(fPitchShiftBuffer);
      for c := 0 to Channels -1 do
      begin
        j := 0;
        for i := 0 to NewSize do
        begin
          Buf16^[i * Channels + c] := TBuf16^[Trunc(j) * Channels + c];
          j := j + pPitch;
          if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      TBuf24 := PInteger24Array(fPitchShiftBuffer);
      for c := 0 to Channels -1 do
      begin
        j := 0;
        for i := 0 to NewSize do
        begin
          Buf24^[i * Channels + c] := TBuf24^[Trunc(j) * Channels + c];
          j := j + pPitch;
          if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
        end;
      end;
    end;
    32:
    begin
      Buf32 := PFloatArray(Buffer);
      TBuf32 := PFloatArray(fPitchShiftBuffer);
      for c := 0 to Channels -1 do
      begin
        j := 0;
        for i := 0 to NewSize do
        begin
          Buf32^[i * Channels + c] := TBuf32^[Trunc(j) * Channels + c];
          j := j + pPitch;
          if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
        end;
      end;
    end;
  end;
  Result := NewSize * (Bits div 8) * Channels;
end;
(*** IDCPitchScale ************************************************************)
function TDCPitchShift.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCPitchShift.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCPitchShift.get_Pitch(out aPitch: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  aPitch := fPitch;
end;

function TDCPitchShift.set_Pitch(aPitch: Cardinal): HRESULT; stdcall;
begin
  Result := S_OK;
  fPitch := aPitch;
end;


end.
