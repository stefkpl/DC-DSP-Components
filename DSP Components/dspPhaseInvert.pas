
    (*********************************************************************
     *  dspPhaseInvert.pas                                               *
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
  @abstract(DSP Filter to Invert the Phase of a specific Channel.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspPhaseInvert;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math, dspInterfaces, Windows;

type
  { TDCPhaseInvert - Phase Invert Component that inverts specific Channels. }
  TDCPhaseInvert = class(TComponent, IDCPhaseInvert)
  private
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fInvert : array[0..MaxChannels -1] of Boolean;
    {@exclude}
    procedure SetInvert(Channel : Byte; Invert : Boolean);
    {@exclude}
    function GetInvert(Channel : Byte) : Boolean;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCPhaseInvert. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Enables and Disables Phaseinvert for a specific Channel. }
    property Invert[Channel : Byte] : Boolean read GetInvert write SetInvert;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Invert(aChannel : Byte; out aInvert : BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Invert(aChannel : Byte; aInvert : BOOL): HRESULT; stdcall;
  published
    { Enables or Disables seperate Invert. If Enabled every Channel will be
      inverted by it´s own invert Value. If Disabled every Channel will be
      inverted by the Value of Channel 0. }
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

constructor TDCPhaseInvert.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fInvert[i] := False;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCPhaseInvert.Destroy;
begin
  inherited Destroy;
end;

procedure TDCPhaseInvert.SetInvert(Channel : Byte; Invert : Boolean);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fInvert[Channel] := Invert;
end;

function TDCPhaseInvert.GetInvert(Channel : Byte) : Boolean;
begin
  Result := False;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fInvert[Channel];
end;

procedure TDCPhaseInvert.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
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

procedure TDCPhaseInvert.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  NumSamples : integer;
  i,c : integer;
  Inv : Boolean;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8  := PByteArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Inv := fInvert[c] else Inv := fInvert[0];
        if Inv then for i := 0 to NumSamples -1 do Buf8^[i * Channels + c] := Clip_8((Buf8^[i * Channels + c] - 128) * -1) + 128;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Inv := fInvert[c] else Inv := fInvert[0];
        if Inv then for i := 0 to NumSamples -1 do Buf16^[i * Channels + c] := Clip_16(Buf16^[i * Channels + c] * -1);
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Inv := fInvert[c] else Inv := fInvert[0];
        if Inv then for i := 0 to NumSamples -1 do Buf24^[i * Channels + c] := Cvt32BitTo24(Clip_24(Cvt24BitTo32(Buf24^[i * Channels + c]) * -1));
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for c := 0 to Channels -1 do
        begin
          if Seperate then Inv := fInvert[c] else Inv := fInvert[0];
          if Inv then for i := 0 to NumSamples -1 do Buf32^[i * Channels + c] := Buf32^[i * Channels + c] * -1;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to Channels -1 do
        begin
          if Seperate then Inv := fInvert[c] else Inv := fInvert[0];
          if Inv then for i := 0 to NumSamples -1 do Buf32i^[i * Channels + c] := Clip_32(Int64(Buf32i^[i * Channels + c]) * -1);
        end;
      end;
    end;
  end;
end;
(*** IDCPhaseInvert ***********************************************************)
function TDCPhaseInvert.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCPhaseInvert.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCPhaseInvert.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCPhaseInvert.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCPhaseInvert.get_Invert(aChannel : Byte; out aInvert : BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aInvert := GetInvert(aChannel);
end;

function TDCPhaseInvert.set_Invert(aChannel : Byte; aInvert : BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  SetInvert(aChannel,aInvert);
end;

end.
