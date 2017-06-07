
    (*********************************************************************
     *  dspChannelOrder.pas                                              *
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
  @abstract(DSP Filter to change the Channel Order. eg: Swap Left/Right.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspChannelOrder;

interface

uses
  Classes, dspConst, dspUtils, Math, Forms, dspInterfaces, Windows;

type
  { TDCChannelOrder - Component to Swap Channels to any needed Position. }
  TDCChannelOrder = class(TComponent, IDCChannelOrder)
  protected
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fBuffer : Pointer;
    {@exclude}
    fBufferSize : integer;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fOrder : TChannelOrder;
    {@exclude}
    procedure SetOrder(Channel : Byte; Order : Byte);
    {@exclude}
    function GetOrder(Channel : Byte) : Byte;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCChannelOrder. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Specifys the Order of the Channels. If one Channel is not in the Range of
      the Channels in the Buffer, then it won´t be Processed. }
    property Order[Channel : Byte] : Byte read GetOrder write SetOrder;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Order(aChannel : Byte; out aOrder : Byte): HRESULT; stdcall;
    {@exclude}
    function set_Order(aChannel : Byte; aOrder : Byte): HRESULT; stdcall;
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
  end;

implementation

uses SysUtils;

constructor TDCChannelOrder.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fOrder[i] := i;
  fEnabled := False;
  fBufferSize := 0;
  fBuffer := AllocMem(2);
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCChannelOrder.Destroy;
begin
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TDCChannelOrder.SetOrder(Channel : Byte; Order : Byte);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fOrder[Channel] := Order;
end;

function TDCChannelOrder.GetOrder(Channel : Byte) : Byte;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fOrder[Channel];
end;

procedure TDCChannelOrder.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCChannelOrder.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf32 : PFloatArray;
  SBuf8  : PByteArray;
  SBuf16 : PSmallIntArray;
  SBuf32 : PFloatArray;
  Buf24 : PInteger24Array;
  SBuf24 : Pinteger24Array;
  i, c : integer;
  NumSamples : integer;
begin
  if fBufferSize < Size then
  begin
    fBuffer := ReallocMemory(fBuffer,Size);
    fBufferSize := Size;
  end;
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      SBuf8 := fBuffer;
      Move(Buf8^,SBuf8^,Size);
      for c := 0 to Channels -1 do
        if (fOrder[c] <> c) and (fOrder[c] < Channels) then
          for i := 0 to NumSamples -1 do
            Buf8^[i * Channels + c] := SBuf8^[i * Channels + fOrder[c]];
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      SBuf16 := fBuffer;
      Move(Buf16^,SBuf16^,Size);
      for c := 0 to Channels -1 do
        if (fOrder[c] <> c) and (fOrder[c] < Channels) then
          for i := 0 to NumSamples -1 do
            Buf16^[i * Channels + c] := SBuf16^[i * Channels + fOrder[c]];
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      SBuf24 := fBuffer;
      Move(Buf24^,SBuf24^,Size);
      for c := 0 to Channels -1 do
        if (fOrder[c] <> c) and (fOrder[c] < Channels) then
          for i := 0 to NumSamples -1 do
            Buf24^[i * Channels + c] := SBuf24^[i * Channels + fOrder[c]];
    end;
    32:
    begin
      Buf32 := PFloatArray(Buffer);
      SBuf32 := fBuffer;
      Move(Buf32^,SBuf32^,Size);
      for c := 0 to Channels -1 do
        if (fOrder[c] <> c) and (fOrder[c] < Channels) then
          for i := 0 to NumSamples -1 do
            Buf32^[i * Channels + c] := SBuf32^[i * Channels + fOrder[c]];
    end;
  end;
end;
(*** IDCChannelOrder **********************************************************)
function TDCChannelOrder.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCChannelOrder.set_Enabled(aEnabled: BOOL ): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCChannelOrder.get_Order(aChannel : Byte; out aOrder : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  aOrder := GetOrder(aChannel);
end;

function TDCChannelOrder.set_Order(aChannel : Byte; aOrder : Byte): HRESULT; stdcall;
begin
  Result := S_OK;
  SetOrder(aChannel,aOrder);
end;

end.

