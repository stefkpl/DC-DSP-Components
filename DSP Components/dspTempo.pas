
    (*********************************************************************
     *  dspTempo.pas                                                     *
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
     *  This Code is based on the C++ Sourcecode of the Winamp2 DSP      *
     *  Pacemaker Plugin (C) 1999 Olli Parviainen (oparviai@iki.fi)      *
     *  Licensing Information:                                           *
     *  The source can freely be modified, reused & redistributed for    *
     *  non profitable uses. Use for commercial purposes prohibited.     *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)
{
  @abstract(DSP Filter to increase or decrease the Tempo without changing the Pitch.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspTempo;

interface

uses
  Windows, Classes, dspConst, dspUtils, Forms, Math, dspInterfaces;

type
  { TDCTempo - DSP Filter to increase or decrease the Tempo without changing the Pitch. }
  TDCTempo = class(TComponent, IDCTempo)
  protected
    {@exclude}
    fDefBuffer : PChar;
    {@exclude}
    fWorkBuffer : PChar;
    {@exclude}
    fDefBufferLen : integer;
    {@exclude}
    fWorkBufferLen : integer;
    {@exclude}
    fTempoMidBuffer : PSmallIntArray;
    {@exclude}
    fTempoInputBuffer : PSmallIntArray;
    {@exclude}
    fTempoOutputBuffer : PSmallIntArray;
    {@exclude}
    fTempoSwapBuffer : PSmallIntArray;
    {@exclude}
    fTempoInbuffSamples : integer;
    {@exclude}
    fTempoOutbuffSamples : integer;
    {@exclude}
    fTempoMidBufferLen : integer;
    {@exclude}
    fTempoOutputBufferPtr : integer;
    {@exclude}
    fTempo,
    {@exclude}
	  fTempoPrev,
    {@exclude}
  	fTempoSampleReq : integer;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    function DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
  public
    { Creates an Instance of TDCTempo. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. Return Value is the new Size of
      the Buffer}
    function Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
    { Initializes the Filter. }
    procedure Init(SampleRate : integer; Bits : Byte; Channels : Byte);
    { Flushes the Filter. }
    procedure Flush;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Tempo(out aTempo: integer): HRESULT; stdcall;
    {@exclude}
    function set_Tempo(aTempo: integer): HRESULT; stdcall;
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
    { Specifys the Tempo. default = 1000. }
    property Tempo : integer read fTempo write fTempo;
  end;

implementation

uses SysUtils;

constructor TDCTempo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEnabled := False;
  fProcessMessages := False;
  fTempo := 1000;
  fSampleSize := DefaultSampleSize;
  fDefBuffer := AllocMem(2);
  fDefBufferLen := 1;
  fWorkBuffer := AllocMem(2);
  fWorkBufferLen := 1;
end;

destructor TDCTempo.Destroy;
begin
	if fTempoInputBuffer <> nil then FreeMem(fTempoInputBuffer);
	if fTempoOutputBuffer <> nil then FreeMem(fTempoOutputBuffer);
	if fTempoMidBuffer <> nil then FreeMem(fTempoMidBuffer);
	if fTempoSwapBuffer <> nil then FreeMem(fTempoSwapBuffer);
  FreeMem(fDefBuffer);
  FreeMem(fWorkBuffer);
  inherited Destroy;
end;

function TDCTempo.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
  NewSize : integer;
  Len : integer;
begin
  Result := Size;
  if not fEnabled then Exit;
  SplitBuffer := Buffer;
  SplitSize := 2048 * (Bits div 8) * Channels;
  Len := Round(Size * 10 / (Size / SplitSize));
  if Len > fDefBufferLen then
  begin
    fDefBuffer := ReallocMemory(fDefBuffer,Len);
    fDefBufferLen := Len;
  end;
  if Len > fWorkBufferLen then
  begin
    fWorkBuffer := ReallocMemory(fWorkBuffer,Len);
    fWorkBufferLen := Len;
  end;
  Move(Buffer^,fWorkBuffer^,Size);
  Result := 0;
  SizeLeft := Size;
  while SizeLeft > 0 do
  begin
    if SizeLeft > SplitSize then CurrentSize := SplitSize
                            else CurrentSize := SizeLeft;

    Move(fWorkBuffer[Size - SizeLeft],fDefBuffer^,CurrentSize);
    NewSize := DoDSP(fDefBuffer,CurrentSize,Bits,Channels,Float);
    if NewSize > 0 then Move(fDefBuffer^,SplitBuffer[Result],NewSize);
    Result := Result + NewSize;
    dec(SizeLeft,SplitSize);
  end;
end;

function TDCTempo.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean) : integer;
var
	ovl_skip,temp,c,i,outsamples : integer;
  NumSamples : integer;
  Buff : Pointer;
  Buf8 : PByteArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
begin
  Buff := nil;
  Buf8 := nil;
  Buf24 := nil;
  Buf32i := nil;
  Buf32 := nil;
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for i := 0 to NumSamples * Channels -1 do fTempoSwapBuffer[i] := (Buf8[i] - 128) * 256;
      Buff := fTempoSwapBuffer;
    end;
    16:
    begin
      Buff := Buffer;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for i := 0 to NumSamples * Channels -1 do fTempoSwapBuffer[i] := Cvt24BitTo32(Buf24[i]) div 256;
      Buff := fTempoSwapBuffer;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for i := 0 to NumSamples * Channels -1 do fTempoSwapBuffer[i] := Round(Buf32[i] * 10000);
        Buff := fTempoSwapBuffer;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for i := 0 to NumSamples * Channels -1 do fTempoSwapBuffer[i] := Buf32i[i] div 65536;
        Buff := fTempoSwapBuffer;
      end;
    end;
  end;
  Move(Buff^, fTempoInputBuffer[Channels * fTempoInbuffSamples], Size);
  fTempoInbuffSamples := fTempoInbuffSamples + numsamples;

	if (fTempoPrev <> tempo) then
  begin
		fTempoSampleReq := max(tempo * 3088 div 1000, 3600);
		fTempoPrev := tempo;
	end;

	if (fTempoInbuffSamples >= fTempoSampleReq) then
  begin
    Move(fTempoOutputBuffer[fTempoOutputBufferPtr], fTempoOutputBuffer^, Channels * (Bits div 8) * fTempoOutbuffSamples);
		fTempoOutputBufferPtr := 0;

    for c := 0 to Channels -1 do
      for i := 0 to 511 do
        fTempoOutputBuffer[(fTempoOutbuffSamples * Channels) + (i * Channels + c)] := (fTempoInputBuffer[i * Channels + c] * i + fTempoMidBuffer[i * channels + c] * (512 - i)) shr 9;

		inc(fTempoOutbuffSamples,512);
		temp := 5152 - (5152 mod (Channels shl 1));
		Move(fTempoInputBuffer[Channels * 512], fTempoOutputBuffer[Channels * fTempoOutbuffSamples], (Bits div 8) * temp);
		fTempoOutbuffSamples := fTempoOutbuffSamples + (temp shr 1);
		Move(fTempoInputBuffer[Channels * 3088], fTempoMidBuffer^, Channels * 1024);
    ovl_skip := tempo * 6176 div 2000;
		ovl_skip := ovl_skip - (ovl_skip mod Channels);
		dec(fTempoInbuffSamples,ovl_skip);
		Move(fTempoInputBuffer[Channels * ovl_skip], fTempoInputBuffer^, Channels * (Bits div 8) * fTempoInbuffSamples);
	end;

  outsamples := 1000 * numsamples div tempo;
  outsamples := outsamples - (outsamples mod Channels);
  if (outsamples >= fTempoOutbuffSamples) then	outsamples := fTempoOutbuffSamples - (fTempoOutbuffSamples mod Channels);

  Move(fTempoOutputBuffer[fTempoOutputBufferPtr], Buff^, outsamples * Channels * (Bits div 8));
  case Bits of
    8 : for i := 0 to outsamples * Channels -1 do Buf8[i]:= (fTempoSwapBuffer[i] div 256) + 128;
    24: for i := 0 to outsamples * Channels -1 do Buf24[i]:= Cvt32BitTo24(fTempoSwapBuffer[i] * 256);
    32:
    begin
      if Float then for i := 0 to outsamples * Channels -1 do Buf32[i] := fTempoSwapBuffer[i] / 10000
               else for i := 0 to outsamples * Channels -1 do Buf32i[i]:= fTempoSwapBuffer[i] * 65536;
    end;
  end;

	fTempoOutbuffSamples := fTempoOutbuffSamples - outsamples;
	fTempoOutputBufferPtr := fTempoOutputBufferPtr + outsamples * Channels;
  Result := outsamples * Channels * (Bits div 8);
end;

procedure TDCTempo.Flush;
begin
	fTempoOutbuffSamples := 0;
	fTempoInbuffSamples := 0;
	if fTempoMidBuffer <> nil then ZeroMemory(fTempoMidBuffer,fTempoMidBufferLen);
end;

procedure TDCTempo.Init(SampleRate : integer; Bits : Byte; Channels : Byte);
begin
	if fTempoInputBuffer <> nil then FreeMem(fTempoInputBuffer);
	if fTempoOutputBuffer <> nil then FreeMem(fTempoOutputBuffer);
	if fTempoMidBuffer <> nil then FreeMem(fTempoMidBuffer);
	if fTempoSwapBuffer <> nil then FreeMem(fTempoSwapBuffer);
  fTempoInputBuffer := nil;
	fTempoOutputBuffer := nil;
	fTempoMidBuffer := nil;
	fTempoSwapBuffer := nil;
	fTempoMidBufferLen := SampleRate * (Bits div 8) * Channels * 5;
	fTempoMidBuffer := AllocMem(fTempoMidBufferLen);
	fTempoInputBuffer := AllocMem(fTempoMidBufferLen);
	fTempoOutputBuffer := AllocMem(fTempoMidBufferLen);
  fTempoSwapBuffer := AllocMem(fTempoMidBufferLen);
	fTempoOutputBufferPtr := 0;
end;
(*** IDCTempo *****************************************************************)
function TDCTempo.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCTempo.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCTempo.get_Tempo(out aTempo: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  atempo := fTempo;
end;

function TDCTempo.set_Tempo(aTempo: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  fTempo := aTempo;
end;

end.
