
    (*********************************************************************
     *  dspDownMix.pas                                                   *
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
  @abstract(DSP Filter that mixes all Channels together.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspDownMix;

interface

uses
  Classes, dspConst, dspUtils, Forms, dspInterfaces, Windows;

type
  { TDCDownMix - DSP Component to Mix every Channel together to one and then
    copy that Value to every Channel. }
  TDCDownMix = class(TComponent, IDCDownMix)
  private
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCDownMix. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this to Process an amount of Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
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

constructor TDCDownMix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCDownMix.Destroy;
begin
  inherited Destroy;
end;

procedure TDCDownMix.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCDownMix.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i,
  c : integer;
  tmp : integer;
  tmp2 : Single;
  tmp3 : Int64;
begin
  NumSamples := Size div (Bits div 8);
  case Bits of
    8:
    begin
      Buf8  := PByteArray(Buffer);
      for i := 0 to (NumSamples div Channels) -1 do
      begin
        tmp := 0;
        for c := 0 to Channels -1 do inc(tmp,(Buf8^ [i * Channels + c] - 128));
        tmp := Clip_8(tmp div Channels) + 128;
        for c := 0 to Channels -1 do Buf8^ [i * Channels + c] := tmp;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for i := 0 to (NumSamples div Channels) -1 do
      begin
        tmp := 0;
        for c := 0 to Channels -1 do inc(tmp,Buf16^[i * Channels + c]);
        tmp := Clip_16(tmp div Channels);
        for c := 0 to Channels -1 do Buf16^ [i * Channels + c] := tmp;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for i := 0 to (NumSamples div Channels) -1 do
      begin
        tmp3 := 0;
        for c := 0 to Channels -1 do inc(tmp3,Cvt24BitTo32(Buf24^[i * Channels + c]));
        tmp3 := Clip_24(tmp3 div Channels);
        for c := 0 to Channels -1 do Buf24^ [i * Channels + c] := Cvt32BitTo24(tmp3);
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for i := 0 to (NumSamples div Channels) -1 do
        begin
          tmp2 := 0;
          for c := 0 to Channels -1 do tmp2 := tmp2 + Buf32^[i * Channels + c];
          tmp2 := tmp2 / Channels;
          for c := 0 to Channels -1 do Buf32^ [i * Channels + c] := tmp2;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for i := 0 to (NumSamples div Channels) -1 do
        begin
          tmp3 := 0;
          for c := 0 to Channels -1 do inc(tmp3,Buf32i^[i * Channels + c]);
          tmp3 := Clip_32(tmp3 div Channels);
          for c := 0 to Channels -1 do Buf32i^ [i * Channels + c] := tmp3;
        end;
      end;
    end;
  end;
end;
(*** IDCDownMix ***************************************************************)
function TDCDownMix.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCDownMix.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

end.
