
    (*********************************************************************
     *  dspReverse.pas                                                   *
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
  @abstract(DSP Filter that reverses the content of an Audio Buffer.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspReverse;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math;

type
  { TDCReverse - DSP Component to Reverse the content of an Audio Buffer. Each
    Channel can be reversed seperate. }
  TDCReverse = class(TComponent)
  private
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : integer;
    {@exclude}
    fReverse : array[0..MaxChannels -1] of Boolean;
    {@exclude}
    procedure SetReverse(Channel : Byte; Reverse : Boolean);
    {@exclude}
    function GetReverse(Channel : Byte) : Boolean;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCReverse. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Enables and Disables Reversing for a specific Channel. }
    property Reverse[Channel : Byte] : Boolean read GetReverse write SetReverse;
  published
    { Enables or Disables seperate Reverse. If Enabled every Channel will be
      reversed by it´s own reverse Value. If Disabled every Channel will be
      reversed by the Value of Channel 0. }
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
    property SampleSize : integer read fSampleSize write fSampleSize;
  end;

implementation

constructor TDCReverse.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fReverse[i] := False;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCReverse.Destroy;
begin
  inherited Destroy;
end;

procedure TDCReverse.SetReverse(Channel : Byte; Reverse : Boolean);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fReverse[Channel] := Reverse;
end;

function TDCReverse.GetReverse(Channel : Byte) : Boolean;
begin
  Result := False;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fReverse[Channel];
end;

procedure TDCReverse.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
begin
  if not fEnabled then Exit;
  DoDSP(Buffer,Size,Bits,Channels,Float);
end;

procedure TDCReverse.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  NumSamples : integer;
  i,c : integer;
  Rev : Boolean;
  t8 : ShortInt;
  t16 : SmallInt;
  t24 : T24BitSample;
  t32 : Single;
  t32i : integer;
begin
  NumSamples := Size div (Bits div 8) div Channels;
  case Bits of
    8:
    begin
      Buf8  := PByteArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Rev := fReverse[c] else Rev := fReverse[0];
        if Rev then
        begin
          for i := 0 to NumSamples div 2 -1 do
          begin
            t8 := Buf8^[i * Channels + c];
            Buf8^[i * Channels + c] := Buf8^[(NumSamples - 1 - i) * Channels + c];
            Buf8^[(NumSamples - 1 - i) * Channels + c] := t8;
            if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
          end;
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Rev := fReverse[c] else Rev := fReverse[0];
        if Rev then
        begin
          for i := 0 to NumSamples div 2 -1 do
          begin
            t16 := Buf16^[i * Channels + c];
            Buf16^[i * Channels + c] := Buf16^[(NumSamples - 1 - i) * Channels + c];
            Buf16^[(NumSamples - 1 - i) * Channels + c] := t16;
            if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
          end;
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to Channels -1 do
      begin
        if Seperate then Rev := fReverse[c] else Rev := fReverse[0];
        if Rev then
        begin
          for i := 0 to NumSamples div 2 -1 do
          begin
            t24 := Buf24^[i * Channels + c];
            Buf24^[i * Channels + c] := Buf24^[(NumSamples - 1 - i) * Channels + c];
            Buf24^[(NumSamples - 1 - i) * Channels + c] := t24;
            if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
          end;
        end;
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        for c := 0 to Channels -1 do
        begin
          if Seperate then Rev := fReverse[c] else Rev := fReverse[0];
          if Rev then
          begin
            for i := 0 to NumSamples div 2 -1 do
            begin
              t32 := Buf32^[i * Channels + c];
              Buf32^[i * Channels + c] := Buf32^[(NumSamples - 1 - i) * Channels + c];
              Buf32^[(NumSamples - 1 - i) * Channels + c] := t32;
              if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
            end;
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to Channels -1 do
        begin
          if Seperate then Rev := fReverse[c] else Rev := fReverse[0];
          if Rev then
          begin
            for i := 0 to NumSamples div 2 -1 do
            begin
              t32i := Buf32i^[i * Channels + c];
              Buf32i^[i * Channels + c] := Buf32i^[(NumSamples - 1 - i) * Channels + c];
              Buf32i^[(NumSamples - 1 - i) * Channels + c] := t32i;
              if fProcessMessages and (fSampleSize > 0) and (i mod fSampleSize = 0) then Application.ProcessMessages;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
