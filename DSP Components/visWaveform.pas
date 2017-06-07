
    (*********************************************************************
     *  visWaveform.pas                                                  *
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
  @abstract(Used to get some Values for Visual Drawing of a Waveform.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit visWaveform;

interface

uses
  Classes, dspConst, dspUtils;

type

  { TDCWaveform - Designed to retrieve some Values to do Visual drawing of a Waveform.
    Output has always the same scaling on every Buffertype. }
  TDCWaveform = class(TComponent)
  private
    {@exclude}
    fMinY : integer;
    {@exclude}
    fMaxY : integer;
    {@exclude}
    fNumSamples : integer;
    {@exclude}
    fMixChannels : Boolean;
    {@exclude}
    fBuffer : TVisualBuffer;
    {@exclude}
    fNotify : TDCVisualNotifyEvent;
    {@exclude}
    fSampleSkip : Byte;
    {@exclude}
    function GetSampleSkip : Byte;
    {@exclude}
    procedure SetSampleSkip(Skip : Byte);
  public
    { TDCWaveform Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCWaveform Destructor }
    destructor Destroy; override;
    { Processes an amount of Data. if the "OnWaveformData" isn´t Assigned then
      this procedure Exits. Output is send as a Pointer to a TVisualBuffer that
      also informs how much Samples, Channels and how the Scaling is done. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Sets every Value of the Buffer to Zero. It´s not really needed to do this. }
    procedure Flush;
    { This is Buffer holds the Result of the Process Call. Usefull if you´re not
      using the OnWaveformData Event. }
    property Buffer : TVisualBuffer read fBuffer;
  published
    { Sets the Minimum Value of the Range the Output Samples will be. This Value
      MUST be lower then MaxY! default = 0 }
    property MinY : integer read fMinY write fMinY;
    { Sets the Maximum Value of the Range the Output Samples will be. This Value
      MUST be higher then MinY! default = 100 }
    property MaxY : integer read fMaxY write fMaxY;
    { Sets the Number of Samples needed. Only that amount of Samples per Channel
      will be placed in the Output Buffer. default = 512 }
    property NumSamples : integer read fNumSamples write fNumSamples;
    { If Enabled then every Channel will be Mixed to one (first) Output Channel.
      If Disabled then every Channel gets his own Buffer. default = True }
    property MixChannels : Boolean read fMixChannels write fMixChannels;
    { This Event will be raised after the Process procedure has been called.
      You MUST Assign it or no Buffer will be Processed! }
    property OnWaveformData : TDCVisualNotifyEvent read fNotify write fNotify;
    { Skips an amount of Samples. This makes the Visual drawing smoother.
      default = 1 }
    property SampleSkip : Byte read GetSampleSkip write SetSampleSkip;
  end;

implementation

uses Math;

constructor TDCWaveform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMixChannels := True;
  fMinY := 0;
  fMaxY := 100;
  fNumSamples := 512;
  fSampleSkip := 1;
  Flush;
end;

destructor TDCWaveform.Destroy;
begin
  inherited Destroy;
end;

procedure TDCWaveform.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  pChannels : integer;
  NumSamples : integer;
  i, c, z : integer;
  tmp : integer;
  ch : integer;
  fRange : integer;
  Skip : integer;
begin
  if (Buffer = nil) or (Size = 0) or (Bits = 0) or (Channels = 0) then Exit;
  Skip := fSampleSkip;
  if Skip < 1 then Skip := 1;
  NumSamples := Size div (Bits div 8) div Channels div Skip;
  if Channels > MaxChannels then ch := MaxChannels else ch := Channels;
  if MixChannels then pChannels := 1 else pChannels := ch;
  if NumSamples > fNumSamples then NumSamples := fNumSamples;

  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      if MixChannels then
      begin
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD((Buf8^[(i * Channels * Skip) + c + z] - 128) * 256 + 32768);
          end;
          fBuffer[0,i] := ClipWORD(tmp div (ch * fSampleSkip));
        end;
      end else
      begin
        for i := 0 to NumSamples -1 do
          for c := 0 to ch -1 do
          begin
            tmp := 0;
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD((Buf8^[(i * Channels * Skip) + c] - 128) * 256 + 32768);
            fBuffer[c,i] := ClipWORD(tmp div fSampleSkip);
          end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      if MixChannels then
      begin
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Buf16^[(i * Channels * Skip) + c + z] + 32768);
          end;
          fBuffer[0,i] := ClipWORD(tmp div (ch * fSampleSkip));
        end;
      end else
      begin
        for i := 0 to NumSamples -1 do
          for c := 0 to ch -1 do
          begin
            tmp := 0;
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Buf16^[(i * Channels * Skip) + c] + 32768);
            fBuffer[c,i] := ClipWORD(tmp div fSampleSkip);
          end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      if MixChannels then
      begin
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Cvt24BitTo32(Buf24^[(i * Channels * Skip) + c + z]) div 256 + 32768);
          end;
          fBuffer[0,i] := ClipWORD(tmp div (ch * fSampleSkip));
        end;
      end else
      begin
        for i := 0 to NumSamples -1 do
          for c := 0 to ch -1 do
          begin
            tmp := 0;
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Cvt24BitTo32(Buf24^[(i * Channels * Skip) + c]) div 256 + 32768);
            fBuffer[c,i] := ClipWORD(tmp div fSampleSkip);
          end;
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        if MixChannels then
        begin
          for i := 0 to NumSamples -1 do
          begin
            tmp := 0;
            for c := 0 to ch -1 do
            begin
              for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Round(Buf32^[(i * Channels * Skip) + c + z] * 32768 + 32768));
            end;
            fBuffer[0,i] := ClipWORD(tmp div (ch * fSampleSkip));
          end;
        end else
        begin
        for i := 0 to NumSamples -1 do
          for c := 0 to ch -1 do
          begin
            tmp := 0;
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Round(Buf32^[(i * Channels * Skip) + c] * 32768 + 32768));
            fBuffer[c,i] := ClipWORD(tmp div fSampleSkip);
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        if MixChannels then
        begin
          for i := 0 to NumSamples -1 do
          begin
            tmp := 0;
            for c := 0 to ch -1 do
            begin
              for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Buf32i^[(i * Channels * Skip) + c + z] div 65536 + 32768);
            end;
            fBuffer[0,i] := ClipWORD(tmp div (ch * fSampleSkip));
          end;
        end else
        begin
        for i := 0 to NumSamples -1 do
          for c := 0 to ch -1 do
          begin
            tmp := 0;
            for z := 0 to fSampleSkip -1 do tmp := tmp + ClipWORD(Buf32i^[(i * Channels * Skip) + c] div 65536 + 32768);
            fBuffer[c,i] := ClipWORD(tmp div fSampleSkip);
          end;
        end;
      end;
    end;
  end;

  fRange := fMaxY - fMinY;

  for c := 0 to ch -1 do
    for i := 0 to NumSamples -1 do
      fBuffer[c,i] := (fBuffer[c,i] * fRange div 65536) + fMinY;

  if Assigned(fNotify) then fNotify(Self,@fBuffer,fMinY,fMaxY,NumSamples,pChannels);
end;

procedure TDCWaveform.Flush;
begin
  FillChar(fBuffer,SizeOf(TVisualBuffer),0);
end;

function TDCWaveform.GetSampleSkip : Byte;
begin
  Result := fSampleSkip;
end;

procedure TDCWaveform.SetSampleSkip(Skip : Byte);
begin
  fSampleSkip := EnsureRange(Skip,1,255);
end;

end.

