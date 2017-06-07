
    (*********************************************************************
     *  dspSound3D.pas                                                   *
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
  @abstract(DSP Filter to enhance the Sound. Works only on 2 Channel Data.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspSound3D;

interface

uses
  Classes, dspConst, dspUtils, Forms, dspInterfaces, Windows;

type
  { TDCSound3D - 3D Sound Amplification Component that works on 2 Channels only.
    The difference between Left and Right Channel is calculated and added to the
    main Signal. For 32Bit Float and SSE CPUs make sure the Buffer is aligned on
    16byte. The Buffer is checked for 16byte Alignment, if this is not present,
    then the default FPU will be used. }
  TDCSound3D = class(TComponent, IDCSound3D)
  protected
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fVolume : Word;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { TDCSound3D Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCSound3D Destructor }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Volume(out aVolume: Word): HRESULT; stdcall;
    {@exclude}
    function set_Volume(aVolume: Word): HRESULT; stdcall;
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
    { Sets the 3D Amplification Value. A Value of 1000 is the same as disabling
      the Filter. Values < 1000 is the same as doing a DownMix (Mono) of the 2
      Channels. Values higher then 10000 will distort the Sound. }
    property Volume : Word read fVolume write fVolume;
  end;

implementation

constructor TDCSound3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVolume := 1000;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCSound3D.Destroy;
begin
  inherited Destroy;
end;

procedure TDCSound3D.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled or (Channels <> 2) then Exit;
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

procedure TDCSound3D.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
const
  pmu3DNow : array[0..1] of Single = (0.5, 0.5);
  pmuSSE : array[0..3] of Single = (0.5, 0.5, 0.5, 0.5);

var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  i : integer;
  avg,
  ldiff,
  rdiff,
  tmp,
  mul : Single;
  NumSamples : integer;
  l, r : integer;
begin
  NumSamples := Size div (Bits div 8);
  mul := Volume / 10000 * 6.5536;
  case Bits of
    8:
    begin
      Buf8  := PByteArray(Buffer);
      for i := 0 to (NumSamples div 2) - 1 do
      begin
        avg := ((Buf8^[i * 2] - 128) + (Buf8^[i * 2 + 1] - 128)) / 2;
        ldiff := (Buf8^[i * 2] - 128) - avg;
        rdiff := (Buf8^[i * 2 + 1] - 128) - avg;
        tmp := avg + ldiff * mul;
        tmp := (tmp + (Buf8^[i * 2] - 128)) / 2;
        Buf8^[i * 2] := Clip_8(Round(tmp)) + 128;
        tmp := avg + rdiff * mul;
        tmp := (tmp + (Buf8^[i * 2 + 1] - 128)) / 2;
        Buf8^[i * 2 + 1] := Clip_8(Round(tmp)) + 128;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for i := 0 to (NumSamples div 2) - 1 do
      begin
        avg := (Buf16^[i * 2] + Buf16^[i * 2 + 1]) / 2;
        ldiff := Buf16^[i * 2] - avg;
        rdiff := Buf16^[i * 2 + 1] - avg;
        tmp := avg + ldiff * mul;
        tmp := (tmp + Buf16^[i * 2]) / 2;
        Buf16^[i * 2] := Clip_16(Round(tmp));
        tmp := avg + rdiff * mul;
        tmp := (tmp + Buf16^[i * 2 + 1]) / 2;
        Buf16^[i * 2 + 1] := Clip_16(Round(tmp));
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for i := 0 to (NumSamples div 2) - 1 do
      begin
        l := Cvt24BitTo32(Buf24^[i * 2]);
        r := Cvt24BitTo32(Buf24^[i * 2 + 1]);
        avg := (l + r) / 2;
        ldiff := l - avg;
        rdiff := r - avg;
        tmp := avg + ldiff * mul;
        tmp := (tmp + l) / 2;
        Buf24^[i * 2] := Cvt32BitTo24(Clip_24(Round(tmp)));
        tmp := avg + rdiff * mul;
        tmp := (tmp + r) / 2;
        Buf24^[i * 2 + 1] := Cvt32BitTo24(Clip_24(Round(tmp)));
      end;
    end;
    32:
    begin
      if Float then
      begin
        Buf32 := PFloatArray(Buffer);
        if has3DNow and (NumSamples mod 2 = 0) then
        begin
           asm
             femms
             mov       eax, Buf32
             movd      mm7, mul
             punpckldq mm7, mm7
             movq      mm6, pmu3DNow
             xor       ecx, ecx
             prefetch  [eax]           // give the mmu a heads-up

           @@Loop:
             prefetch  [eax+ecx]     // Inform mmu about next Sample Position
             movq      mm0, [eax+ecx]
             movq      mm1, mm0
             movq      mm2, mm1
             pfacc     mm0, mm0
             pfmul     mm0, mm6
             pfsub     mm1, mm0
             pfmul     mm1, mm7
             pfadd     mm0, mm1
             pfadd     mm2, mm0
             pfmul     mm2, mm6
             movq      [eax+ecx], mm2

             add       ecx, 8
             cmp       ecx, Size
             jl        @@Loop

             femms
           end
        end else
        if hasSSE and (NumSamples mod 4 = 0) and IsValidSSEBuffer(Buffer) then
        begin
          asm
             mov       eax, Buf32
             movss     xmm7, mul
             shufps    xmm7, xmm7, $0
             movups    xmm6, pmuSSE
             xor       ecx, ecx

           @@Loop:
             movaps    xmm0, [eax+ecx]
             movaps    xmm1, xmm0
             movaps    xmm2, xmm0
             movaps    xmm3, xmm0
             shufps    xmm3, xmm3, $B1
             addps     xmm0, xmm3
             mulps     xmm0, xmm6
             subps     xmm1, xmm0
             mulps     xmm1, xmm7
             addps     xmm1, xmm0
             addps     xmm2, xmm1
             mulps     xmm2, xmm6
             movaps    [eax+ecx], xmm2

             add       ecx, 16
             cmp       ecx, Size
             jl        @@Loop
          end;
        end else
        begin
          for i := 0 to (NumSamples div 2) - 1 do
          begin
            avg := (Buf32^[i * 2] + Buf32^[i * 2 + 1]) / 2;
            ldiff := Buf32^[i * 2] - avg;
            rdiff := Buf32^[i * 2 + 1] - avg;
            tmp := avg + ldiff * mul;
            Buf32^[i * 2] := (tmp + Buf32^[i * 2]) / 2;
            tmp := avg + rdiff * mul;
            Buf32^[i * 2 + 1] := (tmp + Buf32^[i * 2 + 1]) / 2;
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for i := 0 to (NumSamples div 2) - 1 do
        begin
          l := Buf32i^[i * 2] div 32768;
          r := Buf32i^[i * 2 + 1] div 32768;
          avg := (l + r) / 2;
          ldiff := l - avg;
          rdiff := r - avg;
          tmp := avg + ldiff * mul;
          tmp := (tmp + l) / 2;
          Buf32i^[i * 2] := Clip_32(Int64(Round(tmp)) * 32768);
          tmp := avg + rdiff * mul;
          tmp := (tmp + r) / 2;
          Buf32i^[i * 2 + 1] := Clip_32(Int64(Round(tmp)) * 32768);
        end;
      end;
    end;
  end;
end;
(*** IDCSound3D ***************************************************************)
function TDCSound3D.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCSound3D.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCSound3D.get_Volume(out aVolume: Word): HRESULT; stdcall;
begin
  Result := S_OK;
  aVolume := fVolume;
end;

function TDCSound3D.set_Volume(aVolume: Word): HRESULT; stdcall;
begin
  Result := S_OK;
  fVolume := aVolume;
end;

end.
