
    (*********************************************************************
     *  dspBitrateConverter.pas                                          *
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
  @abstract(DSP Filter to convert Bitrates.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspBitrateConverter;

interface

uses
  Classes, dspConst, dspUtils, Forms, Math;

type
  { TDCBitrateConverter - DSP Filter to convert Bitrates. }
  TDCBitrateConverter = class(TComponent)
  protected
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fBitRate: TDCBitRate;
    {@exclude}
    function DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean): integer;
  public
    { Creates an Instance of TDCBitrateConverter. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. }
    function Process(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean): integer;
  published
    { Enables or Disables the Filter. }
    property Enabled : Boolean read fEnabled write fEnabled;
    { Specifys the target BitRate. }
    property BitRate : TDCBitRate read fBitRate write fBitRate;
  end;

implementation

constructor TDCBitrateConverter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fBitRate := br16BitInteger;

  fEnabled := False;
end;

destructor TDCBitrateConverter.Destroy;
begin
  inherited Destroy;
end;

function TDCBitrateConverter.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean): integer;
begin
  Result := Size;
  if not fEnabled then Exit;
  Result := DoDSP(Buffer,Size,Bits,Float);
end;

function TDCBitrateConverter.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Float : Boolean): integer;
const
  mmDivInt: array[0..1] of integer = ($4F000000, $4F000000);
  mmInt   : array[0..1] of integer = ($30000000, $30000000);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  i: integer;
  NumSamples: integer;
  PackedSize : integer;
begin
  Buf8 := PByteArray(Buffer);
  Buf16 := PSmallIntArray(Buffer);
  Buf24 := PInteger24Array(Buffer);
  Buf32 := PFloatArray(Buffer);
  Buf32i := PIntegerArray(Buffer);

  case fBitRate of
    br8BitInteger:  Result := Size * 8 div Bits;
    br16BitInteger: Result := Size * 16 div Bits;
    br24BitInteger: Result := Size * 24 div Bits;
    br32BitInteger,
    br32BitFloat:   Result := Size * 32 div Bits;
    else Result := Size;
  end;

  NumSamples := Size div (Bits div 8);
  case Bits of
    8:
    begin
      if (fBitRate = br8BitInteger) then Exit;
      case fBitRate of
        br16BitInteger: for i := NumSamples -1 downto 0 do Buf16^[i] := Clip_16((integer(Buf8^[i]) - 128) * 256);
        br24BitInteger: for i := NumSamples -1 downto 0 do Buf24^[i] := Cvt32BitTo24(Clip_24((integer(Buf8^[i]) - 128) * 65536));
        br32BitInteger: for i := NumSamples -1 downto 0 do Buf32i^[i] := Clip_32((integer(Buf8^[i]) - 128) * 16777216);
        br32BitFloat:   for i := NumSamples -1 downto 0 do Buf32^[i] := (integer(Buf8^[i]) - 128) / 128;
      end;
    end;
    16:
    begin
      if (fBitRate = br16BitInteger) then Exit;
      case fBitRate of
        br8BitInteger:  for i := 0 to NumSamples -1 do Buf8^[i] := Clip_8(Buf16^[i] div 256) + 128;
        br24BitInteger: for i := NumSamples -1 downto 0 do Buf24^[i] := Cvt32BitTo24(Clip_24(integer(Buf16^[i]) * 256));
        br32BitInteger: for i := NumSamples -1 downto 0 do Buf32i^[i] := Clip_32(integer(Buf16^[i]) * 65536);
        br32BitFloat:   for i := NumSamples -1 downto 0 do Buf32^[i] := Buf16^[i] / 32768;
      end;
    end;
    24:
    begin
      if (fBitRate = br24BitInteger) then Exit;
      case fBitRate of
        br8BitInteger:  for i := 0 to NumSamples -1 do Buf8^[i] := Clip_8(Cvt24BitTo32(Buf24^[i]) div 65536) + 128;
        br16BitInteger: for i := 0 to NumSamples -1 do Buf16^[i] := Clip_16(Cvt24BitTo32(Buf24^[i]) div 256);
        br32BitInteger: for i := NumSamples -1 downto 0 do Buf32i^[i] := Clip_32(Cvt24BitTo32(Buf24^[i]) * 256);
        br32BitFloat:   for i := NumSamples -1 downto 0 do Buf32^[i] := Cvt24BitTo32(Buf24^[i]) / 8388608;
      end;
    end;
    32:
    begin
      if Float then
      begin
        if (fBitRate = br32BitFloat) then Exit;
        case fBitRate of
          br8BitInteger:  for i := 0 to NumSamples -1 do Buf8^[i] := Clip_8(Round(Buf32^[i] * 256)) + 128;
          br16BitInteger: for i := 0 to NumSamples -1 do Buf16^[i] := Clip_16(Round(Buf32^[i] * 32768));
          br24BitInteger: for i := 0 to NumSamples -1 do Buf24^[i] := Cvt32BitTo24(Clip_24(Round(Buf32^[i] * 8388608)));
          br32BitInteger:
          begin
            if has3DNow then
            begin
              PackedSize := Size and -8;
              asm
                femms                     // Fast MMX Enter/Leave
                mov       eax, Buffer     // Move Buffer Pointer into EAX
                movq      mm1, mmDivInt   // use mm3 as high(integer) multiplier
                xor       ecx, ecx
                prefetch  [eax]           // give the mmu a heads-up

                @@Loop2:
                movq      mm0, [eax+ecx]  // Spl1 | Spl2
                pfmul     mm0, mm1        // multiply by high(integer)
                pf2id     mm0, mm0        // convert to integer
                movq      [eax+ecx], mm0  // Store Sample back to RAM

                add       ecx, 8
                prefetch  [eax+ecx]       // Inform mmu about next Sample Position
                cmp       ecx, PackedSize // Loop until ECX = PackedSize
                jl        @@Loop2

                cmp       ecx, Size       // Size doesn´t fit into 8byte Chunks
                jz        @@End2
                movd      mm0, [eax+ecx]
                pfmul     mm0, mm1        // multiply by high(integer)
                pf2id     mm0, mm0        // convert to integer
                movd      [eax+ecx], mm0  // Store Sample back to RAM
                @@End2:
                femms                     // Fast MMX Enter/Leave
              end;
            end else
              for i := 0 to NumSamples -1 do Buf32i^[i] := Clip_32(Round(Buf32^[i] * 2147483648));
          end;
        end;
      end else
      begin
        if (fBitRate = br32BitInteger) then Exit;
        case fBitRate of
          br8BitInteger:  for i := 0 to NumSamples -1 do Buf8^[i] := Clip_8(Buf32i^[i] div 16777216) + 128;
          br16BitInteger: for i := 0 to NumSamples -1 do Buf16^[i] := Clip_16(Buf32i^[i] div 65536);
          br24BitInteger: for i := 0 to NumSamples -1 do Buf24^[i] := Cvt32BitTo24(Clip_24(Buf32i^[i] div 256));
          br32BitFloat:
          begin
            if has3DNow then
            begin
              PackedSize := Size and -8;
              asm
                femms                     // Fast MMX Enter/Leave
                mov       eax, Buffer     // Move Buffer Pointer into EAX
                movq      mm1, mmInt      // use mm3 as 1/high(integer) divider
                xor       ecx, ecx
                prefetch  [eax]           // give the mmu a heads-up

                @@Loop2:
                movq      mm0, [eax+ecx]  // Spl1 | Spl2
                pi2fd     mm0, mm0        // convert to FP
                pfmul     mm0, mm1        // divide by high(integer)
                movq      [eax+ecx], mm0  // Store Sample back to RAM

                add       ecx, 8
                prefetch  [eax+ecx]       // Inform mmu about next Sample Position
                cmp       ecx, PackedSize // Loop until ECX = PackedSize
                jl        @@Loop2

                cmp       ecx, Size       // Size doesn´t fit into 8byte Chunks
                jz        @@End2
                movd      mm0, [eax+ecx]
                pi2fd     mm0, mm0        // convert to FP
                pfmul     mm0, mm1        // divide by high(integer)
                movd      [eax+ecx], mm0  // Store Sample back to RAM
                @@End2:
                femms                     // Fast MMX Enter/Leave
              end;
            end else
              for i := 0 to NumSamples -1 do Buf32^[i] := Buf32i^[i] / 2147483648;
          end
        end;
      end;
    end;
  end;
end;

end.

