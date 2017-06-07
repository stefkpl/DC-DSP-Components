
    (*********************************************************************
     *  dspUtils.pas                                                     *
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
  @abstract(Contains functions that are globaly used by DSP Components.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspUtils;

{$DEFINE USE_SSE} // If there is no 16byte Aligned Memory, Disable this !!

interface

uses
  dspConst, SysUtils;

  { Calculates a window Value. }
  function GetWindowingValue(Value : Single; Index, FFTSize : integer; Window : TWindowMode): Single;

  { Clips one integer Sample into one ShortInt. Same as using EnsureRange(), but much faster. }
  function Clip_8(Value: integer): ShortInt;
  { Clips one integer Sample into one SmallInt. Same as using EnsureRange(), but much faster. }
  function Clip_16(Value: Integer): SmallInt;
  { Clips one integer Sample into one 24Bit integer. Same as using EnsureRange(), but much faster. }
  function Clip_24(Value: Integer): Integer;
  { Clips one 64Bit integer Sample into one 32Bit Integer. Same as using EnsureRange(), but much faster. }
  function Clip_32(Value : Int64) : integer;
  { Clips one integer Sample into one WORD. Same as using EnsureRange(), but much faster. }
  function ClipWORD(Value: Integer): WORD;
  { Clips one Single Float Sample into a -1.0..1.0 Range. }
  function Clip_32F(Value : Single) : Single;

  { The Result of FFTSum is the same as when doing sqrt(sqr(real) + sqr(imag)). }
  function FFTSum(real, imag : Single) : Single;

  { This is needed to convert a 24 Bit Sample into a 32 Bit to do DSP on it. }
  function Cvt24BitTo32(Sample : T24BitSample) : integer;
  { This is used to convert a 32 Bit Sample back to 24 Bit. }
  function Cvt32BitTo24(Sample : integer) : T24BitSample;

  { Checks if a Buffer is aligned on 16 Byte. }
  function IsValidSSEBuffer(var Buffer: Pointer): Boolean;

var
  { Indicates if a CPU can execute MMX Instructions. }
  hasMMX,
  { Indicates if a CPU can execute extended MMX Instructions. }
  hasMMX2,
  { Indicates if a CPU can execute SSE Instructions. }
  hasSSE,
  { Indicates if a CPU can execute SSE2 Instructions. }
  hasSSE2,
  { Indicates if a CPU can execute SSE3 Instructions. }
  hasSSE3,
  { Indicates if a CPU can execute 3DNow Instructions. }
  has3DNow,
  { Indicates if a CPU can execute enhanced 3DNow Instructions. }
  has3DNowExt,
  { Indicates if a CPU can execute 64Bit Instructions. }
  has64 : Boolean;

implementation

function GetWindowingValue(Value : Single; Index, FFTSize : integer; Window : TWindowMode): Single;
begin
  Result := Value;
   case Window of
     wmRectangular    : Result := Value;
     wmBlackmanHarris : Result := Value * (0.35875 - 0.48829 * cos(TWO_PI * (Index + 0.5) / FFTSize) + 0.14128 * cos(TWO_PI * 2 * (Index + 0.5) / FFTSize) - 0.01168 * cos(TWO_PI * 3 * (Index + 0.5) / FFTSize));
     wmHamming        : Result := Value * (0.54 - (0.46 * cos(TWO_PI * Index / FFTSize)));
     wmHanning        : Result := Value * (0.50 - (0.50 * cos(TWO_PI * Index / FFTSize)));
     wmBlackman       : Result := Value * (0.42 - 0.50 * cos(TWO_PI * Index / FFTSize) + 0.08 * cos(FOUR_PI * Index / FFTSize));
     wmGaussian       : Result := Value * (exp(-5.0 / (sqr(FFTSize)) * (2 * Index - FFTSize) * (2 * Index - FFTSize)));
     wmBartlett:
     begin
       if Index < (FFTSize div 2) then Result := Value * (2 * Index / (FFTSize - 1))
                                  else Result := Value * (2 - (2 * Index / (FFTSize - 1)));
     end;
   end;
end;

function Cvt24BitTo32(Sample : T24BitSample) : integer;
begin
  Result := Sample.a0 + (Sample.a1 shl 8) + (Sample.a2 shl 16);
end;

function Cvt32BitTo24(Sample : integer) : T24BitSample;
begin
  Result.a0 := Sample;
  Result.a1 := Sample shr 8;
  Result.a2 := Sample shr 16;
end;

function Clip_8(Value: integer): ShortInt;
asm
        cmp       eax, 127
        jle       @@Lower
        mov       al, 127
        ret
@@Lower:
        cmp       eax, -128
        jge       @@Finished
        mov       al, -128
@@Finished:
end;

function Clip_16(Value: Integer): SmallInt;
asm
        cmp       eax, 32767
        jle       @@Lower
        mov       ax, 32767
        ret
@@Lower:
        cmp       eax, -32768
        jge       @@Finished
        mov       ax, -32768
@@Finished:
end;

function Clip_24(Value : integer) : integer;
asm
        cmp       eax, 8388607
        jle       @@Lower
        mov       eax, 8388607
        ret
@@Lower:
        cmp       eax, -8388608
        jge       @@Finished
        mov       eax, -8388608
@@Finished:
end;

function Clip_32(Value : Int64) : integer;
begin
  if Value > 2147483647 then Result := 2147483647
  else if Value < -2147483647 then Result := -2147483647
  else Result := integer(Value);
end;

function Clip_32F(Value : Single) : Single;
begin
  if Value > 1.0 then Result := 1.0
  else if Value < -1.0 then Result := -1.0
  else Result := Value;
end;

function ClipWORD(Value: Integer): WORD;
asm
        cmp       eax, 65535
        jle       @@Lower
        mov       eax, 65535
        ret
@@Lower:
        cmp       eax, 0
        jge       @@Finished
        mov       eax, 0
@@Finished:
end;

procedure CheckCPUID;
asm
        pushfd
        pop       eax
        mov       ecx, eax
        xor       eax, $200000
        push      eax
        popfd
        pushfd
        pop       eax
        cmp       eax,ecx
end;

procedure CheckMMX;
asm
        pushad
        mov       eax, $1
        db        $0F,$A2            // CPUID
        test      edx, $800000
        jz        @@End
        mov       hasMMX, 1
@@End:
        popad
        xor       eax,eax
end;

procedure CheckMMX2;
asm
        pushad
        mov       eax, $1
        db        $0F,$A2            // CPUID
        test      edx, $400000
        jz        @@CheckAMD
        mov       hasMMX2, 1
        jmp       @@End
@@CheckAMD:
        mov       eax, $80000000
        db        $0F,$A2            // CPUID
        cmp       eax, $80000000
        jbe       @@End
        mov       eax, $80000001
        db        $0F,$A2            // CPUID
        test      edx, $400000
        jz	  @@End
        mov       hasMMX2, 1
@@End:
        popad
        xor       eax,eax
end;

procedure CheckSSE;
asm
        pushad
        mov       eax, $1
        db        $0F,$A2            // CPUID
        test      edx, $2000000
        jz        @@End
        mov       hasSSE, 1
@@End:
        popad
        xor       eax,eax
end;

procedure CheckSSE2;
asm
        pushad
        mov       eax, $1
        db        $0F,$A2            // CPUID
        test      edx, $4000000
        jz        @@End
        mov       hasSSE2, 1
@@End:
        popad
        xor       eax,eax
end;

procedure CheckSSE3;
asm
        pushad
        mov       eax, $1
        db        $0F,$A2            // CPUID
        test      ecx, $1
        jz        @@End
        mov       hasSSE3, 1
@@End:
        popad
        xor       eax,eax
end;

procedure Check3DNow;
asm
        pushad
        mov       eax, $80000000
        db        $0F,$A2            // CPUID
        cmp       eax, $80000000
        jbe       @@End
        mov       eax, $80000001
        db        $0F,$A2            // CPUID
        test      edx, $80000000
        jz	      @@End
        mov       has3DNow, 1
@@End:
        popad
        xor       eax,eax
end;

procedure Check3DNowExt;
asm
        pushad
        mov       eax, $80000000
        db        $0F,$A2            // CPUID
        cmp       eax, $80000000
        jbe       @@End
        mov       eax, $80000001
        db        $0F,$A2            // CPUID
        test      edx, $40000000
        jz	      @@End
        mov       has3DNowExt, 1
@@End:
        popad
        xor       eax,eax
end;

procedure Check64;
asm
        pushad
        mov       eax, $80000000
        db        $0F,$A2            // CPUID
        cmp       eax, $80000000
        jbe       @@End
        mov       eax, $80000001
        db        $0F,$A2            // CPUID
        test      edx, $20000000
        jz	      @@End
        mov       has64, 1
@@End:
        popad
        xor       eax,eax
end;

procedure CheckCPU;
asm
        mov       ecx, eax
        mov       hasMMX, 0
        mov       hasMMX2, 0
        mov       hasSSE, 0
        mov       hasSSE2, 0
        mov       hasSSE3, 0
        mov       has3DNow, 0
        mov       has3DNowExt, 0
        mov       has64, 0
        call      CheckCPUID
        jz        @@END
        Call      CheckMMX
        Call      CheckMMX2
      {$IFDEF USE_SSE}
        Call      CheckSSE
        Call      CheckSSE2
        Call      CheckSSE3
      {$ENDIF}
        Call      Check3DNow
        Call      Check64
        Call      Check3DNowExt
@@End:
end;

function FFTSum(real, imag : Single) : Single;
begin
  Result := sqrt(sqr(real) + sqr(imag));
end;

function IsValidSSEBuffer(var Buffer: Pointer): Boolean;
begin
  Result := (Buffer <> nil) and (Cardinal(Buffer) mod 16 = 0);
end;

initialization
  CheckCPU;

end.
