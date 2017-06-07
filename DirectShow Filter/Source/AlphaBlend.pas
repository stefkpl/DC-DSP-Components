
    (*********************************************************************
     *  AlphaBlend.pas                                                   *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
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
     *  Blendit_ASM is taken from TBSMorphButton v1.0 (C) Babak Sateli   *
     *  MMX optimized Blendit_MMX (C) Milenko Mitrovic                   *
     *  MMX Version works more then 2x faster then the ASM Version :)    *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)

unit AlphaBlend;

interface

uses
  Windows, Graphics, dspUtils;

  procedure Blendit(bFr,bTo,bLn : TBitmap; Dens : LongInt);

implementation

const
  Mask0101 = $00FF00FF;
  Mask1010 = $FF00FF00;
  Mask0101MMX : array[0..1] of DWORD = (
    $00FF00FF, $00FF00FF
  );
  Mask1010MMX : array[0..1] of DWORD = (
    $FF00FF00, $FF00FF00
  );
  MMXDivider : array[0..3] of WORD = (
    32, 32, 32, 32
  );

var
  EBX, ESI, EDI, ESP, EBP,
  FinA,
  Dens1, Dens2 : Longint;
  fBlend : procedure (bFr,bTo,bLn : Pointer; Width,Height : integer; Dens : LongInt);

(*******************************************************************************)
procedure Blendit_ASM(bFr,bTo,bLn : Pointer ; Width,Height : Integer ; Dens : LongInt);
asm

  MOV &EBX, EBX
  MOV &EDI, EDI
  MOV &ESI, ESI
  MOV &ESP, ESP
  MOV &EBP, EBP

  MOV EBX, Dens
  MOV Dens1, EBX

  NEG BL
  ADD BL, $20
  MOV Dens2, EBX
  CMP Dens1, 0
  JZ  @Final
  MOV EDI, bFr
  MOV ESI, bTo
  MOV ECX, bLn

  MOV EAX, Width
  lea EAX, [EAX+EAX*2+3]
  AND EAX, $FFFFFFFC
  IMUL Height
  ADD EAX, EDI
  MOV FinA, EAX

  MOV EBP,EDI
  MOV ESP,ESI
  MOV ECX,ECX

@LOOPA:
  MOV  EAX, [EBP]
  MOV  EDI, [ESP]

  MOV  EBX, EAX
  AND  EAX, Mask1010
  AND  EBX, Mask0101
  SHR  EAX, 5
  IMUL EAX, Dens2
  IMUL EBX, Dens2

  MOV  ESI, EDI
  AND  EDI, Mask1010
  AND  ESI, Mask0101
  SHR  EDI, 5
  IMUL EDI, Dens1
  IMUL ESI, Dens1

  ADD  EAX, EDI
  ADD  EBX, ESI
  AND  EAX, Mask1010
  SHR  EBX, 5
  AND  EBX, Mask0101
  OR   EAX, EBX
  MOV [ECX], EAX

  ADD  EBP, 4
  ADD  ESP, 4
  ADD  ECX, 4

  CMP  EBP, FinA
  JNE  @LOOPA

@FINAL:

  MOV EBX, &EBX
  MOV EDI, &EDI
  MOV ESI, &ESI
  MOV ESP, &ESP
  MOV EBP, &EBP

end;
(*******************************************************************************)
procedure Blendit_MMX(bFr,bTo,bLn : Pointer; Width,Height : integer; Dens : LongInt);
asm
        push      edi
        push      esi
        emms
        movd      mm6, Dens
        punpcklwd mm6, mm6
        punpckldq mm6, mm6
        movq      mm7, MMXDivider
        psubd     mm7, mm6
        mov       edi, bFr
        mov       esi, bTo
        mov       ecx, bLn
        mov       eax, Width
        lea       eax, [eax+eax*2+3]
        and       eax, $FFFFFFFC
        imul      Height
        add       eax, edi
        movq      mm5, Mask0101MMX
        movq      mm4, Mask1010MMX
@LOOP:
        movq      mm0, [edi]
        movq      mm1, [esi]
        movq      mm2, mm0
        pand      mm0, mm4
        pand      mm2, mm5
        psrld     mm0, 5
        movq      mm3, mm1
        pand      mm1, mm4
        pand      mm3, mm5
        psrld     mm1, 5
        pmullw    mm0, mm7
        pmullw    mm2, mm7
        pmullw    mm1, mm6
        pmullw    mm3, mm6
        paddd     mm0, mm1
        paddd     mm2, mm3
        pand      mm0, mm4
        psrld     mm2, 5
        pand      mm2, mm5
        por       mm0, mm2
        movq      [ecx], mm0
        add       edi, 8
        add       esi, 8
        add       ecx, 8
        cmp       edi, eax
        jl        @LOOP
@END:
        emms
        pop       edi
        pop       esi
end;
(*******************************************************************************)
procedure Blendit(bFr,bTo,bLn : TBitmap; Dens : LongInt);
begin
  fBlend(bFr.Scanline[bFr.Height-1],bTo.Scanline[bTo.Height-1],bLn.Scanline[bLn.Height-1],bFr.Width,bFr.Height,Dens);
end;
(*******************************************************************************)
initialization
  if hasMMX then @fBlend := @BlendIt_MMX else @fBlend := @BlendIt_ASM;

end.
