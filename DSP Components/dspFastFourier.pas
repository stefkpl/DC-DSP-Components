
    (*********************************************************************
     *  dspFastFourier.pas                                               *
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
     *  Based on D.J.Bernstein큦 Split Radix FFT C+ Code v0.76 (Linux).  *
     *  Available at http://cr.yp.to/djbfft.html.                        *
     *                                                                   *
     *  On my AthlonXP 1800+ CPU the FFT Results are the follwing :      *
     *  3DNowExt : FFT = 28% and IFFT = 24% faster then the FPU Code     *
     *  3DNow    : FFT = 18% and IFFT =  8% faster then the FPU Code     *
     *  SSE      : FFT = 17% and IFFT =  8% faster then the FPU Code     *
     *                                                                   *
     *  (C) 2003 Milenko Mitrovic <dcoder@dsp-worx.de>                   *
     *                                                                   *
     *********************************************************************)
{
  @abstract(Fast Fourier Transform Unit that can do Transformations
            up to a Size of 8192 Samples (<u>Power of 2 only!</u>).
            Optimized for SSE, 3DNow, 3DNowExt and CPUs that only supports FPU
            instructions. This Routine chooses automatically the fastest one.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspFastFourier;

interface

uses
  Classes, Math, dspConst, dspUtils, Windows;

type
  { TDCFFT - Fast Fourier Transform Component that can do Transformations up to
    a Size of 8192 Samples (<u>Power of 2 only!</u>). Optimized for SSE, 3DNow,
    3DNowExt and CPUs that only supports FPU instructions. This Routine chooses
    automatically the fastest one. }
  TDCFFT = class(TComponent)
  private
    {@exclude}
    fSwapBuffer : Pointer;
    {@exclude}
    fCplx : PComplexArray;
    {@exclude}
    fScale : Boolean;
    {@exclude}
    fReOrder : Boolean;
    {@exclude}
    fFFTSize : TDCFFTSize;
    {@exclude}
    function GetFFTSize : integer;
  public
    { TDCFFT Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCFFT Destructor }
    destructor Destroy; override;
    { Does a forward Fourier Transform of the Samples in Complex. }
    procedure FFT;
    { Does a reverse Fourier Transform of the Samples in Complex. }
    procedure IFFT;
    { Pointer to an Array of 8192 Real and Imag Values that is used for the
      forward and inverse Transformation. This Buffer is Aligned on a 16 Byte
      boundary. }
    property Complex : PComplexArray read fCplx write fCplx;
    { Sets every Value to Zero. When creating the Component, this procedure is
      automatically called, so it큦 not needed to use it after creating. }
    procedure Flush;
  published
    { Normally this is needed for the Inverse Transform to Scaledown the Samples.
      Sometimes it큦 not needed. In that case set Scale to False. default = True }
    property Scale : Boolean read fScale write fScale;
    { Since this is a 2-2-4 Split Radix FFT, Samples are out of Order after the FFT.
      If Enabled then the Samples will be Reordered after the FFT. If ReOrder was
      Enabled for the forward FFT, then it MUST also be Enabled for the inverse FFT !
      Sometimes it큦 not needed to Reorder the Samples, so it can be disabled.
      default = True }
    property ReOrder : Boolean read fReOrder write fReOrder;
    { Sets the FFT Size that will be used for the Transformation. Can be one of
      TDCFFTSize Values. eg: fts2048 for a 2048 Point FFT. default = fts512 }
    property FFTSize : TDCFFTSize read fFFTSize write fFFTSize;
    { returns the Current FFT Size as an integer Value. }
    property FFTSizeInt : integer read GetFFTSize;
  end;

  { This procedure is made to use your own allocated Buffer to do a Fast Fourier
    Transform without using the TDCFFT Class. Values are the same as for the
    Component. When using a SSE capable CPU, make sure that your Buffer
    (Cplx:PComplexArray) is Aligned on 16 Byte Boundary or you will recieve an
    Error called "Privileged instruction". The Complex Buffer in the TDCFFT Class
    is already Aligned, so it큦 appricated to use this Class. SwapBuffer is used
    when ReOrder is used to prevent the temporary allocation/freeing of Memry used
    for the Reordering. Create a Buffer on your own, pass it to this function and
    Release it when the FFT is not needed anymore. If no Buffer is specified, the
    procedure will allocate the memory on it큦 own. For fastest Memory Movements
    this Buffer should be initialized using GetAlignedMemory. }
  procedure dspDoFFT(Cplx : PComplexArray; FFTSize : integer;
                     Inverse,Scale,ReOrder : Boolean; SwapBuffer : Pointer = nil);

var
  { Used to get the Real Index of a Frequency in a 2 Point FFT. }
  RevBins2 : array[0..1] of WORD = (0,1);
  { Used to get the Real Index of a Frequency in a 4 Point FFT. }
  RevBins4 : array[0..3] of WORD;
  { Used to get the Real Index of a Frequency in a 8 Point FFT. }
  RevBins8 : array[0..7] of WORD;
  { Used to get the Real Index of a Frequency in a 16 Point FFT. }
  RevBins16 : array[0..15] of WORD;
  { Used to get the Real Index of a Frequency in a 32 Point FFT. }
  RevBins32 : array[0..31] of WORD;
  { Used to get the Real Index of a Frequency in a 64 Point FFT. }
  RevBins64 : array[0..63] of WORD;
  { Used to get the Real Index of a Frequency in a 128 Point FFT. }
  RevBins128 : array[0..127] of WORD;
  { Used to get the Real Index of a Frequency in a 256 Point FFT. }
  RevBins256 : array[0..255] of WORD;
  { Used to get the Real Index of a Frequency in a 512 Point FFT. }
  RevBins512 : array[0..511] of WORD;
  { Used to get the Real Index of a Frequency in a 1024 Point FFT. }
  RevBins1024 : array[0..1023] of WORD;
  { Used to get the Real Index of a Frequency in a 2048 Point FFT. }
  RevBins2048 : array[0..2047] of WORD;
  { Used to get the Real Index of a Frequency in a 4096 Point FFT. }
  RevBins4096 : array[0..4095] of WORD;
  { Used to get the Real Index of a Frequency in a 8192 Point FFT. }
  RevBins8192 : array[0..8191] of WORD;

implementation

uses SysUtils;

var
  d32 : array[0..6] of TComplex;
  d64 : array[0..14] of TComplex;
  d128 : array[0..30] of TComplex;
  d256 : array[0..62] of TComplex;
  d512 : array[0..126] of TComplex;
  d1024 : array[0..126] of TComplex;
  d2048 : array[0..254] of TComplex;
  d4096 : array[0..510] of TComplex;
  d8192 : array[0..1022] of TComplex;

  fft2    : procedure (Cplx : PComplexArray);
  ifft2   : procedure (Cplx : PComplexArray);
  fft4    : procedure (Cplx : PComplexArray);
  ifft4   : procedure (Cplx : PComplexArray);
  fft8    : procedure (Cplx : PComplexArray);
  ifft8   : procedure (Cplx : PComplexArray);
  fft16   : procedure (Cplx : PComplexArray);
  ifft16  : procedure (Cplx : PComplexArray);
  fft32   : procedure (Cplx : PComplexArray);
  ifft32  : procedure (Cplx : PComplexArray);
  fft64   : procedure (Cplx : PComplexArray);
  ifft64  : procedure (Cplx : PComplexArray);
  fft128  : procedure (Cplx : PComplexArray);
  ifft128 : procedure (Cplx : PComplexArray);
  fft256  : procedure (Cplx : PComplexArray);
  ifft256 : procedure (Cplx : PComplexArray);
  fft512  : procedure (Cplx : PComplexArray);
  ifft512 : procedure (Cplx : PComplexArray);
  fft1024  : procedure (Cplx : PComplexArray);
  ifft1024  : procedure (Cplx : PComplexArray);
  fft2048  : procedure (Cplx : PComplexArray);
  ifft2048  : procedure (Cplx : PComplexArray);
  fft4096  : procedure (Cplx : PComplexArray);
  ifft4096  : procedure (Cplx : PComplexArray);
  fft8192  : procedure (Cplx : PComplexArray);
  ifft8192  : procedure (Cplx : PComplexArray);

  d323DNow,
  d643DNow,
  d1283DNow,
  d2563DNow,
  d5123DNow,
  d10243DNow,
  d20483DNow,
  d40963DNow,
  d81923DNow : PComplexArray3DNow;

  SqrtHalf : Single = 0.70710678118654;
  d16re : Single = 0.92387953251128;
  d16im : Single = 0.38268343236508;
  nd16re : Single = -0.92387953251128;
  nd16im : Single = -0.38268343236508;

  SqrtHalf3DNow : array[0..1] of Single = (
    0.70710678118654, 0.70710678118654
  );

  d16re3DNow : array[0..1] of Single = (
    0.92387953251128, 0.92387953251128
  );

  d16im3DNow : array[0..1] of Single = (
    0.38268343236508, 0.38268343236508
  );

  Invert3DNow_H : array[0..1] of Cardinal = (
    $80000000, 0
  );

  d32SSE,
  d64SSE,
  d128SSE,
  d256SSE,
  d512SSE,
  d1024SSE,
  d2048SSE,
  d4096SSE,
  d8192SSE : PComplexArraySSE;
  Params : PChar;

  SqrtHalfSSE : array[0..3] of Single = (
    0.70710678118654, 0.70710678118654, 0.70710678118654, 0.70710678118654
  );

  d16reSSE : array[0..3] of Single = (
    0.92387953251128, 0.92387953251128, 0.92387953251128, 0.92387953251128
  );

  d16imSSE : array[0..3] of Single = (
    0.38268343236508, 0.38268343236508, 0.38268343236508, 0.38268343236508
  );

  InvertSSE_HL : array[0..3] of Cardinal = (
    $80000000, 0, 0, $80000000
  );

  InvertSSE_MHL : array[0..3] of Cardinal = (
    0, $80000000, $80000000, 0
  );

  InvertSSE_LL : array[0..3] of Cardinal = (
    0, 0, $80000000, $80000000
  );

procedure InitRevBitTable;

  procedure CopyFromTo(pFrom, pTo : PWORDArray; x,z,m : PInteger);
  var
    i : integer;
  begin
    if x^ = 0 then x^ := 1 else x^ := 0;
    z^ := z^ shl 1 - x^;
    m^ := m^ shl 1;
    for i := 0 to (m^ shr 1 - 1) do
    begin
      pTo^[i]            := pFrom^[i] * 2;
      pTo^[i + m^ shr 1] := pFrom^[i] * 2;
    end;
    for i := z^ to (m^ - (m^ shr 1 - (z^ - 1))) do inc(pTo^[i]);
  end;

var
  z : integer;
  x : integer;
  m : integer;
begin
  x := 1;
  z := 1;
  m := 2;
  CopyFromTo(@RevBins2,@RevBins4,@x,@z,@m);
  CopyFromTo(@RevBins4,@RevBins8,@x,@z,@m);
  CopyFromTo(@RevBins8,@RevBins16,@x,@z,@m);
  CopyFromTo(@RevBins16,@RevBins32,@x,@z,@m);
  CopyFromTo(@RevBins32,@RevBins64,@x,@z,@m);
  CopyFromTo(@RevBins64,@RevBins128,@x,@z,@m);
  CopyFromTo(@RevBins128,@RevBins256,@x,@z,@m);
  CopyFromTo(@RevBins256,@RevBins512,@x,@z,@m);
  CopyFromTo(@RevBins512,@RevBins1024,@x,@z,@m);
  CopyFromTo(@RevBins1024,@RevBins2048,@x,@z,@m);
  CopyFromTo(@RevBins2048,@RevBins4096,@x,@z,@m);
  CopyFromTo(@RevBins4096,@RevBins8192,@x,@z,@m);
end;

procedure InitSinCosTable;

  procedure SinCosTable(n : Integer; s : PComplexArray);
  var
    tmp : Double;
    i : integer;
    m : integer;
    fs,fc : Extended;
  begin
    if n > 512 then m := n shr 3
               else m := n shr 2;
    tmp := 2 * PI / n;
    for i := 1 to m -1 do
    begin
      SinCos(tmp * i,fs,fc);
      s^[i-1].re := fc;
      s^[i-1].im := fs;
    end;
  end;

begin
  SinCosTable(32,@d32);
  SinCosTable(64,@d64);
  SinCosTable(128,@d128);
  SinCosTable(256,@d256);
  SinCosTable(512,@d512);
  SinCosTable(1024,@d1024);
  SinCosTable(2048,@d2048);
  SinCosTable(4096,@d4096);
  SinCosTable(8192,@d8192);
end;

procedure c2_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm1, mm0
        movq      mm2, [eax+8]
        pfadd     mm0, mm2
        pfsub     mm1, mm2
        movq      [eax], mm0
        movq      [eax+8], mm1
end;

procedure c4_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm1, mm0
        movq      mm2, [eax+8]
        movq      mm3, mm2
        movq      mm4, [eax+16]
        movq      mm5, [eax+24]
        pfadd     mm0, mm4
        pfadd     mm2, mm5
        pfsub     mm1, mm4
        pfsub     mm3, mm5
        movq      mm4, mm0
        pfadd     mm0, mm2
        pfsub     mm4, mm2
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      mm0, mm1
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        movq      mm4, mm3
        pxor      mm3, Invert3DNow_H
        pfadd     mm0, mm3
        movq      [eax+16], mm0
        pxor      mm4, Invert3DNow_H
        pfsub     mm1, mm4
        movq      [eax+24], mm1
end;

procedure c8_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm4, [eax+32]
        movq      mm6, mm0
        pfsub     mm0, mm4
        pfadd     mm6, mm4       
        movq      mm2, [eax+16]  
        movq      mm5, [eax+48]  
        movq      mm7, mm2       
        pfsub     mm2, mm5       
        pfadd     mm7, mm5       
        movq      [eax+16], mm7
        punpckldq mm1, mm2
        punpckhdq mm2, mm1       
        movq      mm7, mm0       
        pfsub     mm7, mm2       
        pfadd     mm0, mm2       
        movq      mm1, [eax+8]   
        movq      mm5, [eax+40]  
        movq      mm4, mm1       
        pfsub     mm1, mm5       
        pfadd     mm4, mm5       
        movq      [eax+8], mm4
        movq      mm3, [eax+24]  
        movq      mm2, [eax+56]  
        movq      mm4, mm3       
        pfsub     mm3, mm2       
        pfadd     mm4, mm2       
        movq      [eax+24], mm4
        punpckldq mm2, mm3
        punpckhdq mm3, mm2       
        movq      mm2, mm1       
        pfsub     mm2, mm3       
        pfadd     mm1, mm3       
        punpckldq mm4, mm2
        punpckhdq mm2, mm4       
        movq      mm4, mm1       
        pfadd     mm4, mm2       
        pfsub     mm1, mm2       
        pfmul     mm4, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm3, mm1       
        punpckhdq mm3, mm4       
        punpckldq mm2, mm7
        punpckhdq mm7, mm2       
        movq      mm2, mm7       
        punpckhdq mm2, mm0       
        movq      mm5, mm2       
        pfsub     mm5, mm3       
        pfadd     mm2, mm3       
        punpckldq mm3, mm2
        punpckhdq mm2, mm3       
        movq      mm3, mm5       
        punpckldq mm3, mm2
        punpckhdq mm2, mm5
        movq      [eax+32], mm3
        movq      [eax+40], mm2
        punpckldq mm0, mm7       
        punpckldq mm4, mm1       
        movq      mm5, mm0       
        pfsub     mm5, mm4       
        pfadd     mm0, mm4       
        punpckldq mm3, mm0
        punpckhdq mm0, mm3       
        movq      mm3, mm5       
        punpckldq mm3, mm0
        punpckhdq mm0, mm5
        movq      [eax+48], mm0
        movq      [eax+56], mm3
        movq      mm2, [eax+16]  
        movq      mm5, mm6       
        pfadd     mm6, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+8]   
        movq      mm4, mm1       
        movq      mm3, [eax+24]  
        pfadd     mm1, mm3       
        pfsub     mm4, mm3       
        movq      mm2, mm6       
        pfadd     mm6, mm1       
        pfsub     mm2, mm1       
        movq      [eax], mm6
        movq      [eax+8], mm2
        movq      mm7, mm5       
        punpckldq mm1, mm4
        punpckhdq mm4, mm1       
        pfsub     mm7, mm4       
        pfadd     mm5, mm4       
        punpckldq mm1, mm5
        punpckhdq mm5, mm1       
        movq      mm4, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm4      
        movq      [eax+16], mm7
        movq      [eax+24], mm5
end;

procedure c16_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm1, mm0
        movq      mm2, [eax+64]
        pfsub     mm0, mm2         
        pfadd     mm1, mm2
        movq      [eax], mm1
        movq      mm1, [eax+32]
        movq      mm2, mm1
        movq      mm3, [eax+96]
        pfsub     mm1, mm3        
        pfadd     mm2, mm3
        movq      [eax+32], mm2
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm0
        pfsub     mm0, mm1       
        pfadd     mm2, mm1       
        punpckldq mm1, mm0
        punpckhdq mm0, mm1
        movq      mm1, mm2       
        punpckldq mm1, mm0       
        punpckhdq mm0, mm2       
        movq      [eax+64], mm0
        movq      [eax+96], mm1
        movq      mm2, [eax+16]  
        movq      mm3, mm2       
        movq      mm4, [eax+48]  
        movq      mm5, mm4       
        movq      mm6, [eax+80]  
        movq      mm7, [eax+112] 
        pfsub     mm2, mm6         
        pfsub     mm4, mm7         
        pfadd     mm3, mm6
        pfadd     mm5, mm7
        movq      [eax+16], mm3
        movq      [eax+48], mm5
        punpckldq mm3, mm4
        punpckhdq mm4, mm3
        movq      mm3, mm2
        pfadd     mm3, mm4        
        pfsub     mm2, mm4        
        punpckldq mm1, mm3
        punpckhdq mm3, mm1
        movq      mm1, mm2
        pfsub     mm1, mm3        
        pfadd     mm2, mm3        
        pfmul     mm1, SqrtHalf3DNow
        pfmul     mm2, SqrtHalf3DNow
        movq      mm3, mm1      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm3      
        movq      [eax+80], mm1
        movq      [eax+112], mm2
        movq      mm1, [eax+8]
        movq      mm2, mm1
        movq      mm3, [eax+40]
        movq      mm4, mm3
        movq      mm6, [eax+72]
        movq      mm7, [eax+104]
        pfsub     mm1, mm6        
        pfadd     mm2, mm6
        pfsub     mm3, mm7        
        pfadd     mm4, mm7
        movq      [eax+8], mm2
        movq      [eax+40], mm4
        punpckldq mm0, mm3
        punpckhdq mm3, mm0
        movq      mm0, mm1
        pfsub     mm1, mm3  
        pfadd     mm0, mm3  
        movq      mm2, mm0     
        movq      mm3, mm1     
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16re3DNow  
        pfmul     mm2, d16im3DNow  
        pfmul     mm3, d16im3DNow  
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        pfsub     mm1, mm2     
        pfadd     mm0, mm3     
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm0     
        punpckldq mm0, mm1     
        punpckhdq mm1, mm2     
        movq      [eax+72], mm1
        movq      [eax+104], mm0
        movq      mm1, [eax+24]  
        movq      mm2, mm1       
        movq      mm3, [eax+56]  
        movq      mm4, mm3       
        movq      mm6, [eax+88]  
        movq      mm7, [eax+120] 
        pfsub     mm1, mm6        
        pfadd     mm2, mm6
        pfsub     mm3, mm7        
        pfadd     mm4, mm7
        movq      [eax+24], mm2
        movq      [eax+56], mm4
        punpckldq mm2, mm3
        punpckhdq mm3, mm2
        movq      mm2, mm1   
        pfsub     mm2, mm3   
        pfadd     mm1, mm3   
        movq      mm3, mm1     
        movq      mm0, mm2     
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16re3DNow  
        pfmul     mm2, d16im3DNow  
        pfmul     mm3, d16im3DNow  
        punpckldq mm4, mm1
        punpckhdq mm1, mm4
        punpckldq mm4, mm3
        punpckhdq mm3, mm4
        pfsub     mm2, mm1     
        pfadd     mm3, mm0     
        movq      mm0, mm3     
        punpckhdq mm0, mm2     
        punpckldq mm2, mm3     
        movq      [eax+88], mm2
        movq      [eax+120], mm0

        movq      mm0, [eax]     
        movq      mm4, [eax+32]  
        movq      mm6, mm0       
        pfsub     mm0, mm4       
        pfadd     mm6, mm4       
        movq      mm2, [eax+16]  
        movq      mm5, [eax+48]  
        movq      mm7, mm2       
        pfsub     mm2, mm5       
        pfadd     mm7, mm5       
        movq      [eax+16], mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm7, mm0       
        pfsub     mm7, mm2       
        pfadd     mm0, mm2       
        movq      mm1, [eax+8]   
        movq      mm5, [eax+40]  
        movq      mm4, mm1       
        pfsub     mm1, mm5       
        pfadd     mm4, mm5       
        movq      [eax+8], mm4
        movq      mm3, [eax+24]  
        movq      mm2, [eax+56]  
        movq      mm4, mm3       
        pfsub     mm3, mm2       
        pfadd     mm4, mm2       
        movq      [eax+24], mm4
        punpckldq mm2, mm3
        punpckhdq mm3, mm2
        movq      mm2, mm1       
        pfsub     mm2, mm3       
        pfadd     mm1, mm3       
        punpckldq mm4, mm2
        punpckhdq mm2, mm4
        movq      mm4, mm1       
        pfadd     mm4, mm2       
        pfsub     mm1, mm2       
        pfmul     mm4, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm3, mm1       
        punpckhdq mm3, mm4       
        punpckldq mm2, mm7
        punpckhdq mm7, mm2
        movq      mm2, mm7       
        punpckhdq mm2, mm0       
        movq      mm5, mm2       
        pfsub     mm5, mm3       
        pfadd     mm2, mm3       
        punpckldq mm3, mm2
        punpckhdq mm2, mm3
        movq      mm3, mm5       
        punpckldq mm3, mm2
        punpckhdq mm2, mm5
        movq      [eax+32], mm3
        movq      [eax+40], mm2
        punpckldq mm0, mm7       
        punpckldq mm4, mm1       
        movq      mm5, mm0       
        pfsub     mm5, mm4       
        pfadd     mm0, mm4       
        punpckldq mm3, mm0
        punpckhdq mm0, mm3
        movq      mm3, mm5       
        punpckldq mm3, mm0
        punpckhdq mm0, mm5
        movq      [eax+48], mm0
        movq      [eax+56], mm3
        movq      mm2, [eax+16]  
        movq      mm5, mm6       
        pfadd     mm6, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+8]   
        movq      mm4, mm1       
        movq      mm3, [eax+24]  
        pfadd     mm1, mm3       
        pfsub     mm4, mm3       
        movq      mm2, mm6       
        pfadd     mm6, mm1       
        pfsub     mm2, mm1       
        movq      [eax], mm6
        movq      [eax+8], mm2
        movq      mm7, mm5       
        punpckldq mm0, mm4
        punpckhdq mm4, mm0
        pfsub     mm7, mm4       
        pfadd     mm5, mm4       
        punpckldq mm0, mm5
        punpckhdq mm5, mm0
        movq      mm4, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm4      
        movq      [eax+16], mm7
        movq      [eax+24], mm5

        movq      mm0, [eax+64]     
        movq      mm2, [eax+80]  
        movq      mm5, mm0       
        pfadd     mm0, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+72]   
        movq      mm6, mm1       
        movq      mm3, [eax+88]  
        pfadd     mm1, mm3       
        pfsub     mm6, mm3       
        movq      mm2, mm0       
        pfadd     mm0, mm1       
        pfsub     mm2, mm1       
        movq      [eax+64], mm0
        movq      [eax+72], mm2
        movq      mm7, mm5       
        punpckldq mm0, mm6
        punpckhdq mm6, mm0
        pfsub     mm7, mm6       
        pfadd     mm5, mm6       
        punpckldq mm0, mm5
        punpckhdq mm5, mm0
        movq      mm6, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm6      
        movq      [eax+80], mm7
        movq      [eax+88], mm5

        movq      mm0, [eax+96]     
        movq      mm2, [eax+112]  
        movq      mm5, mm0       
        pfadd     mm0, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+104]   
        movq      mm6, mm1       
        movq      mm3, [eax+120]  
        pfadd     mm1, mm3       
        pfsub     mm6, mm3       
        movq      mm2, mm0       
        pfadd     mm0, mm1       
        pfsub     mm2, mm1       
        movq      [eax+96], mm0
        movq      [eax+104], mm2
        movq      mm7, mm5       
        punpckldq mm0, mm6
        punpckhdq mm6, mm0
        pfsub     mm7, mm6       
        pfadd     mm5, mm6       
        punpckldq mm0, mm5
        punpckhdq mm5, mm0
        movq      mm6, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm6      
        movq      [eax+112], mm7
        movq      [eax+120], mm5
end;

procedure cpass_3DNow(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       ebx, DWORD PTR [ecx+ecx]
        mov       DWORD PTR [esp+8], eax
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, eax
        lea       edi, DWORD PTR [ecx-1]
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebp, DWORD PTR [eax+ecx]
        lea       esi, DWORD PTR [ebx+ecx]

        movq      mm0, [eax]     
        movq      mm1, [ebp]     
        movq      mm2, [ebx]     
        movq      mm3, [esi]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2      
        pfadd     mm5, mm3      
        movq      [eax], mm4
        movq      [ebp], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm0
        punpckhdq mm0, mm7
        movq      mm1, mm2      
        punpckldq mm1, mm0      
        punpckhdq mm0, mm2      
        movq      [ebx], mm0
        movq      [esi], mm1

        movq      mm0, [eax+8]     
        movq      mm1, [ebp+8]     
        movq      mm2, [ebx+8]     
        movq      mm3, [esi+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+8], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+8], mm1
        movq      [esi+8], mm2
        mov       DWORD PTR [esp+4], ecx
        jmp       @@B92
@@B93:
        mov       ebp, DWORD PTR [esp]
        mov       ebx, ecx
        mov       eax, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebp
        lea       ebp, DWORD PTR [ebp+eax]
        lea       esi, DWORD PTR [ecx+eax]
@@B92:
        mov       eax, DWORD PTR [esp+8]
        lea       ecx, DWORD PTR [eax+16]
        mov       DWORD PTR [esp], ecx
        lea       ecx, DWORD PTR [ebx+16]
        movq      mm0, [eax+16]     
        movq      mm1, [ebp+16]     
        movq      mm2, [ebx+16]     
        movq      mm3, [esi+16]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+16], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+16], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm6, [edx+16]
        movq      mm7, [edx+24]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+16], mm1
        movq      [esi+16], mm2
        add       edx, 32
        dec       edi
        movq      mm0, [eax+24]     
        movq      mm1, [ebp+24]     
        movq      mm2, [ebx+24]     
        movq      mm3, [esi+24]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+24], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+24], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+24], mm1
        movq      [esi+24], mm2
        jne       @@B93
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure cpassbig_3DNow(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 32
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        lea       ebx, DWORD PTR [ecx+ecx]
        add       ebx, ebx
        sub       ebx, ecx
        add       ebx, ebx

        movq      mm0, [eax]     
        movq      mm1, [eax+ebp*8]     
        movq      mm2, [eax+esi*8]     
        movq      mm3, [eax+ebx*8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2      
        pfadd     mm5, mm3      
        movq      [eax], mm4
        movq      [eax+ebp*8], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm0
        punpckhdq mm0, mm7
        movq      mm1, mm2      
        punpckldq mm1, mm0      
        punpckhdq mm0, mm2      
        movq      [eax+esi*8], mm0
        movq      [eax+ebx*8], mm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+12], edx
        mov       DWORD PTR [esp+16], eax
        mov       DWORD PTR [esp+8], ecx
@@B122:
        lea       ecx, DWORD PTR [eax+8]

        movq      mm0, [eax+8]     
        movq      mm1, [eax+ebp*8+8]     
        movq      mm2, [eax+esi*8+8]     
        movq      mm3, [eax+ebx*8+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [eax+ebp*8+8], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [eax+esi*8+8], mm1
        movq      [eax+ebx*8+8], mm2

        mov       eax, ecx
        add       edx, 16
        dec       edi
        jne       @@B122
@@B123:
        mov       DWORD PTR [esp], ecx
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+8], ecx
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [ecx+8]
        mov       DWORD PTR [esp+4], edi

        movq      mm0, [ecx+8]
        movq      mm1, [eax+ebp*8+8]
        movq      mm2, [eax+esi*8+8]
        movq      mm3, [eax+ebx*8+8]
        movq      mm4, mm0
        movq      mm5, mm1
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2
        pfadd     mm5, mm3
        movq      [ecx+8], mm4
        movq      [eax+ebp*8+8], mm5
        punpckldq mm2, mm1
        punpckhdq mm1, mm2      
        movq      mm2, mm0
        pfsub     mm2, mm1      
        pfadd     mm0, mm1      
        punpckldq mm1, mm0
        punpckhdq mm0, mm1      
        movq      mm3, mm2      
        pfsub     mm3, mm0      
        pfadd     mm0, mm2      
        pfmul     mm0, SqrtHalf3DNow
        pfmul     mm3, SqrtHalf3DNow
        movq      mm1, mm0
        punpckhdq mm0, mm3      
        punpckldq mm3, mm1      
        movq      [eax+esi*8+8], mm3
        movq      [eax+ebx*8+8], mm0

        add       edx, -16
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+20], ecx
        mov       DWORD PTR [esp+24], edx
        mov       DWORD PTR [esp+28], edi
        mov       eax, edi
@@B124:
        lea       edi, DWORD PTR [eax+8]

        movq      mm0, [eax+8]     
        movq      mm1, [eax+ebp*8+8]     
        movq      mm2, [eax+esi*8+8]     
        movq      mm3, [eax+ebx*8+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [eax+ebp*8+8], mm5
        movq      mm2, mm0      
        punpckldq mm7, mm1
        punpckhdq mm1, mm7
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        movq      mm6, [edx+8]
        movq      mm7, [edx]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [eax+esi*8+8], mm1
        movq      [eax+ebx*8+8], mm2

        mov       eax, edi
        add       edx, -16
        dec       ecx
        jne       @@B124
@@B125:
        add       esp, 32
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure u2_3DNow(a : PComplexArray);
begin
  c2_3DNow(a);
end;

procedure u4_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        punpckldq mm1, mm6
        punpckhdq mm6, mm1
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+16], mm1
        movq      [eax+24], mm5
end;

procedure u8_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        punpckldq mm1, mm6
        punpckhdq mm6, mm1
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+24], mm5

        movq      mm0, [eax+32]  
        movq      mm7, [eax+40]  
        movq      mm2, [eax+48]  
        movq      mm3, [eax+56]  
        movq      mm4, mm0       
        movq      mm5, mm2       
        pfadd     mm4, mm7       
        pfsub     mm0, mm7
        pfadd     mm5, mm3       
        pfsub     mm2, mm3
        movq      mm3, mm4       
        pfadd     mm3, mm5                    
        punpckldq mm6, mm4
        punpckhdq mm4, mm6
        movq      mm6, mm4       
        punpckldq mm6, mm5       
        punpckhdq mm5, mm4       
        pfsub     mm6, mm5       
        movq      mm7, mm1
        pfsub     mm1, mm6
        movq      [eax+48], mm1
        movq      mm1, [eax]
        pfsub     mm1, mm3
        movq      [eax+32], mm1
        pfadd     mm7, mm6
        movq      [eax+16], mm7
        movq      mm1, [eax]
        pfadd     mm1, mm3
        movq      [eax], mm1
        movq      mm1, mm0
        movq      mm4, mm1
        punpckldq mm4, mm2 
        punpckhdq mm1, mm2 
        pfadd     mm1, mm4
        pfmul     mm1, SqrtHalf3DNow  
        punpckldq mm7, mm0
        punpckhdq mm0, mm7
        movq      mm4, mm0
        punpckldq mm4, mm2 
        punpckhdq mm0, mm2 
        pfsub     mm4, mm0
        pfmul     mm4, SqrtHalf3DNow  
        punpckldq mm7, mm4
        punpckhdq mm4, mm7
        movq      mm2, mm4
        pfadd     mm2, mm1   
        pfsub     mm4, mm1   
        punpckldq mm7, mm4
        punpckhdq mm4, mm7
        movq      mm3, [eax+24]
        movq      mm7, mm3
        movq      mm5, [eax+8]
        movq      mm6, mm5
        pfsub     mm3, mm4
        pfsub     mm5, mm2
        movq      [eax+56], mm3
        movq      [eax+40], mm5
        pfadd     mm7, mm4
        pfadd     mm6, mm2
        movq      [eax+24], mm7
        movq      [eax+8], mm6
end;

procedure u16_3DNow(a : PComplexArray);
asm
        movq      mm0, [eax+64]
        movq      mm5, mm0
        movq      mm1, [eax+72]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+88] 
        movq      mm6, mm3      
        movq      mm1, [eax+80] 
        pfadd     mm3, mm1      
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        punpckldq mm1, mm6
        punpckhdq mm6, mm1
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax+64], mm0
        movq      [eax+72], mm4
        movq      [eax+80], mm1
        movq      [eax+88], mm5

        movq      mm0, [eax+96]
        movq      mm5, mm0
        movq      mm1, [eax+104]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+120] 
        movq      mm6, mm3      
        movq      mm1, [eax+112] 
        pfadd     mm3, mm1      
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        punpckldq mm1, mm6
        punpckhdq mm6, mm1
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax+96], mm0
        movq      [eax+104], mm4
        movq      [eax+112], mm1
        movq      [eax+120], mm5

        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        punpckldq mm1, mm6
        punpckhdq mm6, mm1
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+24], mm5

        movq      mm0, [eax+32]  
        movq      mm7, [eax+40]  
        movq      mm2, [eax+48]  
        movq      mm3, [eax+56]  
        movq      mm4, mm0       
        movq      mm5, mm2       
        pfadd     mm4, mm7       
        pfsub     mm0, mm7
        pfadd     mm5, mm3       
        pfsub     mm2, mm3
        movq      mm3, mm4       
        pfadd     mm3, mm5                    
        punpckldq mm6, mm4
        punpckhdq mm4, mm6
        movq      mm6, mm4       
        punpckldq mm6, mm5       
        punpckhdq mm5, mm4       
        pfsub     mm6, mm5       
        movq      mm7, mm1
        pfsub     mm1, mm6
        movq      [eax+48], mm1
        movq      mm1, [eax]
        pfsub     mm1, mm3
        movq      [eax+32], mm1
        pfadd     mm7, mm6
        movq      [eax+16], mm7
        movq      mm1, [eax]
        pfadd     mm1, mm3
        movq      [eax], mm1
        movq      mm1, mm0
        movq      mm4, mm1
        punpckldq mm4, mm2 
        punpckhdq mm1, mm2 
        pfadd     mm1, mm4
        pfmul     mm1, SqrtHalf3DNow  
        punpckldq mm3, mm0
        punpckhdq mm0, mm3
        movq      mm4, mm0
        punpckldq mm4, mm2 
        punpckhdq mm0, mm2 
        pfsub     mm4, mm0
        pfmul     mm4, SqrtHalf3DNow  
        punpckldq mm3, mm4
        punpckhdq mm4, mm3
        movq      mm2, mm4
        pfadd     mm2, mm1   
        pfsub     mm4, mm1   
        punpckldq mm3, mm4
        punpckhdq mm4, mm3
        movq      mm3, [eax+24]
        movq      mm7, mm3
        movq      mm5, [eax+8]
        movq      mm6, mm5
        pfsub     mm3, mm4
        pfsub     mm5, mm2
        movq      [eax+56], mm3
        movq      [eax+40], mm5
        pfadd     mm7, mm4
        pfadd     mm6, mm2
        movq      [eax+24], mm7
        movq      [eax+8], mm6

        movq      mm0, [eax+96]      
        movq      mm1, mm0           
        movq      mm2, [eax+64]      
        pfadd     mm0, mm2           
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm3, mm1           
        punpckhdq mm3, mm2           
        punpckldq mm2, mm1           
        pfsub     mm3, mm2           
        punpckldq mm1, mm3
        punpckhdq mm3, mm1
        movq      mm1, [eax]         
        movq      mm2, mm1
        movq      mm4, [eax+32]      
        movq      mm5, mm4
        pfsub     mm1, mm0
        pfsub     mm4, mm3
        movq      [eax+64], mm1
        movq      [eax+96], mm4
        pfadd     mm2, mm0
        pfadd     mm5, mm3
        movq      [eax], mm2
        movq      [eax+32], mm5
        movq      mm0, [eax+80]
        punpckldq mm1, mm0
        punpckhdq mm0, mm1
        movq      mm1, mm0
        punpckldq mm2, mm1
        punpckhdq mm1, mm2
        pxor      mm1, Invert3DNow_H
        pfadd     mm0, mm1
        movq      mm1, [eax+112]
        movq      mm4, mm1
        punpckldq mm2, mm4
        punpckhdq mm4, mm2
        pxor      mm4, Invert3DNow_H
        pfadd     mm1, mm4
        pfmul     mm0, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm2, mm1       
        punpckldq mm1, mm0       
        punpckhdq mm0, mm2       
        movq      mm2, mm1       
        pfadd     mm2, mm0       
        pfsub     mm1, mm0       
        punpckldq mm0, mm1
        punpckhdq mm1, mm0
        movq      mm0, [eax+48]
        movq      mm4, mm0
        pfsub     mm4, mm1     
        pfadd     mm0, mm1     
        movq      mm5, [eax+16]
        movq      mm6, mm5
        pfsub     mm5, mm2     
        pfadd     mm6, mm2     
        movq      [eax+16], mm6
        movq      [eax+48], mm0
        movq      [eax+80], mm5
        movq      [eax+112], mm4
        movq      mm0, [eax+72]      
        movq      mm1, mm0
        movq      mm2, [eax+104]     
        movq      mm3, mm2
        pfmul     mm0, d16re3DNow        
        pfmul     mm1, d16im3DNow
        punpckldq mm4, mm1
        punpckhdq mm1, mm4
        pfmul     mm2, d16re3DNow        
        pfmul     mm3, d16im3DNow
        punpckldq mm4, mm3
        punpckhdq mm3, mm4
        movq      mm4, mm0           
        pfadd     mm0, mm1           
        pfsub     mm4, mm1           
        movq      mm5, mm2           
        pfsub     mm2, mm3           
        pfadd     mm5, mm3           
        punpckldq mm2, mm0           
        punpckhdq mm5, mm4           
        punpckldq mm0, mm5
        punpckhdq mm5, mm0
        movq      mm0, mm5           
        movq      mm4, mm0
        punpckldq mm4, mm2 
        punpckhdq mm0, mm2 
        pfsub     mm4, mm0
        movq      mm0, mm5
        punpckldq mm0, mm2 
        punpckhdq mm5, mm2 
        pfadd     mm5, mm0
        punpckldq mm1, mm5
        punpckhdq mm5, mm1
        movq      mm1, [eax+40]
        movq      mm2, [eax+8]
        movq      mm3, mm1
        movq      mm0, mm2
        pfsub     mm1, mm4        
        pfsub     mm2, mm5        
        pfadd     mm3, mm4        
        pfadd     mm0, mm5        
        movq      [eax+8], mm0
        movq      [eax+40], mm3
        movq      [eax+72], mm2
        movq      [eax+104], mm1
        movq      mm0, [eax+88]
        movq      mm1, mm0
        movq      mm2, [eax+120]
        movq      mm3, mm2
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16im3DNow
        punpckldq mm4, mm1
        punpckhdq mm1, mm4
        pfmul     mm2, d16re3DNow  
        pfmul     mm3, d16im3DNow
        punpckldq mm4, mm3
        punpckhdq mm3, mm4
        movq      mm4, mm0     
        pfadd     mm4, mm1     
        pfsub     mm1, mm0     
        movq      mm5, mm2     
        pfadd     mm5, mm3     
        pfsub     mm3, mm2     
        punpckhdq mm3, mm4     
        punpckldq mm1, mm5     
        movq      mm4, mm3
        punpckldq mm2, mm4
        punpckhdq mm4, mm2
        pxor      mm4, Invert3DNow_H
        pfadd     mm3, mm4
        movq      mm4, mm1
        punpckldq mm2, mm4
        punpckhdq mm4, mm2
        pxor      mm4, Invert3DNow_H
        pfadd     mm1, mm4
        movq      mm2, mm1
        punpckldq mm2, mm3     
        punpckhdq mm3, mm1     
        movq      mm0, [eax+56]
        movq      mm1, mm0
        pfsub     mm1, mm2  
        pfadd     mm0, mm2  
        movq      mm4, [eax+24]
        movq      mm5, mm4
        pfsub     mm5, mm3  
        pfadd     mm4, mm3  
        movq      [eax+24], mm4
        movq      [eax+56], mm0
        movq      [eax+88], mm5
        movq      [eax+120], mm1
end;

procedure upass_3DNow(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       edi, DWORD PTR [ecx+ecx]
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, eax
        lea       esi, DWORD PTR [ecx-1]
        mov       DWORD PTR [esp+8], edi
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebx, DWORD PTR [edi+ecx]
        lea       ebp, DWORD PTR [eax+ecx]

        movq      mm0, [eax]
        movq      mm1, [ebp]
        movq      mm2, [edi]
        movq      mm3, [ebx]
        movq      mm4, mm2
        pfadd     mm4, mm3  
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        movq      mm7, mm2  
        punpckldq mm7, mm3  
        punpckhdq mm3, mm2  
        pfsub     mm3, mm7  
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        movq      mm5, mm0
        movq      mm6, mm1
        pfsub     mm0, mm4  
        pfsub     mm1, mm3  
        pfadd     mm5, mm4  
        pfadd     mm6, mm3  
        movq      [eax], mm5
        movq      [ebp], mm6
        movq      [edi], mm0
        movq      [ebx], mm1

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [edi+8]         
        movq      mm3, [ebx+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        punpckldq mm7, mm5
        punpckhdq mm5, mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm2, [eax+8]         
        movq      mm3, [ebp+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+8], mm4
        movq      [ebp+8], mm5
        movq      [edi+8], mm2
        movq      [ebx+8], mm3

        mov       DWORD PTR [esp+4], ecx
        jmp       @@B42
@@B43:
        mov       eax, edi
        mov       ebx, DWORD PTR [esp]
        mov       ecx, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebx
        lea       ebx, DWORD PTR [ebx+ecx]
        lea       ebp, DWORD PTR [edi+ecx]
@@B42:
        mov       ecx, DWORD PTR [esp+8]
        lea       edi, DWORD PTR [ecx+16]
        mov       DWORD PTR [esp], edi
        lea       edi, DWORD PTR [eax+16]

        movq      mm0, [edx+16]
        movq      mm1, [edx+24]
        movq      mm2, [ecx+16]         
        movq      mm3, [ebx+16]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        punpckldq mm7, mm5
        punpckhdq mm5, mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm2, [eax+16]         
        movq      mm3, [ebp+16]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+16], mm4
        movq      [ebp+16], mm5
        movq      [ecx+16], mm2
        movq      [ebx+16], mm3

        add       edx, 32
        dec       esi

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [ecx+24]         
        movq      mm3, [ebx+24]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        punpckldq mm7, mm5
        punpckhdq mm5, mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm2, [eax+24]         
        movq      mm3, [ebp+24]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+24], mm4
        movq      [ebp+24], mm5
        movq      [ecx+24], mm2
        movq      [ebx+24], mm3

        jne       @@B43
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi

end;

procedure upassbig_3DNow(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 24
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        add       ebp, ebp
        sub       ebp, ecx
        add       ebp, ebp
        lea       ebx, DWORD PTR [ecx+ecx]

        movq      mm0, [eax]
        movq      mm1, [eax+ebx*8]
        movq      mm2, [eax+esi*8]
        movq      mm3, [eax+ebp*8]
        movq      mm4, mm2
        pfadd     mm4, mm3  
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        movq      mm7, mm2  
        punpckldq mm7, mm3  
        punpckhdq mm3, mm2  
        pfsub     mm3, mm7  
        punpckldq mm7, mm3
        punpckhdq mm3, mm7
        movq      mm5, mm0
        movq      mm6, mm1
        pfsub     mm0, mm4  
        pfsub     mm1, mm3  
        pfadd     mm5, mm4  
        pfadd     mm6, mm3  
        movq      [eax], mm5
        movq      [eax+ebx*8], mm6
        movq      [eax+esi*8], mm0
        movq      [eax+ebp*8], mm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+4], eax
        mov       DWORD PTR [esp+8], edx
        mov       DWORD PTR [esp], ecx
        mov       ecx, eax
@@B12:
        lea       eax, DWORD PTR [ecx+8]

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [ecx+esi*8+8]         
        movq      mm3, [ecx+ebp*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        punpckldq mm7, mm5
        punpckhdq mm5, mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm2, [ecx+8]         
        movq      mm3, [ecx+ebx*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [ecx+8], mm4
        movq      [ecx+ebx*8+8], mm5
        movq      [ecx+esi*8+8], mm2
        movq      [ecx+ebp*8+8], mm3

        add       edx, 16
        mov       ecx, eax
        dec       edi
        jne       @@B12
@@B13:
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [eax+8]

        movq      mm0, [eax+esi*8+8]          
        movq      mm1, [eax+ebp*8+8]          
        movq      mm2, mm0           
        movq      mm3, mm1           
        punpckldq mm4, mm3
        punpckhdq mm3, mm4            
        punpckldq mm4, mm2
        punpckhdq mm2, mm4            
        pxor      mm2, Invert3DNow_H  
        pfsub     mm0, mm2
        pxor      mm3, Invert3DNow_H  
        pfadd     mm1, mm3
        pfmul     mm0, SqrtHalf3DNow  
        pfmul     mm1, SqrtHalf3DNow  
        movq      mm2, mm0
        pfadd     mm2, mm1            
        punpckldq mm4, mm0
        punpckhdq mm0, mm4            
        movq      mm4, mm0            
        punpckldq mm4, mm1            
        punpckhdq mm1, mm0            
        pfsub     mm4, mm1            
        movq      mm1, [eax+ebx*8+8]
        movq      mm3, [eax+8]
        movq      mm5, mm3
        movq      mm6, mm1
        pfsub     mm1, mm4         
        pfsub     mm3, mm2         
        pfadd     mm6, mm4         
        pfadd     mm5, mm2         
        movq      [eax+ebp*8+8], mm1
        movq      [eax+esi*8+8], mm3
        movq      [eax+ebx*8+8], mm6
        movq      [eax+8], mm5

        add       edx, -16
        mov       DWORD PTR [esp+12], ecx
        mov       DWORD PTR [esp+16], edi
        mov       DWORD PTR [esp+20], edx
@@B14:
        lea       eax, DWORD PTR [edi+8]

        movq      mm0, [edx+8]
        movq      mm1, [edx]
        movq      mm2, [edi+esi*8+8]         
        movq      mm3, [edi+ebp*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        punpckldq mm7, mm5
        punpckhdq mm5, mm7
        punpckldq mm7, mm2
        punpckhdq mm2, mm7
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        punpckldq mm3, mm1
        punpckhdq mm1, mm3
        movq      mm2, [edi+8]         
        movq      mm3, [edi+ebx*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [edi+8], mm4
        movq      [edi+ebx*8+8], mm5
        movq      [edi+esi*8+8], mm2
        movq      [edi+ebp*8+8], mm3

        add       edx, -16
        mov       edi, eax
        dec       ecx
        jne       @@B14
@@B15:
        add       esp, 24
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure c32_3DNow(a : PComplexArray);
begin
  cpass_3DNow(a,d323DNow,4);
  c8_3DNow(@a^[16]);
  c8_3DNow(@a^[24]);
  c16_3DNow(a);
end;

procedure c64_3DNow(a : PComplexArray);
begin
  cpass_3DNow(a,d643DNow,8);
  c16_3DNow(@a^[32]);
  c16_3DNow(@a^[48]);
  c32_3DNow(a);
end;

procedure c128_3DNow(a : PComplexArray);
begin
  cpass_3DNow(a,d1283DNow,16);
  c32_3DNow(@a^[64]);
  c32_3DNow(@a^[96]);
  c64_3DNow(a);
end;

procedure c256_3DNow(a : PComplexArray);
begin
  cpass_3DNow(a,d2563DNow,32);
  c64_3DNow(@a^[128]);
  c64_3DNow(@a^[192]);
  c128_3DNow(a);
end;

procedure c512_3DNow(a : PComplexArray);
begin
  cpass_3DNow(a,d5123DNow,64);
  c128_3DNow(@a^[384]);
  c128_3DNow(@a^[256]);
  c256_3DNow(a);
end;
procedure c1024_3DNow(a : PComplexArray);
begin
  cpassbig_3DNow(a,d10243DNow,128);
  c256_3DNow(@a^[768]);
  c256_3DNow(@a^[512]);
  c512_3DNow(a);
end;

procedure c2048_3DNow(a : PComplexArray);
begin
  cpassbig_3DNow(a,d20483DNow,256);
  c512_3DNow(@a^[1536]);
  c512_3DNow(@a^[1024]);
  c1024_3DNow(a);
end;

procedure c4096_3DNow(a : PComplexArray);
begin
  cpassbig_3DNow(a,d40963DNow,512);
  c1024_3DNow(@a^[3072]);
  c1024_3DNow(@a^[2048]);
  c2048_3DNow(a);
end;

procedure c8192_3DNow(a : PComplexArray);
begin
  cpassbig_3DNow(a,d81923DNow,1024);
  c2048_3DNow(@a^[6144]);
  c2048_3DNow(@a^[4096]);
  c4096_3DNow(a);
end;

procedure u32_3DNow(a : PComplexArray);
begin
  u16_3DNow(a);
  u8_3DNow(@a^[16]);
  u8_3DNow(@a^[24]);
  upass_3DNow(a,d323DNow,4);
end;

procedure u64_3DNow(a : PComplexArray);
begin
  u32_3DNow(a);
  u16_3DNow(@a^[32]);
  u16_3DNow(@a^[48]);
  upass_3DNow(a,d643DNow,8);
end;

procedure u128_3DNow(a : PComplexArray);
begin
  u64_3DNow(a);
  u32_3DNow(@a^[64]);
  u32_3DNow(@a^[96]);
  upass_3DNow(a,d1283DNow,16);
end;

procedure u256_3DNow(a : PComplexArray);
begin
  u128_3DNow(a);
  u64_3DNow(@a^[128]);
  u64_3DNow(@a^[192]);
  upass_3DNow(a,d2563DNow,32);
end;

procedure u512_3DNow(a : PComplexArray);
begin
  u256_3DNow(a);
  u128_3DNow(@a^[256]);
  u128_3DNow(@a^[384]);
  upass_3DNow(a,d5123DNow,64);
end;

procedure u1024_3DNow(a : PComplexArray);
begin
  u512_3DNow(a);
  u256_3DNow(@a^[512]);
  u256_3DNow(@a^[768]);
  upassbig_3DNow(a,d10243DNow,128);
end;

procedure u2048_3DNow(a : PComplexArray);
begin
  u1024_3DNow(a);
  u512_3DNow(@a^[1024]);
  u512_3DNow(@a^[1536]);
  upassbig_3DNow(a,d20483DNow,256);
end;

procedure u4096_3DNow(a : PComplexArray);
begin
  u2048_3DNow(a);
  u1024_3DNow(@a^[2048]);
  u1024_3DNow(@a^[3072]);
  upassbig_3DNow(a,d40963DNow,512);
end;

procedure u8192_3DNow(a : PComplexArray);
begin
  u4096_3DNow(a);
  u2048_3DNow(@a^[4096]);
  u2048_3DNow(@a^[6144]);
  upassbig_3DNow(a,d81923DNow,1024);
end;

procedure fft2__3DNow(a : PComplexArray);
begin
  asm femms end;
  c2_3DNow(a);
  asm femms end;
end;

procedure ifft2__3DNow(a : PComplexArray);
begin
  asm femms end;
  c2_3DNow(a);
  asm femms end;
end;

procedure fft4__3DNow(a : PComplexArray);
begin
  asm femms end;
  c4_3DNow(a);
  asm femms end;
end;

procedure ifft4__3DNow(a : PComplexArray);
begin
  asm femms end;
  u4_3DNow(a);
  asm femms end;
end;

procedure fft8__3DNow(a : PComplexArray);
begin
  asm femms end;
  c8_3DNow(a);
  asm femms end;
end;

procedure ifft8__3DNow(a : PComplexArray);
begin
  asm femms end;
  u8_3DNow(a);
  asm femms end;
end;

procedure fft16__3DNow(a : PComplexArray);
begin
  asm femms end;
  c16_3DNow(a);
  asm femms end;
end;

procedure ifft16__3DNow(a : PComplexArray);
begin
  asm femms end;
  u16_3DNow(a);
  asm femms end;
end;

procedure fft32__3DNow(a : PComplexArray);
begin
  asm femms end;
  c32_3DNow(a);
  asm femms end;
end;

procedure ifft32__3DNow(a : PComplexArray);
begin
  asm femms end;
  u32_3DNow(a);
  asm femms end;
end;

procedure fft64__3DNow(a : PComplexArray);
begin
  asm femms end;
  c64_3DNow(a);
  asm femms end;
end;

procedure ifft64__3DNow(a : PComplexArray);
begin
  asm femms end;
  u64_3DNow(a);
  asm femms end;
end;

procedure fft128__3DNow(a : PComplexArray);
begin
  asm femms end;
  c128_3DNow(a);
  asm femms end;
end;

procedure ifft128__3DNow(a : PComplexArray);
begin
  asm femms end;
  u128_3DNow(a);
  asm femms end;
end;

procedure fft256__3DNow(a : PComplexArray);
begin
  asm femms end;
  c256_3DNow(a);
  asm femms end;
end;

procedure ifft256__3DNow(a : PComplexArray);
begin
  asm femms end;
  u256_3DNow(a);
  asm femms end;
end;

procedure fft512__3DNow(a : PComplexArray);
begin
  asm femms end;
  c512_3DNow(a);
  asm femms end;
end;

procedure ifft512__3DNow(a : PComplexArray);
begin
  asm femms end;
  u512_3DNow(a);
  asm femms end;
end;

procedure fft1024__3DNow(a : PComplexArray);
begin
  asm femms end;
  c1024_3DNow(a);
  asm femms end;
end;

procedure ifft1024__3DNow(a : PComplexArray);
begin
  asm femms end;
  u1024_3DNow(a);
  asm femms end;
end;

procedure fft2048__3DNow(a : PComplexArray);
begin
  asm femms end;
  c2048_3DNow(a);
  asm femms end;
end;

procedure ifft2048__3DNow(a : PComplexArray);
begin
  asm femms end;
  u2048_3DNow(a);
  asm femms end;
end;

procedure fft4096__3DNow(a : PComplexArray);
begin
  asm femms end;
  c4096_3DNow(a);
  asm femms end;
end;

procedure ifft4096__3DNow(a : PComplexArray);
begin
  asm femms end;
  u4096_3DNow(a);
  asm femms end;
end;

procedure fft8192__3DNow(a : PComplexArray);
begin
  asm femms end;
  c8192_3DNow(a);
  asm femms end;
end;

procedure ifft8192__3DNow(a : PComplexArray);
begin
  asm femms end;
  u8192_3DNow(a);
  asm femms end;
end;

procedure c4_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax]       
        movq      mm1, mm0         
        movq      mm2, [eax+8]     
        movq      mm3, mm2         
        movq      mm4, [eax+16]    
        movq      mm5, [eax+24]    
        pfadd     mm0, mm4         
        pfadd     mm2, mm5         
        pfsub     mm1, mm4         
        pfsub     mm3, mm5         
        movq      mm4, mm0
        pfadd     mm0, mm2
        pfsub     mm4, mm2
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      mm0, mm1
        pswapd    mm3, mm3
        movq      mm4, mm3
        pxor      mm3, Invert3DNow_H
        pfadd     mm0, mm3       
        movq      [eax+16], mm0
        pxor      mm4, Invert3DNow_H
        pfsub     mm1, mm4       
        movq      [eax+24], mm1
end;

procedure c8_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax]     
        movq      mm4, [eax+32]  
        movq      mm6, mm0       
        pfsub     mm0, mm4       
        pfadd     mm6, mm4       
        movq      mm2, [eax+16]  
        movq      mm5, [eax+48]  
        movq      mm7, mm2       
        pfsub     mm2, mm5       
        pfadd     mm7, mm5       
        movq      [eax+16], mm7
        pswapd    mm2, mm2       
        movq      mm7, mm0       
        pfsub     mm7, mm2       
        pfadd     mm0, mm2       
        movq      mm1, [eax+8]   
        movq      mm5, [eax+40]  
        movq      mm4, mm1       
        pfsub     mm1, mm5       
        pfadd     mm4, mm5       
        movq      [eax+8], mm4
        movq      mm3, [eax+24]  
        movq      mm2, [eax+56]  
        movq      mm4, mm3       
        pfsub     mm3, mm2       
        pfadd     mm4, mm2       
        movq      [eax+24], mm4
        pswapd    mm3, mm3          
        movq      mm2, mm1       
        pfsub     mm2, mm3       
        pfadd     mm1, mm3       
        pswapd    mm2, mm2       
        movq      mm4, mm1       
        pfadd     mm4, mm2       
        pfsub     mm1, mm2       
        pfmul     mm4, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm3, mm1       
        punpckhdq mm3, mm4       
        pswapd    mm7, mm7       
        movq      mm2, mm7       
        punpckhdq mm2, mm0       
        movq      mm5, mm2       
        pfsub     mm5, mm3       
        pfadd     mm2, mm3       
        pswapd    mm2, mm2
        movq      mm3, mm5       
        punpckldq mm3, mm2
        punpckhdq mm2, mm5
        movq      [eax+32], mm3
        movq      [eax+40], mm2
        punpckldq mm0, mm7       
        punpckldq mm4, mm1       
        movq      mm5, mm0       
        pfsub     mm5, mm4       
        pfadd     mm0, mm4       
        pswapd    mm0, mm0
        movq      mm3, mm5       
        punpckldq mm3, mm0
        punpckhdq mm0, mm5
        movq      [eax+48], mm0
        movq      [eax+56], mm3
        movq      mm2, [eax+16]  
        movq      mm5, mm6       
        pfadd     mm6, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+8]   
        movq      mm4, mm1       
        movq      mm3, [eax+24]  
        pfadd     mm1, mm3       
        pfsub     mm4, mm3       
        movq      mm2, mm6       
        pfadd     mm6, mm1       
        pfsub     mm2, mm1       
        movq      [eax], mm6
        movq      [eax+8], mm2
        movq      mm7, mm5       
        pswapd    mm4, mm4       
        pfsub     mm7, mm4       
        pfadd     mm5, mm4       
        pswapd    mm5, mm5       
        movq      mm4, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm4      
        movq      [eax+16], mm7
        movq      [eax+24], mm5
end;

procedure c16_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm1, mm0
        movq      mm2, [eax+64]
        pfsub     mm0, mm2         
        pfadd     mm1, mm2
        movq      [eax], mm1
        movq      mm1, [eax+32]
        movq      mm2, mm1
        movq      mm3, [eax+96]
        pfsub     mm1, mm3        
        pfadd     mm2, mm3
        movq      [eax+32], mm2
        pswapd    mm1, mm1
        movq      mm2, mm0
        pfsub     mm0, mm1       
        pfadd     mm2, mm1       
        pswapd    mm0, mm0       
        movq      mm1, mm2       
        punpckldq mm1, mm0       
        punpckhdq mm0, mm2       
        movq      [eax+64], mm0
        movq      [eax+96], mm1
        movq      mm2, [eax+16]  
        movq      mm3, mm2       
        movq      mm4, [eax+48]  
        movq      mm5, mm4       
        movq      mm6, [eax+80]  
        movq      mm7, [eax+112] 
        pfsub     mm2, mm6         
        pfsub     mm4, mm7         
        pfadd     mm3, mm6
        pfadd     mm5, mm7
        movq      [eax+16], mm3
        movq      [eax+48], mm5
        pswapd    mm4, mm4
        movq      mm3, mm2
        pfadd     mm3, mm4        
        pfsub     mm2, mm4        
        pswapd    mm3, mm3        
        movq      mm1, mm2
        pfsub     mm1, mm3        
        pfadd     mm2, mm3        
        pfmul     mm1, SqrtHalf3DNow
        pfmul     mm2, SqrtHalf3DNow
        movq      mm3, mm1      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm3      
        movq      [eax+80], mm1
        movq      [eax+112], mm2
        movq      mm1, [eax+8]
        movq      mm2, mm1
        movq      mm3, [eax+40]
        movq      mm4, mm3
        movq      mm6, [eax+72]
        movq      mm7, [eax+104]
        pfsub     mm1, mm6        
        pfadd     mm2, mm6
        pfsub     mm3, mm7        
        pfadd     mm4, mm7
        movq      [eax+8], mm2
        movq      [eax+40], mm4
        pswapd    mm3, mm3
        movq      mm0, mm1
        pfsub     mm1, mm3  
        pfadd     mm0, mm3  
        movq      mm2, mm0     
        movq      mm3, mm1     
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16re3DNow  
        pfmul     mm2, d16im3DNow  
        pfmul     mm3, d16im3DNow  
        pswapd    mm2, mm2     
        pswapd    mm3, mm3     
        pfsub     mm1, mm2     
        pfadd     mm0, mm3     
        pswapd    mm1, mm1     
        movq      mm2, mm0     
        punpckldq mm0, mm1     
        punpckhdq mm1, mm2     
        movq      [eax+72], mm1
        movq      [eax+104], mm0
        movq      mm1, [eax+24]  
        movq      mm2, mm1       
        movq      mm3, [eax+56]  
        movq      mm4, mm3       
        movq      mm6, [eax+88]  
        movq      mm7, [eax+120] 
        pfsub     mm1, mm6        
        pfadd     mm2, mm6
        pfsub     mm3, mm7        
        pfadd     mm4, mm7
        movq      [eax+24], mm2
        movq      [eax+56], mm4
        pswapd    mm3, mm3
        movq      mm2, mm1   
        pfsub     mm2, mm3   
        pfadd     mm1, mm3   
        movq      mm3, mm1     
        movq      mm0, mm2     
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16re3DNow  
        pfmul     mm2, d16im3DNow  
        pfmul     mm3, d16im3DNow  
        pswapd    mm1, mm1     
        pswapd    mm3, mm3     
        pfsub     mm2, mm1     
        pfadd     mm3, mm0     
        movq      mm0, mm3     
        punpckhdq mm0, mm2     
        punpckldq mm2, mm3     
        movq      [eax+88], mm2
        movq      [eax+120], mm0

        movq      mm0, [eax]     
        movq      mm4, [eax+32]  
        movq      mm6, mm0       
        pfsub     mm0, mm4       
        pfadd     mm6, mm4       
        movq      mm2, [eax+16]  
        movq      mm5, [eax+48]  
        movq      mm7, mm2       
        pfsub     mm2, mm5       
        pfadd     mm7, mm5       
        movq      [eax+16], mm7
        pswapd    mm2, mm2       
        movq      mm7, mm0       
        pfsub     mm7, mm2       
        pfadd     mm0, mm2       
        movq      mm1, [eax+8]   
        movq      mm5, [eax+40]  
        movq      mm4, mm1       
        pfsub     mm1, mm5       
        pfadd     mm4, mm5       
        movq      [eax+8], mm4
        movq      mm3, [eax+24]  
        movq      mm2, [eax+56]  
        movq      mm4, mm3       
        pfsub     mm3, mm2       
        pfadd     mm4, mm2       
        movq      [eax+24], mm4
        pswapd    mm3, mm3          
        movq      mm2, mm1       
        pfsub     mm2, mm3       
        pfadd     mm1, mm3       
        pswapd    mm2, mm2       
        movq      mm4, mm1       
        pfadd     mm4, mm2       
        pfsub     mm1, mm2       
        pfmul     mm4, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm3, mm1       
        punpckhdq mm3, mm4       
        pswapd    mm7, mm7       
        movq      mm2, mm7       
        punpckhdq mm2, mm0       
        movq      mm5, mm2       
        pfsub     mm5, mm3       
        pfadd     mm2, mm3       
        pswapd    mm2, mm2
        movq      mm3, mm5       
        punpckldq mm3, mm2
        punpckhdq mm2, mm5
        movq      [eax+32], mm3
        movq      [eax+40], mm2
        punpckldq mm0, mm7       
        punpckldq mm4, mm1       
        movq      mm5, mm0       
        pfsub     mm5, mm4       
        pfadd     mm0, mm4       
        pswapd    mm0, mm0
        movq      mm3, mm5       
        punpckldq mm3, mm0
        punpckhdq mm0, mm5
        movq      [eax+48], mm0
        movq      [eax+56], mm3
        movq      mm2, [eax+16]  
        movq      mm5, mm6       
        pfadd     mm6, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+8]   
        movq      mm4, mm1       
        movq      mm3, [eax+24]  
        pfadd     mm1, mm3       
        pfsub     mm4, mm3       
        movq      mm2, mm6       
        pfadd     mm6, mm1       
        pfsub     mm2, mm1       
        movq      [eax], mm6
        movq      [eax+8], mm2
        movq      mm7, mm5       
        pswapd    mm4, mm4       
        pfsub     mm7, mm4       
        pfadd     mm5, mm4       
        pswapd    mm5, mm5       
        movq      mm4, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm4      
        movq      [eax+16], mm7
        movq      [eax+24], mm5

        movq      mm0, [eax+64]     
        movq      mm2, [eax+80]  
        movq      mm5, mm0       
        pfadd     mm0, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+72]   
        movq      mm6, mm1       
        movq      mm3, [eax+88]  
        pfadd     mm1, mm3       
        pfsub     mm6, mm3       
        movq      mm2, mm0       
        pfadd     mm0, mm1       
        pfsub     mm2, mm1       
        movq      [eax+64], mm0
        movq      [eax+72], mm2
        movq      mm7, mm5       
        pswapd    mm6, mm6       
        pfsub     mm7, mm6       
        pfadd     mm5, mm6       
        pswapd    mm5, mm5       
        movq      mm6, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm6      
        movq      [eax+80], mm7
        movq      [eax+88], mm5

        movq      mm0, [eax+96]     
        movq      mm2, [eax+112]  
        movq      mm5, mm0       
        pfadd     mm0, mm2       
        pfsub     mm5, mm2       
        movq      mm1, [eax+104]   
        movq      mm6, mm1       
        movq      mm3, [eax+120]  
        pfadd     mm1, mm3       
        pfsub     mm6, mm3       
        movq      mm2, mm0       
        pfadd     mm0, mm1       
        pfsub     mm2, mm1       
        movq      [eax+96], mm0
        movq      [eax+104], mm2
        movq      mm7, mm5       
        pswapd    mm6, mm6       
        pfsub     mm7, mm6       
        pfadd     mm5, mm6       
        pswapd    mm5, mm5       
        movq      mm6, mm7      
        punpckldq mm7, mm5      
        punpckhdq mm5, mm6      
        movq      [eax+112], mm7
        movq      [eax+120], mm5
end;

procedure cpass_3DNowExt(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       ebx, DWORD PTR [ecx+ecx]
        mov       DWORD PTR [esp+8], eax
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, eax
        lea       edi, DWORD PTR [ecx-1]
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebp, DWORD PTR [eax+ecx]
        lea       esi, DWORD PTR [ebx+ecx]

        movq      mm0, [eax]     
        movq      mm1, [ebp]     
        movq      mm2, [ebx]     
        movq      mm3, [esi]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2      
        pfadd     mm5, mm3      
        movq      [eax], mm4
        movq      [ebp], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1      
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm0, mm0      
        movq      mm1, mm2      
        punpckldq mm1, mm0      
        punpckhdq mm0, mm2      
        movq      [ebx], mm0
        movq      [esi], mm1

        movq      mm0, [eax+8]     
        movq      mm1, [ebp+8]     
        movq      mm2, [ebx+8]     
        movq      mm3, [esi+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+8], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1      
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm2, mm2      
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+8], mm1
        movq      [esi+8], mm2

        mov       DWORD PTR [esp+4], ecx
        jmp       @@B92
@@B93:
        mov       ebp, DWORD PTR [esp]
        mov       ebx, ecx
        mov       eax, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebp
        lea       ebp, DWORD PTR [ebp+eax]
        lea       esi, DWORD PTR [ecx+eax]
@@B92:
        mov       eax, DWORD PTR [esp+8]
        lea       ecx, DWORD PTR [eax+16]
        mov       DWORD PTR [esp], ecx
        lea       ecx, DWORD PTR [ebx+16]

        movq      mm0, [eax+16]     
        movq      mm1, [ebp+16]     
        movq      mm2, [ebx+16]     
        movq      mm3, [esi+16]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+16], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+16], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1      
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm2, mm2      
        movq      mm6, [edx+16]
        movq      mm7, [edx+24]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+16], mm1
        movq      [esi+16], mm2

        add       edx, 32
        dec       edi

        movq      mm0, [eax+24]     
        movq      mm1, [ebp+24]     
        movq      mm2, [ebx+24]     
        movq      mm3, [esi+24]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+24], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [ebp+24], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1      
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm2, mm2      
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [ebx+24], mm1
        movq      [esi+24], mm2

        jne       @@B93
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure cpassbig_3DNowExt(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 32
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        lea       ebx, DWORD PTR [ecx+ecx]
        add       ebx, ebx
        sub       ebx, ecx
        add       ebx, ebx

        movq      mm0, [eax]     
        movq      mm1, [eax+ebp*8]     
        movq      mm2, [eax+esi*8]     
        movq      mm3, [eax+ebx*8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2      
        pfadd     mm5, mm3      
        movq      [eax], mm4
        movq      [eax+ebp*8], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm0, mm0
        movq      mm1, mm2      
        punpckldq mm1, mm0      
        punpckhdq mm0, mm2      
        movq      [eax+esi*8], mm0
        movq      [eax+ebx*8], mm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+12], edx
        mov       DWORD PTR [esp+16], eax
        mov       DWORD PTR [esp+8], ecx
@@B122:
        lea       ecx, DWORD PTR [eax+8]

        movq      mm0, [eax+8]     
        movq      mm1, [eax+ebp*8+8]     
        movq      mm2, [eax+esi*8+8]     
        movq      mm3, [eax+ebx*8+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [eax+ebp*8+8], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm2, mm2
        movq      mm6, [edx]
        movq      mm7, [edx+8]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [eax+esi*8+8], mm1
        movq      [eax+ebx*8+8], mm2

        mov       eax, ecx
        add       edx, 16
        dec       edi
        jne       @@B122
@@B123:
        mov       DWORD PTR [esp], ecx
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+8], ecx
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [ecx+8]
        mov       DWORD PTR [esp+4], edi

        movq      mm0, [ecx+8]
        movq      mm1, [eax+ebp*8+8]
        movq      mm2, [eax+esi*8+8]
        movq      mm3, [eax+ebx*8+8]
        movq      mm4, mm0
        movq      mm5, mm1
        pfsub     mm0, mm2      
        pfsub     mm1, mm3      
        pfadd     mm4, mm2
        pfadd     mm5, mm3
        movq      [ecx+8], mm4
        movq      [eax+ebp*8+8], mm5
        pswapd    mm1, mm1
        movq      mm2, mm0
        pfsub     mm2, mm1      
        pfadd     mm0, mm1      
        pswapd    mm0, mm0
        movq      mm3, mm2      
        pfsub     mm3, mm0      
        pfadd     mm0, mm2      
        pfmul     mm0, SqrtHalf3DNow
        pfmul     mm3, SqrtHalf3DNow
        movq      mm1, mm0
        punpckhdq mm0, mm3      
        punpckldq mm3, mm1      
        movq      [eax+esi*8+8], mm3
        movq      [eax+ebx*8+8], mm0

        add       edx, -16
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+20], ecx
        mov       DWORD PTR [esp+24], edx
        mov       DWORD PTR [esp+28], edi
        mov       eax, edi
@@B124:
        lea       edi, DWORD PTR [eax+8]

        movq      mm0, [eax+8]     
        movq      mm1, [eax+ebp*8+8]     
        movq      mm2, [eax+esi*8+8]     
        movq      mm3, [eax+ebx*8+8]     
        movq      mm4, mm0      
        movq      mm5, mm1      
        pfsub     mm0, mm2      
        pfadd     mm4, mm2      
        movq      [eax+8], mm4
        pfsub     mm1, mm3      
        pfadd     mm5, mm3      
        movq      [eax+ebp*8+8], mm5
        movq      mm2, mm0      
        pswapd    mm1, mm1
        pfsub     mm0, mm1      
        pfadd     mm2, mm1      
        pswapd    mm2, mm2
        movq      mm6, [edx+8]
        movq      mm7, [edx]
        movq      mm1, mm0      
        movq      mm3, mm2      
        pfmul     mm0, mm6      
        pfmul     mm2, mm6      
        pfmul     mm1, mm7      
        pfmul     mm3, mm7      
        pfsub     mm0, mm3      
        pfadd     mm2, mm1      
        movq      mm1, mm0      
        punpckldq mm1, mm2      
        punpckhdq mm2, mm0      
        movq      [eax+esi*8+8], mm1
        movq      [eax+ebx*8+8], mm2

        mov       eax, edi
        add       edx, -16
        dec       ecx
        jne       @@B124
@@B125:
        add       esp, 32
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure u4_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        pswapd    mm1, mm1
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        pswapd    mm6, mm6
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+16], mm1
        movq      [eax+24], mm5
end;
procedure u8_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        pswapd    mm1, mm1
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        pswapd    mm6, mm6
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+24], mm5

        movq      mm0, [eax+32]  
        movq      mm7, [eax+40]  
        movq      mm2, [eax+48]  
        movq      mm3, [eax+56]  
        movq      mm4, mm0       
        movq      mm5, mm2       
        pfadd     mm4, mm7       
        pfsub     mm0, mm7
        pfadd     mm5, mm3       
        pfsub     mm2, mm3
        movq      mm3, mm4       
        pfadd     mm3, mm5                    
        pswapd    mm4, mm4       
        movq      mm6, mm4       
        punpckldq mm6, mm5       
        punpckhdq mm5, mm4       
        pfsub     mm6, mm5       
        movq      mm7, mm1
        pfsub     mm1, mm6
        movq      [eax+48], mm1
        movq      mm1, [eax]
        pfsub     mm1, mm3
        movq      [eax+32], mm1
        pfadd     mm7, mm6
        movq      [eax+16], mm7
        movq      mm1, [eax]
        pfadd     mm1, mm3
        movq      [eax], mm1
        movq      mm1, mm0
        pfacc     mm1, mm2
        pfmul     mm1, SqrtHalf3DNow  
        pswapd    mm0, mm0
        pfnacc    mm0, mm2
        pfmul     mm0, SqrtHalf3DNow  
        pswapd    mm0, mm0    
        movq      mm2, mm0
        pfadd     mm2, mm1   
        pfsub     mm0, mm1   
        pswapd    mm0, mm0   
        movq      mm3, [eax+24]
        movq      mm4, mm3
        movq      mm5, [eax+8]
        movq      mm6, mm5
        pfsub     mm3, mm0
        pfsub     mm5, mm2
        movq      [eax+56], mm3
        movq      [eax+40], mm5
        pfadd     mm4, mm0
        pfadd     mm6, mm2
        movq      [eax+24], mm4
        movq      [eax+8], mm6
end;

procedure u16_3DNowExt(a : PComplexArray);
asm
        movq      mm0, [eax+64]
        movq      mm5, mm0
        movq      mm1, [eax+72]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+88] 
        movq      mm6, mm3      
        movq      mm1, [eax+80] 
        pfadd     mm3, mm1      
        pswapd    mm1, mm1
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        pswapd    mm6, mm6
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax+64], mm0
        movq      [eax+72], mm4
        movq      [eax+80], mm1
        movq      [eax+88], mm5

        movq      mm0, [eax+96]
        movq      mm5, mm0
        movq      mm1, [eax+104]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+120] 
        movq      mm6, mm3      
        movq      mm1, [eax+112] 
        pfadd     mm3, mm1      
        pswapd    mm1, mm1
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        pswapd    mm6, mm6
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax+96], mm0
        movq      [eax+104], mm4
        movq      [eax+112], mm1
        movq      [eax+120], mm5

        movq      mm0, [eax]
        movq      mm5, mm0
        movq      mm1, [eax+8]
        pfadd     mm0, mm1        
        pfsub     mm5, mm1        
        movq      mm3, [eax+24] 
        movq      mm6, mm3      
        movq      mm1, [eax+16] 
        pfadd     mm3, mm1      
        pswapd    mm1, mm1
        movq      mm2, mm1
        punpckhdq mm1, mm6       
        punpckldq mm6, mm2       
        pfsub     mm6, mm1       
        pswapd    mm6, mm6
        movq      mm1, mm0  
        movq      mm4, mm5  
        pfadd     mm0, mm3  
        pfsub     mm1, mm3  
        pfadd     mm4, mm6  
        pfsub     mm5, mm6  
        movq      [eax], mm0
        movq      [eax+8], mm4
        movq      [eax+24], mm5

        movq      mm0, [eax+32]  
        movq      mm7, [eax+40]  
        movq      mm2, [eax+48]  
        movq      mm3, [eax+56]  
        movq      mm4, mm0       
        movq      mm5, mm2       
        pfadd     mm4, mm7       
        pfsub     mm0, mm7
        pfadd     mm5, mm3       
        pfsub     mm2, mm3
        movq      mm3, mm4       
        pfadd     mm3, mm5                    
        pswapd    mm4, mm4       
        movq      mm6, mm4       
        punpckldq mm6, mm5       
        punpckhdq mm5, mm4       
        pfsub     mm6, mm5       
        movq      mm7, mm1
        pfsub     mm1, mm6
        movq      [eax+48], mm1
        movq      mm1, [eax]
        pfsub     mm1, mm3
        movq      [eax+32], mm1
        pfadd     mm7, mm6
        movq      [eax+16], mm7
        movq      mm1, [eax]
        pfadd     mm1, mm3
        movq      [eax], mm1
        movq      mm1, mm0
        pfacc     mm1, mm2
        pfmul     mm1, SqrtHalf3DNow  
        pswapd    mm0, mm0
        pfnacc    mm0, mm2
        pfmul     mm0, SqrtHalf3DNow  
        pswapd    mm0, mm0    
        movq      mm2, mm0
        pfadd     mm2, mm1   
        pfsub     mm0, mm1   
        pswapd    mm0, mm0   
        movq      mm3, [eax+24]
        movq      mm4, mm3
        movq      mm5, [eax+8]
        movq      mm6, mm5
        pfsub     mm3, mm0
        pfsub     mm5, mm2
        movq      [eax+56], mm3
        movq      [eax+40], mm5
        pfadd     mm4, mm0
        pfadd     mm6, mm2
        movq      [eax+24], mm4
        movq      [eax+8], mm6

        movq      mm0, [eax+96]      
        movq      mm1, mm0           
        movq      mm2, [eax+64]      
        pfadd     mm0, mm2           
        pswapd    mm1, mm1           
        movq      mm3, mm1           
        punpckhdq mm3, mm2           
        punpckldq mm2, mm1           
        pfsub     mm3, mm2           
        pswapd    mm3, mm3
        movq      mm1, [eax]         
        movq      mm2, mm1
        movq      mm4, [eax+32]      
        movq      mm5, mm4
        pfsub     mm1, mm0
        pfsub     mm4, mm3
        movq      [eax+64], mm1
        movq      [eax+96], mm4
        pfadd     mm2, mm0
        pfadd     mm5, mm3
        movq      [eax], mm2
        movq      [eax+32], mm5
        movq      mm0, [eax+80]
        pswapd    mm0, mm0
        pfpnacc   mm0, mm0           
        movq      mm1, [eax+112]
        pfpnacc   mm1, mm1           
        pfmul     mm0, SqrtHalf3DNow     
        pfmul     mm1, SqrtHalf3DNow     
        movq      mm2, mm1       
        punpckldq mm1, mm0       
        punpckhdq mm0, mm2       
        movq      mm2, mm1       
        pfadd     mm2, mm0       
        pfsub     mm1, mm0       
        pswapd    mm1, mm1       
        movq      mm0, [eax+48]
        movq      mm4, mm0
        pfsub     mm4, mm1     
        pfadd     mm0, mm1     
        movq      mm5, [eax+16]
        movq      mm6, mm5
        pfsub     mm5, mm2     
        pfadd     mm6, mm2     
        movq      [eax+16], mm6
        movq      [eax+48], mm0
        movq      [eax+80], mm5
        movq      [eax+112], mm4
        movq      mm0, [eax+72]      
        movq      mm1, mm0
        movq      mm2, [eax+104]     
        movq      mm3, mm2
        pfmul     mm0, d16re3DNow        
        pfmul     mm1, d16im3DNow
        pswapd    mm1, mm1           
        pfmul     mm2, d16re3DNow        
        pfmul     mm3, d16im3DNow
        pswapd    mm3, mm3           
        movq      mm4, mm0           
        pfadd     mm0, mm1           
        pfsub     mm4, mm1           
        movq      mm5, mm2           
        pfsub     mm2, mm3           
        pfadd     mm5, mm3           
        punpckldq mm2, mm0           
        punpckhdq mm5, mm4           
        pswapd    mm5, mm5           
        movq      mm0, mm5           
        pfnacc    mm0, mm2           
        pfacc     mm5, mm2           
        pswapd    mm5, mm5           
        movq      mm1, [eax+40]
        movq      mm2, [eax+8]
        movq      mm3, mm1
        movq      mm4, mm2
        pfsub     mm1, mm0        
        pfsub     mm2, mm5        
        pfadd     mm3, mm0        
        pfadd     mm4, mm5        
        movq      [eax+8], mm4
        movq      [eax+40], mm3
        movq      [eax+72], mm2
        movq      [eax+104], mm1
        movq      mm0, [eax+88]
        movq      mm1, mm0
        movq      mm2, [eax+120]
        movq      mm3, mm2
        pfmul     mm0, d16re3DNow  
        pfmul     mm1, d16im3DNow
        pswapd    mm1, mm1     
        pfmul     mm2, d16re3DNow  
        pfmul     mm3, d16im3DNow
        pswapd    mm3, mm3     
        movq      mm4, mm0     
        pfadd     mm4, mm1     
        pfsub     mm1, mm0     
        movq      mm5, mm2     
        pfadd     mm5, mm3     
        pfsub     mm3, mm2     
        punpckhdq mm3, mm4     
        punpckldq mm1, mm5     
        pfpnacc   mm3, mm3     
        pfpnacc   mm1, mm1     
        movq      mm2, mm1
        punpckldq mm2, mm3     
        punpckhdq mm3, mm1     
        movq      mm0, [eax+56]
        movq      mm1, mm0
        pfsub     mm1, mm2  
        pfadd     mm0, mm2  
        movq      mm4, [eax+24]
        movq      mm5, mm4
        pfsub     mm5, mm3  
        pfadd     mm4, mm3  
        movq      [eax+24], mm4
        movq      [eax+56], mm0
        movq      [eax+88], mm5
        movq      [eax+120], mm1
end;

procedure upass_3DNowExt(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       edi, DWORD PTR [ecx+ecx]
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, eax
        lea       esi, DWORD PTR [ecx-1]
        mov       DWORD PTR [esp+8], edi
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebx, DWORD PTR [edi+ecx]
        lea       ebp, DWORD PTR [eax+ecx]

        movq      mm0, [eax]
        movq      mm1, [ebp]
        movq      mm2, [edi]
        movq      mm3, [ebx]
        movq      mm4, mm2
        pfadd     mm4, mm3  
        pswapd    mm3, mm3
        movq      mm7, mm2  
        punpckldq mm7, mm3  
        punpckhdq mm3, mm2  
        pfsub     mm3, mm7  
        pswapd    mm3, mm3
        movq      mm5, mm0
        movq      mm6, mm1
        pfsub     mm0, mm4  
        pfsub     mm1, mm3  
        pfadd     mm5, mm4  
        pfadd     mm6, mm3  
        movq      [eax], mm5
        movq      [ebp], mm6
        movq      [edi], mm0
        movq      [ebx], mm1

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [edi+8]         
        movq      mm3, [ebx+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        pswapd    mm5, mm5          
        pswapd    mm2, mm2          
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        pswapd    mm1, mm1          
        movq      mm2, [eax+8]         
        movq      mm3, [ebp+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+8], mm4
        movq      [ebp+8], mm5
        movq      [edi+8], mm2
        movq      [ebx+8], mm3

        mov       DWORD PTR [esp+4], ecx
        jmp       @@B42
@@B43:
        mov       eax, edi
        mov       ebx, DWORD PTR [esp]
        mov       ecx, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebx
        lea       ebx, DWORD PTR [ebx+ecx]
        lea       ebp, DWORD PTR [edi+ecx]
@@B42:
        mov       ecx, DWORD PTR [esp+8]
        lea       edi, DWORD PTR [ecx+16]
        mov       DWORD PTR [esp], edi
        lea       edi, DWORD PTR [eax+16]

        movq      mm0, [edx+16]
        movq      mm1, [edx+24]
        movq      mm2, [ecx+16]         
        movq      mm3, [ebx+16]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        pswapd    mm5, mm5          
        pswapd    mm2, mm2          
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        pswapd    mm1, mm1          
        movq      mm2, [eax+16]         
        movq      mm3, [ebp+16]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+16], mm4
        movq      [ebp+16], mm5
        movq      [ecx+16], mm2
        movq      [ebx+16], mm3

        add       edx, 32
        dec       esi

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [ecx+24]         
        movq      mm3, [ebx+24]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        pswapd    mm5, mm5          
        pswapd    mm2, mm2          
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        pswapd    mm1, mm1          
        movq      mm2, [eax+24]         
        movq      mm3, [ebp+24]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [eax+24], mm4
        movq      [ebp+24], mm5
        movq      [ecx+24], mm2
        movq      [ebx+24], mm3

        jne       @@B43
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure upassbig_3DNowExt(a : PComplexArray; w : PComplexArray3DNow; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 24
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        add       ebp, ebp
        sub       ebp, ecx
        add       ebp, ebp
        lea       ebx, DWORD PTR [ecx+ecx]

        movq      mm0, [eax]
        movq      mm1, [eax+ebx*8]
        movq      mm2, [eax+esi*8]
        movq      mm3, [eax+ebp*8]
        movq      mm4, mm2
        pfadd     mm4, mm3  
        pswapd    mm3, mm3
        movq      mm7, mm2  
        punpckldq mm7, mm3  
        punpckhdq mm3, mm2  
        pfsub     mm3, mm7  
        pswapd    mm3, mm3
        movq      mm5, mm0
        movq      mm6, mm1
        pfsub     mm0, mm4  
        pfsub     mm1, mm3  
        pfadd     mm5, mm4  
        pfadd     mm6, mm3  
        movq      [eax], mm5
        movq      [eax+ebx*8], mm6
        movq      [eax+esi*8], mm0
        movq      [eax+ebp*8], mm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+4], eax
        mov       DWORD PTR [esp+8], edx
        mov       DWORD PTR [esp], ecx
        mov       ecx, eax
@@B12:
        lea       eax, DWORD PTR [ecx+8]

        movq      mm0, [edx]
        movq      mm1, [edx+8]
        movq      mm2, [ecx+esi*8+8]         
        movq      mm3, [ecx+ebp*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        pswapd    mm5, mm5          
        pswapd    mm2, mm2          
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        pswapd    mm1, mm1          
        movq      mm2, [ecx+8]         
        movq      mm3, [ecx+ebx*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [ecx+8], mm4
        movq      [ecx+ebx*8+8], mm5
        movq      [ecx+esi*8+8], mm2
        movq      [ecx+ebp*8+8], mm3

        add       edx, 16
        mov       ecx, eax
        dec       edi
        jne       @@B12
@@B13:
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [eax+8]

        movq      mm0, [eax+esi*8+8]          
        movq      mm1, [eax+ebp*8+8]          
        movq      mm2, mm0           
        movq      mm3, mm1           
        pswapd    mm3, mm3
        pswapd    mm2, mm2
        pxor      mm2, Invert3DNow_H  
        pfsub     mm0, mm2
        pxor      mm3, Invert3DNow_H  
        pfadd     mm1, mm3
        pfmul     mm0, SqrtHalf3DNow  
        pfmul     mm1, SqrtHalf3DNow  
        movq      mm2, mm0
        pfadd     mm2, mm1            
        pswapd    mm0, mm0
        movq      mm4, mm0            
        punpckldq mm4, mm1            
        punpckhdq mm1, mm0            
        pfsub     mm4, mm1            
        movq      mm1, [eax+ebx*8+8]
        movq      mm3, [eax+8]
        movq      mm5, mm3
        movq      mm6, mm1
        pfsub     mm1, mm4         
        pfsub     mm3, mm2         
        pfadd     mm6, mm4         
        pfadd     mm5, mm2         
        movq      [eax+ebp*8+8], mm1
        movq      [eax+esi*8+8], mm3
        movq      [eax+ebx*8+8], mm6
        movq      [eax+8], mm5

        add       edx, -16
        mov       DWORD PTR [esp+12], ecx
        mov       DWORD PTR [esp+16], edi
        mov       DWORD PTR [esp+20], edx
@@B14:
        lea       eax, DWORD PTR [edi+8]

        movq      mm0, [edx+8]
        movq      mm1, [edx]
        movq      mm2, [edi+esi*8+8]         
        movq      mm3, [edi+ebp*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfmul     mm2, mm0          
        pfmul     mm3, mm0          
        pfmul     mm4, mm1          
        pfmul     mm5, mm1          
        pswapd    mm5, mm5          
        pswapd    mm2, mm2          
        pxor      mm5, Invert3DNow_H
        pfadd     mm3, mm5          
        pxor      mm4, Invert3DNow_H
        pfadd     mm2, mm4          
        movq      mm1, mm3          
        punpckldq mm1, mm2          
        punpckhdq mm2, mm3          
        movq      mm0, mm1          
        pfadd     mm0, mm2          
        pfsub     mm1, mm2          
        pswapd    mm1, mm1          
        movq      mm2, [edi+8]         
        movq      mm3, [edi+ebx*8+8]         
        movq      mm4, mm2          
        movq      mm5, mm3          
        pfsub     mm2, mm0          
        pfsub     mm3, mm1          
        pfadd     mm4, mm0          
        pfadd     mm5, mm1          
        movq      [edi+8], mm4
        movq      [edi+ebx*8+8], mm5
        movq      [edi+esi*8+8], mm2
        movq      [edi+ebp*8+8], mm3

        add       edx, -16
        mov       edi, eax
        dec       ecx
        jne       @@B14
@@B15:
        add       esp, 24
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure c32_3DNowExt(a : PComplexArray);
begin
  cpass_3DNowExt(a,d323DNow,4);
  c8_3DNowExt(@a^[16]);
  c8_3DNowExt(@a^[24]);
  c16_3DNowExt(a);
end;

procedure c64_3DNowExt(a : PComplexArray);
begin
  cpass_3DNowExt(a,d643DNow,8);
  c16_3DNowExt(@a^[32]);
  c16_3DNowExt(@a^[48]);
  c32_3DNowExt(a);
end;

procedure c128_3DNowExt(a : PComplexArray);
begin
  cpass_3DNowExt(a,d1283DNow,16);
  c32_3DNowExt(@a^[64]);
  c32_3DNowExt(@a^[96]);
  c64_3DNowExt(a);
end;

procedure c256_3DNowExt(a : PComplexArray);
begin
  cpass_3DNowExt(a,d2563DNow,32);
  c64_3DNowExt(@a^[128]);
  c64_3DNowExt(@a^[192]);
  c128_3DNowExt(a);
end;

procedure c512_3DNowExt(a : PComplexArray);
begin
  cpass_3DNowExt(a,d5123DNow,64);
  c128_3DNowExt(@a^[384]);
  c128_3DNowExt(@a^[256]);
  c256_3DNowExt(a);
end;
procedure c1024_3DNowExt(a : PComplexArray);
begin
  cpassbig_3DNowExt(a,d10243DNow,128);
  c256_3DNowExt(@a^[768]);
  c256_3DNowExt(@a^[512]);
  c512_3DNowExt(a);
end;

procedure c2048_3DNowExt(a : PComplexArray);
begin
  cpassbig_3DNowExt(a,d20483DNow,256);
  c512_3DNowExt(@a^[1536]);
  c512_3DNowExt(@a^[1024]);
  c1024_3DNowExt(a);
end;

procedure c4096_3DNowExt(a : PComplexArray);
begin
  cpassbig_3DNowExt(a,d40963DNow,512);
  c1024_3DNowExt(@a^[3072]);
  c1024_3DNowExt(@a^[2048]);
  c2048_3DNowExt(a);
end;

procedure c8192_3DNowExt(a : PComplexArray);
begin
  cpassbig_3DNowExt(a,d81923DNow,1024);
  c2048_3DNowExt(@a^[6144]);
  c2048_3DNowExt(@a^[4096]);
  c4096_3DNowExt(a);
end;

procedure u32_3DNowExt(a : PComplexArray);
begin
  u16_3DNowExt(a);
  u8_3DNowExt(@a^[16]);
  u8_3DNowExt(@a^[24]);
  upass_3DNowExt(a,d323DNow,4);
end;

procedure u64_3DNowExt(a : PComplexArray);
begin
  u32_3DNowExt(a);
  u16_3DNowExt(@a^[32]);
  u16_3DNowExt(@a^[48]);
  upass_3DNowExt(a,d643DNow,8);
end;

procedure u128_3DNowExt(a : PComplexArray);
begin
  u64_3DNowExt(a);
  u32_3DNowExt(@a^[64]);
  u32_3DNowExt(@a^[96]);
  upass_3DNowExt(a,d1283DNow,16);
end;

procedure u256_3DNowExt(a : PComplexArray);
begin
  u128_3DNowExt(a);
  u64_3DNowExt(@a^[128]);
  u64_3DNowExt(@a^[192]);
  upass_3DNowExt(a,d2563DNow,32);
end;

procedure u512_3DNowExt(a : PComplexArray);
begin
  u256_3DNowExt(a);
  u128_3DNowExt(@a^[256]);
  u128_3DNowExt(@a^[384]);
  upass_3DNowExt(a,d5123DNow,64);
end;

procedure u1024_3DNowExt(a : PComplexArray);
begin
  u512_3DNowExt(a);
  u256_3DNowExt(@a^[512]);
  u256_3DNowExt(@a^[768]);
  upassbig_3DNowExt(a,d10243DNow,128);
end;

procedure u2048_3DNowExt(a : PComplexArray);
begin
  u1024_3DNowExt(a);
  u512_3DNowExt(@a^[1024]);
  u512_3DNowExt(@a^[1536]);
  upassbig_3DNowExt(a,d20483DNow,256);
end;

procedure u4096_3DNowExt(a : PComplexArray);
begin
  u2048_3DNowExt(a);
  u1024_3DNowExt(@a^[2048]);
  u1024_3DNowExt(@a^[3072]);
  upassbig_3DNowExt(a,d40963DNow,512);
end;

procedure u8192_3DNowExt(a : PComplexArray);
begin
  u4096_3DNowExt(a);
  u2048_3DNowExt(@a^[4096]);
  u2048_3DNowExt(@a^[6144]);
  upassbig_3DNowExt(a,d81923DNow,1024);
end;

procedure fft4__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c4_3DNowExt(a);
  asm femms end;
end;

procedure ifft4__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u4_3DNowExt(a);
  asm femms end;
end;

procedure fft8__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c8_3DNowExt(a);
  asm femms end;
end;

procedure ifft8__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u8_3DNowExt(a);
  asm femms end;
end;

procedure fft16__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c16_3DNowExt(a);
  asm femms end;
end;

procedure ifft16__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u16_3DNowExt(a);
  asm femms end;
end;

procedure fft32__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c32_3DNowExt(a);
  asm femms end;
end;

procedure ifft32__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u32_3DNowExt(a);
  asm femms end;
end;

procedure fft64__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c64_3DNowExt(a);
  asm femms end;
end;

procedure ifft64__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u64_3DNowExt(a);
  asm femms end;
end;

procedure fft128__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c128_3DNowExt(a);
  asm femms end;
end;

procedure ifft128__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u128_3DNowExt(a);
  asm femms end;
end;

procedure fft256__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c256_3DNowExt(a);
  asm femms end;
end;

procedure ifft256__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u256_3DNowExt(a);
  asm femms end;
end;

procedure fft512__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c512_3DNowExt(a);
  asm femms end;
end;

procedure ifft512__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u512_3DNowExt(a);
  asm femms end;
end;

procedure fft1024__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c1024_3DNowExt(a);
  asm femms end;
end;

procedure ifft1024__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u1024_3DNowExt(a);
  asm femms end;
end;

procedure fft2048__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c2048_3DNowExt(a);
  asm femms end;
end;

procedure ifft2048__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u2048_3DNowExt(a);
  asm femms end;
end;

procedure fft4096__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c4096_3DNowExt(a);
  asm femms end;
end;

procedure ifft4096__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u4096_3DNowExt(a);
  asm femms end;
end;

procedure fft8192__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  c8192_3DNowExt(a);
  asm femms end;
end;

procedure ifft8192__3DNowExt(a : PComplexArray);
begin
  asm femms end;
  u8192_3DNowExt(a);
  asm femms end;
end;

procedure c2_SSE(a : PComplexArray; Param : Pointer);
asm
        movaps    xmm0, [eax]
        movhlps   xmm1, xmm0
        movlhps   xmm1, xmm0
        xorps     xmm0, DQWORD [Param+80]
        addps     xmm1, xmm0
        movaps    [eax], xmm1
end;

procedure u2_SSE(a : PComplexArray; Param : Pointer);
begin
  c2_SSE(a,Param);
end;

procedure c4_SSE(a : PComplexArray; Param : Pointer);
asm
        movaps    xmm2, [eax]
        movaps    xmm4, [eax+16]
        movaps    xmm0, xmm2
        addps     xmm2, xmm4         
        subps     xmm0, xmm4         
        movaps    xmm3, xmm2         
        shufps    xmm3, xmm0, $14    
        shufps    xmm2, xmm0, $EE    
        movaps    xmm0, xmm3         
        addps     xmm0, xmm2         
        subps     xmm3, xmm2         
        movaps    xmm2, xmm0         
        movlhps   xmm2, xmm3         
        movaps    [eax], xmm2
        movhlps   xmm0, xmm3         
        shufps    xmm0, xmm0, $39    
        movaps    [eax+16], xmm0
end;

procedure c8_SSE(a : PComplexArray; Param : Pointer);
asm
        movaps    xmm0, [eax]        
        movaps    xmm1, [eax+32]     
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movaps    xmm3, [eax+16]     
        movaps    xmm1, [eax+48]     
        movaps    xmm4, xmm3         
        subps     xmm3, xmm1         
        addps     xmm4, xmm1         
        movaps    xmm1, xmm0         
        shufps    xmm3, xmm3, $B1    
        subps     xmm1, xmm3         
        addps     xmm0, xmm3         
        movaps    xmm7, xmm0         
        movaps    xmm3, xmm1         
        shufps    xmm3, xmm3, $FA    
        shufps    xmm7, xmm7, $AF    
        movaps    xmm5, [Param]   
        movaps    xmm6, DQWORD [Param+16] 
        xorps     xmm7, xmm5
        addps     xmm3, xmm7
        mulps     xmm3, xmm6         
        movlhps   xmm1, xmm0         
        shufps    xmm1, xmm1, $6C    
        movaps    xmm0, xmm1         
        addps     xmm0, xmm3         
        subps     xmm1, xmm3         
        movaps    xmm7, xmm0         
        movlhps   xmm7, xmm1         
        movhlps   xmm1, xmm0         
        movaps    [eax+32], xmm7
        movaps    [eax+48], xmm1
        movaps    xmm0, xmm2
        addps     xmm2, xmm4         
        subps     xmm0, xmm4         
        movaps    xmm3, xmm2         
        shufps    xmm3, xmm0, $14    
        shufps    xmm2, xmm0, $EE    
        movaps    xmm0, xmm3         
        addps     xmm0, xmm2         
        subps     xmm3, xmm2         
        movaps    xmm2, xmm0         
        movlhps   xmm2, xmm3         
        movaps    [eax], xmm2
        movhlps   xmm0, xmm3         
        shufps    xmm0, xmm0, $39    
        movaps    [eax+16], xmm0
end;

procedure c16_SSE(a : PComplexArray; Param : Pointer);
asm
        movlps    xmm0, [eax]
        movhps    xmm0, [eax+32]      
        movlps    xmm1, [eax+64]
        movhps    xmm1, [eax+96]      
        movaps    xmm2, xmm0
        subps     xmm2, xmm1          
        addps     xmm0, xmm1          
        movlps    [eax], xmm0
        movhps    [eax+32], xmm0
        movaps    xmm1, xmm2          
        movlhps   xmm1, xmm1          
        shufps    xmm2, xmm2, $BB     
        movaps    xmm5, [Param]  
        xorps     xmm2, xmm5
        addps     xmm1, xmm2
        movlps    [eax+64], xmm1
        movhps    [eax+96], xmm1
        movlps    xmm0, [eax+8]
        movhps    xmm0, [eax+40]      
        movlps    xmm1, [eax+72]
        movhps    xmm1, [eax+104]     
        movaps    xmm2, xmm0
        subps     xmm2, xmm1          
        addps     xmm0, xmm1          
        movlps    [eax+8], xmm0
        movhps    [eax+40], xmm0
        movaps    xmm1, xmm2          
        movlhps   xmm1, xmm1          
        shufps    xmm2, xmm2, $BB     
        xorps     xmm2, xmm5
        addps     xmm1, xmm2          
        movaps    xmm2, xmm1          
        shufps    xmm2, xmm2, $B1     
        movaps    xmm6, DQWORD [Param+32] 
        movaps    xmm7, DQWORD [Param+48] 
        mulps     xmm1, xmm6
        mulps     xmm2, xmm7
        xorps     xmm2, xmm5
        addps     xmm1, xmm2
        movlps    [eax+72], xmm1
        movhps    [eax+104], xmm1
        movlps    xmm0, [eax+16]
        movhps    xmm0, [eax+48]      
        movlps    xmm1, [eax+80]
        movhps    xmm1, [eax+112]     
        movaps    xmm2, xmm0
        subps     xmm2, xmm1          
        addps     xmm0, xmm1          
        movlps    [eax+16], xmm0
        movhps    [eax+48], xmm0
        movaps    xmm1, xmm2          
        movlhps   xmm1, xmm1          
        shufps    xmm2, xmm2, $BB     
        movaps    xmm0, DQWORD [Param+64] 
        xorps     xmm2, xmm0
        addps     xmm1, xmm2          
        movaps    xmm2, xmm1          
        shufps    xmm2, xmm2, $B1     
        xorps     xmm2, xmm0
        addps     xmm1, xmm2
        movaps    xmm4, DQWORD [Param+16] 
        mulps     xmm1, xmm4
        movlps    [eax+112], xmm1
        movhps    [eax+80], xmm1
        movlps    xmm0, [eax+24]
        movhps    xmm0, [eax+56]      
        movlps    xmm1, [eax+88]
        movhps    xmm1, [eax+120]     
        movaps    xmm2, xmm0
        subps     xmm2, xmm1          
        addps     xmm0, xmm1          
        movlps    [eax+24], xmm0
        movhps    [eax+56], xmm0
        movaps    xmm1, xmm2          
        movlhps   xmm1, xmm1          
        shufps    xmm2, xmm2, $BB     
        xorps     xmm2, xmm5
        addps     xmm1, xmm2          
        movaps    xmm2, xmm1          
        shufps    xmm2, xmm2, $B1     
        mulps     xmm1, xmm7
        mulps     xmm2, xmm6
        xorps     xmm2, xmm5
        addps     xmm1, xmm2
        movlps    [eax+88], xmm1
        movhps    [eax+120], xmm1

        movaps    xmm7, [eax]        
        movaps    xmm1, [eax+32]     
        movaps    xmm2, xmm7         
        subps     xmm7, xmm1         
        addps     xmm2, xmm1         
        movaps    xmm3, [eax+16]     
        movaps    xmm1, [eax+48]     
        movaps    xmm6, xmm3         
        subps     xmm3, xmm1         
        addps     xmm6, xmm1         
        movaps    xmm1, xmm7         
        shufps    xmm3, xmm3, $B1    
        subps     xmm1, xmm3         
        addps     xmm7, xmm3         
        movaps    xmm0, xmm7         
        movaps    xmm3, xmm1         
        shufps    xmm3, xmm3, $FA    
        shufps    xmm7, xmm7, $AF    
        xorps     xmm7, xmm5
        addps     xmm3, xmm7
        mulps     xmm3, xmm4         
        movlhps   xmm1, xmm0         
        shufps    xmm1, xmm1, $6C    
        movaps    xmm0, xmm1         
        addps     xmm0, xmm3         
        subps     xmm1, xmm3         
        movaps    xmm7, xmm0         
        movlhps   xmm7, xmm1         
        movhlps   xmm1, xmm0         
        movaps    [eax+32], xmm7
        movaps    [eax+48], xmm1
        movaps    xmm0, xmm2
        addps     xmm2, xmm6         
        subps     xmm0, xmm6         
        movaps    xmm3, xmm2         
        shufps    xmm3, xmm0, $14    
        shufps    xmm2, xmm0, $EE    
        movaps    xmm0, xmm3         
        addps     xmm0, xmm2         
        subps     xmm3, xmm2         
        movaps    xmm2, xmm0         
        movlhps   xmm2, xmm3         
        movaps    [eax], xmm2
        movhlps   xmm0, xmm3         
        shufps    xmm0, xmm0, $39    
        movaps    [eax+16], xmm0

        movaps    xmm0, [eax+64]     
        movaps    xmm1, [eax+80]     
        movaps    xmm2, xmm0
        addps     xmm2, xmm1         
        subps     xmm0, xmm1         
        movaps    xmm3, xmm2         
        shufps    xmm3, xmm0, $14    
        shufps    xmm2, xmm0, $EE    
        movaps    xmm0, xmm3         
        addps     xmm0, xmm2         
        subps     xmm3, xmm2         
        movaps    xmm2, xmm0         
        movlhps   xmm2, xmm3         
        movaps    [eax+64], xmm2
        movhlps   xmm0, xmm3         
        shufps    xmm0, xmm0, $39    
        movaps    [eax+80], xmm0

        movaps    xmm0, [eax+96]     
        movaps    xmm1, [eax+112]    
        movaps    xmm2, xmm0
        addps     xmm2, xmm1         
        subps     xmm0, xmm1         
        movaps    xmm3, xmm2         
        shufps    xmm3, xmm0, $14    
        shufps    xmm2, xmm0, $EE    
        movaps    xmm0, xmm3         
        addps     xmm0, xmm2         
        subps     xmm3, xmm2         
        movaps    xmm2, xmm0         
        movlhps   xmm2, xmm3         
        movaps    [eax+96], xmm2
        movhlps   xmm0, xmm3         
        shufps    xmm0, xmm0, $39    
        movaps    [eax+112], xmm0
end;

procedure cpass_SSE(a : PComplexArray; w : PComplexArraySSE; n : integer; Param : Pointer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        mov       ebx, DWORD PTR [esp+24]
        movaps    xmm7, [ebx] 
        sub       esp, 12

        lea       ebx, DWORD PTR [ecx+ecx]
        mov       DWORD PTR [esp+8], eax
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, eax
        lea       edi, DWORD PTR [ecx-1]
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx

        add       ecx, ecx
        lea       ebp, DWORD PTR [eax+ecx]
        lea       esi, DWORD PTR [ebx+ecx]

        movlps    xmm0, [eax]        
        movhps    xmm0, [ebp]        
        movlps    xmm1, [ebx]        
        movhps    xmm1, [esi]        
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax], xmm2        
        movhps    [ebp], xmm2        
        movaps    xmm1, xmm0
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0
        movlps    [ebx], xmm1
        movhps    [esi], xmm1

        movlps    xmm0, [eax+8]      
        movhps    xmm0, [ebp+8]      
        movlps    xmm1, [ebx+8]      
        movhps    xmm1, [esi+8]      
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax+8], xmm2      
        movhps    [ebp+8], xmm2      
        movaps    xmm1, xmm0         
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0
        movaps    xmm5, [edx]
        movaps    xmm6, [edx+16]
        movaps    xmm0, xmm1         
        shufps    xmm0, xmm0, $B1    
        mulps     xmm1, xmm5
        mulps     xmm0, xmm6
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movlps   [ebx+8], xmm1
        movhps   [esi+8], xmm1

        mov       DWORD PTR [esp+4], ecx
        jmp       @@B92
@@B93:
        mov       ebp, DWORD PTR [esp]
        mov       ebx, ecx
        mov       eax, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebp
        lea       ebp, DWORD PTR [ebp+eax]
        lea       esi, DWORD PTR [ecx+eax]
@@B92:
        mov       eax, DWORD PTR [esp+8]
        lea       ecx, DWORD PTR [eax+16]
        mov       DWORD PTR [esp], ecx
        lea       ecx, DWORD PTR [ebx+16]

        movlps    xmm0, [eax+16]     
        movhps    xmm0, [ebp+16]     
        movlps    xmm1, [ebx+16]     
        movhps    xmm1, [esi+16]     
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax+16], xmm2     
        movhps    [ebp+16], xmm2     
        movaps    xmm1, xmm0         
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movaps    xmm5, [edx+32]
        movaps    xmm6, [edx+48]
        movaps    xmm0, xmm1         
        shufps    xmm0, xmm0, $B1    
        mulps     xmm1, xmm5
        mulps     xmm0, xmm6
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movlps   [ebx+16], xmm1
        movhps   [esi+16], xmm1

        add       edx, 64
        dec       edi

        movlps    xmm0, [eax+24]     
        movhps    xmm0, [ebp+24]     
        movlps    xmm1, [ebx+24]     
        movhps    xmm1, [esi+24]     
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax+24], xmm2     
        movhps    [ebp+24], xmm2     
        movaps    xmm1, xmm0         
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movaps    xmm5, [edx]
        movaps    xmm6, [edx+16]
        movaps    xmm0, xmm1         
        shufps    xmm0, xmm0, $B1    
        mulps     xmm1, xmm5
        mulps     xmm0, xmm6
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movlps   [ebx+24], xmm1
        movhps   [esi+24], xmm1

        jne       @@B93
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure cpassbig_SSE(a : PComplexArray; w : PComplexArraySSE; n : integer; Param : Pointer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        mov       ebx, DWORD PTR [esp+24]
        movaps    xmm7, [ebx]  
        movaps    xmm3, [ebx+64] 
        movaps    xmm4, [ebx+16] 
        sub       esp, 32
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        lea       ebx, DWORD PTR [ecx+ecx]
        add       ebx, ebx
        sub       ebx, ecx
        add       ebx, ebx

        movlps    xmm0, [eax]        
        movhps    xmm0, [eax+ebp*8]        
        movlps    xmm1, [eax+esi*8]        
        movhps    xmm1, [eax+ebx*8]        
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax], xmm2        
        movhps    [eax+ebp*8], xmm2        
        movaps    xmm1, xmm0
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0
        movlps    [eax+esi*8], xmm1
        movhps    [eax+ebx*8], xmm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+12], edx
        mov       DWORD PTR [esp+16], eax
        mov       DWORD PTR [esp+8], ecx
@@B122:
        lea       ecx, DWORD PTR [eax+8]

        movlps    xmm0, [eax+8]      
        movhps    xmm0, [eax+ebp*8+8]      
        movlps    xmm1, [eax+esi*8+8]      
        movhps    xmm1, [eax+ebx*8+8]      
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax+8], xmm2      
        movhps    [eax+ebp*8+8], xmm2      
        movaps    xmm1, xmm0         
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movaps    xmm5, [edx]
        movaps    xmm6, [edx+16]
        movaps    xmm0, xmm1         
        shufps    xmm0, xmm0, $B1    
        mulps     xmm1, xmm5
        mulps     xmm0, xmm6
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movlps   [eax+esi*8+8], xmm1
        movhps   [eax+ebx*8+8], xmm1

        mov       eax, ecx
        add       edx, 32
        dec       edi
        jne       @@B122

        mov       DWORD PTR [esp], ecx
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+8], ecx
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [ecx+8]
        mov       DWORD PTR [esp+4], edi

        movlps    xmm0, [ecx+8]
        movhps    xmm0, [eax+ebp*8+8]      
        movlps    xmm1, [eax+esi*8+8]
        movhps    xmm1, [eax+ebx*8+8]     
        movaps    xmm2, xmm0
        subps     xmm2, xmm1          
        addps     xmm0, xmm1          
        movlps    [ecx+8], xmm0
        movhps    [eax+ebp*8+8], xmm0
        movaps    xmm1, xmm2          
        movlhps   xmm1, xmm1          
        shufps    xmm2, xmm2, $BB     
        xorps     xmm2, xmm3
        addps     xmm1, xmm2          
        movaps    xmm2, xmm1          
        shufps    xmm2, xmm2, $B1     
        xorps     xmm2, xmm3
        addps     xmm1, xmm2
        mulps     xmm1, xmm4
        movlps    [eax+ebx*8+8], xmm1
        movhps    [eax+esi*8+8], xmm1

        add       edx, -32
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+20], ecx
        mov       DWORD PTR [esp+24], edx
        mov       DWORD PTR [esp+28], edi
        mov       eax, edi
@@B124:
        lea       edi, DWORD PTR [eax+8]

        movlps    xmm0, [eax+8]      
        movhps    xmm0, [eax+ebp*8+8]      
        movlps    xmm1, [eax+esi*8+8]      
        movhps    xmm1, [eax+ebx*8+8]      
        movaps    xmm2, xmm0         
        subps     xmm0, xmm1         
        addps     xmm2, xmm1         
        movlps    [eax+8], xmm2      
        movhps    [eax+ebp*8+8], xmm2      
        movaps    xmm1, xmm0         
        movlhps   xmm1, xmm1         
        shufps    xmm0, xmm0, $BB    
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movaps    xmm5, [edx+16]
        movaps    xmm6, [edx]
        movaps    xmm0, xmm1         
        shufps    xmm0, xmm0, $B1    
        mulps     xmm1, xmm5
        mulps     xmm0, xmm6
        xorps     xmm0, xmm7
        addps     xmm1, xmm0         
        movlps   [eax+esi*8+8], xmm1
        movhps   [eax+ebx*8+8], xmm1

        mov       eax, edi
        add       edx, -32
        dec       ecx
        jne       @@B124

        add       esp, 32
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure u4_SSE(a : PComplexArray; Param : Pointer);
asm
        movlps    xmm0, [eax]
        movlhps   xmm0, xmm0      
        movlps    xmm1, [eax+8]
        movlhps   xmm1, xmm1      
        movaps    xmm3, DQWORD [Param+80] 
        xorps     xmm1, xmm3
        addps     xmm0, xmm1      
        movaps    xmm1, [eax+16]  
        movaps    xmm2, xmm1      
        shufps    xmm1, xmm1, $9E 
        shufps    xmm2, xmm2, $34 
        xorps     xmm2, xmm3
        addps     xmm1, xmm2      
        movaps    xmm2, xmm0      
        addps     xmm0, xmm1      
        subps     xmm2, xmm1      
        movaps    [eax], xmm0
        movaps    [eax+16], xmm2
end;
procedure u8_SSE(a : PComplexArray; Param : Pointer);
asm
        movlps    xmm0, [eax]
        movlhps   xmm0, xmm0      
        movlps    xmm1, [eax+8]
        movlhps   xmm1, xmm1      
        movaps    xmm3, DQWORD [Param+80] 
        xorps     xmm1, xmm3
        addps     xmm0, xmm1      
        movaps    xmm1, [eax+16]  
        movaps    xmm2, xmm1      
        shufps    xmm1, xmm1, $9E 
        shufps    xmm2, xmm2, $34 
        xorps     xmm2, xmm3
        addps     xmm1, xmm2      
        movaps    xmm2, xmm0      
        addps     xmm0, xmm1      
        subps     xmm2, xmm1      
        movlps    [eax], xmm0
        movhps    [eax+24], xmm2

        movlps    xmm6, [eax+32]
        movhps    xmm6, [eax+48]
        movlps    xmm1, [eax+40]
        movhps    xmm1, [eax+56]
        movaps    xmm5, xmm6
        addps     xmm6, xmm1          
        subps     xmm5, xmm1          
        movaps    xmm1, xmm6          
        shufps    xmm1, xmm1, $49     
        shufps    xmm6, xmm6, $E3     
        xorps     xmm6, xmm3
        subps     xmm1, xmm6          
        movhps    xmm2, [eax]
        movaps    xmm7, xmm2
        subps     xmm7, xmm1          
        addps     xmm2, xmm1          
        movlps    [eax+48], xmm7
        movhps    [eax+32], xmm7
        movlps    [eax+16], xmm2
        movhps    [eax], xmm2
        movaps    xmm6, xmm5          
        shufps    xmm5, xmm5, $89     
        shufps    xmm6, xmm6, $DC     
        xorps     xmm6, xmm3
        subps     xmm5, xmm6
        movaps    xmm6, DQWORD [Param+16] 
        mulps     xmm5, xmm6          
        movaps    xmm1, xmm5          
        shufps    xmm1, xmm1, $24     
        shufps    xmm5, xmm5, $DB     
        xorps     xmm5, xmm3
        subps     xmm1, xmm5          
        movlps    xmm0, [eax+24]
        movaps    xmm2, xmm0
        subps     xmm0, xmm1          
        addps     xmm2, xmm1          
        movlps    [eax+56], xmm0
        movhps    [eax+40], xmm0
        movlps    [eax+24], xmm2
        movhps    [eax+8], xmm2
end;

procedure u16_SSE(a : PComplexArray; Param : Pointer);
asm
        movlps    xmm0, [eax+64]
        movlhps   xmm0, xmm0      
        movlps    xmm1, [eax+72]
        movlhps   xmm1, xmm1      
        movaps    xmm3, DQWORD [Param+80] 
        xorps     xmm1, xmm3
        addps     xmm0, xmm1      
        movlps    xmm1, [eax+80]
        movhps    xmm1, [eax+88]  
        movaps    xmm2, xmm1      
        shufps    xmm1, xmm1, $9E 
        shufps    xmm2, xmm2, $34 
        xorps     xmm2, xmm3
        addps     xmm1, xmm2      
        movaps    xmm2, xmm0      
        addps     xmm0, xmm1      
        subps     xmm2, xmm1      
        movhps    [eax+72], xmm0
        movlps    [eax+80], xmm2
        movhps    [eax+88], xmm2

        movlps    xmm5, [eax+96]
        movlhps   xmm5, xmm5      
        movlps    xmm1, [eax+104]
        movlhps   xmm1, xmm1      
        xorps     xmm1, xmm3
        addps     xmm5, xmm1      
        movlps    xmm1, [eax+112]
        movhps    xmm1, [eax+120]  
        movaps    xmm2, xmm1      
        shufps    xmm1, xmm1, $9E 
        shufps    xmm2, xmm2, $34 
        xorps     xmm2, xmm3
        addps     xmm1, xmm2      
        movaps    xmm2, xmm5      
        addps     xmm5, xmm1      
        subps     xmm2, xmm1      
        movlps    [eax+96], xmm5
        movhps    [eax+104], xmm5
        movlps    [eax+112], xmm2
        movhps    [eax+120], xmm2

        movlps    xmm5, [eax]
        movlhps   xmm5, xmm5      
        movlps    xmm1, [eax+8]
        movlhps   xmm1, xmm1      
        xorps     xmm1, xmm3
        addps     xmm5, xmm1      
        movaps    xmm1, [eax+16]  
        movaps    xmm2, xmm1      
        shufps    xmm1, xmm1, $9E 
        shufps    xmm2, xmm2, $34 
        xorps     xmm2, xmm3
        addps     xmm1, xmm2      
        movaps    xmm2, xmm5      
        addps     xmm5, xmm1      
        subps     xmm2, xmm1      
        movlps    [eax], xmm5
        movhps    [eax+8], xmm5
        movlps    [eax+16], xmm2
        movhps    [eax+24], xmm2

        movlps    xmm5, [eax+32]
        movhps    xmm5, [eax+48]
        movlps    xmm1, [eax+40]
        movhps    xmm1, [eax+56]
        movaps    xmm2, xmm5
        addps     xmm5, xmm1          
        subps     xmm2, xmm1          
        movaps    xmm1, xmm5          
        shufps    xmm1, xmm1, $49     
        shufps    xmm5, xmm5, $E3     
        xorps     xmm5, xmm3
        subps     xmm1, xmm5          
        movlps    xmm5, [eax+16]
        movhps    xmm5, [eax]
        movaps    xmm4, xmm5
        subps     xmm4, xmm1          
        addps     xmm5, xmm1          
        movlps    [eax+48], xmm4
        movlps    [eax+16], xmm5
        movhps    [eax], xmm5
        movaps    xmm5, xmm2          
        shufps    xmm2, xmm2, $89     
        shufps    xmm5, xmm5, $DC     
        xorps     xmm5, xmm3
        subps     xmm2, xmm5
        movaps    xmm6, DQWORD [Param+16]
        mulps     xmm2, xmm6          
        movaps    xmm1, xmm2          
        shufps    xmm1, xmm1, $24     
        shufps    xmm2, xmm2, $DB     
        xorps     xmm2, xmm3
        subps     xmm1, xmm2          
        movlps    xmm5, [eax+24]
        movhps    xmm5, [eax+8]
        movaps    xmm2, xmm5
        subps     xmm2, xmm1          
        addps     xmm5, xmm1          
        movlps    [eax+56], xmm2
        movhps    [eax+40], xmm2
        movhps    [eax+8], xmm5

        movhps    xmm0, [eax+96]      
        movaps    xmm1, xmm0
        shufps    xmm0, xmm0, $94 
        shufps    xmm1, xmm1, $3E 
        xorps     xmm1, xmm3
        addps     xmm0, xmm1
        movlps    xmm4, [eax]
        movaps    xmm2, xmm4
        subps     xmm4, xmm0      
        addps     xmm2, xmm0      
        movlps    [eax], xmm2
        movhps    [eax+32], xmm2
        movlps    [eax+64], xmm4
        movhps    [eax+96], xmm4

        movlps    xmm0, [eax+80]
        movhps    xmm0, [eax+112]       
        movaps    xmm1, xmm0
        shufps    xmm1, xmm1, $B1  
        movaps    xmm4, DQWORD [Param+64] 
        xorps     xmm1, xmm4
        addps     xmm0, xmm1
        mulps     xmm0, xmm6       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $49  
        shufps    xmm0, xmm0, $E3  
        xorps     xmm0, xmm3
        subps     xmm1, xmm0       
        movlps    xmm0, [eax+48]
        movhps    xmm0, [eax+16]       
        movaps    xmm2, xmm0
        subps     xmm0, xmm1       
        addps     xmm2, xmm1       
        movlps    [eax+48], xmm2
        movhps    [eax+16], xmm2
        movlps    [eax+112], xmm0
        movhps    [eax+80], xmm0

        movlps    xmm0, [eax+72]
        movhps    xmm0, [eax+104]       
        movaps    xmm1, xmm0
        movaps    xmm6, DQWORD [Param+32]  
        movaps    xmm7, DQWORD [Param+48]  
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm4
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movlps    xmm1, [eax+8]
        movhps    xmm1, [eax+40]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [eax+8], xmm1
        movhps    [eax+40], xmm1
        movlps    [eax+72], xmm2
        movhps    [eax+104], xmm2

        movlps    xmm0, [eax+88]
        movhps    xmm0, [eax+120]       
        movaps    xmm1, xmm0
        mulps     xmm0, xmm7       
        mulps     xmm1, xmm6       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm4
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movhps    xmm5, [eax+56]       
        movaps    xmm2, xmm5
        subps     xmm2, xmm0       
        addps     xmm5, xmm0       
        movlps    [eax+24], xmm5
        movhps    [eax+56], xmm5
        movlps    [eax+88], xmm2
        movhps    [eax+120], xmm2
end;
{*******************************************************************************}
procedure upass_SSE(a : PComplexArray; w : PComplexArraySSE; n : integer; Param : Pointer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        mov       ebx, DWORD PTR [esp+24]
        movaps    xmm5, [ebx+80] 
        movaps    xmm3, [ebx+64] 

        sub       esp, 12
        lea       edi, DWORD PTR [ecx+ecx]
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, eax
        lea       esi, DWORD PTR [ecx-1]
        mov       DWORD PTR [esp+8], edi
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebx, DWORD PTR [edi+ecx]
        lea       ebp, DWORD PTR [eax+ecx]

        movlps    xmm0, [edi]
        movhps    xmm0, [ebx]      
        movaps    xmm1, xmm0
        shufps    xmm0, xmm0, $94 
        shufps    xmm1, xmm1, $3E 
        xorps     xmm1, xmm5
        addps     xmm0, xmm1
        movlps    xmm1, [eax]
        movhps    xmm1, [ebp]      
        movaps    xmm2, xmm1
        subps     xmm1, xmm0      
        addps     xmm2, xmm0      
        movlps    [eax], xmm2
        movhps    [ebp], xmm2
        movlps    [edi], xmm1
        movhps    [ebx], xmm1

        movlps    xmm0, [edi+8]
        movhps    xmm0, [ebx+8]       
        movaps    xmm1, xmm0
        movaps    xmm6, [edx]
        movaps    xmm7, [edx+16]
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm5
        addps     xmm0, xmm1       
        movlps    xmm1, [eax+8]
        movhps    xmm1, [ebp+8]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [eax+8], xmm1
        movhps    [ebp+8], xmm1
        movlps    [edi+8], xmm2
        movhps    [ebx+8], xmm2

        mov       DWORD PTR [esp+4], ecx
        jmp       @@B42
@@B43:
        mov       eax, edi
        mov       ebx, DWORD PTR [esp]
        mov       ecx, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebx
        lea       ebx, DWORD PTR [ebx+ecx]
        lea       ebp, DWORD PTR [edi+ecx]
@@B42:
        mov       ecx, DWORD PTR [esp+8]
        lea       edi, DWORD PTR [ecx+16]
        mov       DWORD PTR [esp], edi

        movlps    xmm0, [ecx+16]
        movhps    xmm0, [ebx+16]   
        movaps    xmm1, xmm0
        movaps    xmm6, [edx+32]
        movaps    xmm7, [edx+48]
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm5
        addps     xmm0, xmm1       
        movlps    xmm1, [eax+16]
        movhps    xmm1, [ebp+16]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [eax+16], xmm1
        movhps    [ebp+16], xmm1
        movlps    [ecx+16], xmm2
        movhps    [ebx+16], xmm2

        lea       edi, DWORD PTR [eax+16]
        add       edx, 64

        movlps    xmm0, [ecx+24]
        movhps    xmm0, [ebx+24]   
        movaps    xmm1, xmm0
        movaps    xmm6, [edx]
        movaps    xmm7, [edx+16]
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm5
        addps     xmm0, xmm1       
        movlps    xmm1, [eax+24]
        movhps    xmm1, [ebp+24]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [eax+24], xmm1
        movhps    [ebp+24], xmm1
        movlps    [ecx+24], xmm2
        movhps    [ebx+24], xmm2

        dec       esi
        jne       @@B43
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure upassbig_SSE(a : PComplexArray; w : PComplexArraySSE; n : integer; Param : Pointer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx

        mov       ebx, DWORD PTR [esp+24]
        movaps    xmm5, [ebx+80] 
        movaps    xmm3, [ebx+64] 
        movaps    xmm4, [ebx+16] 

        sub       esp, 24
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        lea       ebp, DWORD PTR [ecx+ecx]
        add       ebp, ebp
        sub       ebp, ecx
        add       ebp, ebp
        lea       ebx, DWORD PTR [ecx+ecx]

        movlps    xmm0, [eax+esi*8]
        movhps    xmm0, [eax+ebp*8]      
        movaps    xmm1, xmm0
        shufps    xmm0, xmm0, $94 
        shufps    xmm1, xmm1, $3E 
        xorps     xmm1, xmm5
        addps     xmm0, xmm1
        movlps    xmm1, [eax]
        movhps    xmm1, [eax+ebx*8]      
        movaps    xmm2, xmm1
        subps     xmm1, xmm0      
        addps     xmm2, xmm0      
        movlps    [eax], xmm2
        movhps    [eax+ebx*8], xmm2
        movlps    [eax+esi*8], xmm1
        movhps    [eax+ebp*8], xmm1

        dec       ecx
        mov       edi, ecx
        mov       DWORD PTR [esp+4], eax
        mov       DWORD PTR [esp+8], edx
        mov       DWORD PTR [esp], ecx
        mov       ecx, eax
@@B12:
        lea       eax, DWORD PTR [ecx+8]

        movlps    xmm0, [ecx+esi*8+8]
        movhps    xmm0, [ecx+ebp*8+8]       
        movaps    xmm1, xmm0
        movaps    xmm6, [edx]
        movaps    xmm7, [edx+16]
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm5
        addps     xmm0, xmm1       
        movlps    xmm1, [ecx+8]
        movhps    xmm1, [ecx+ebx*8+8]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [ecx+8], xmm1
        movhps    [ecx+ebx*8+8], xmm1
        movlps    [ecx+esi*8+8], xmm2
        movhps    [ecx+ebp*8+8], xmm2

        add       edx, 32
        mov       ecx, eax
        dec       edi
        jne       @@B12

        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [eax+8]

        movlps    xmm0, [eax+esi*8+8]
        movhps    xmm0, [eax+ebp*8+8]          
        movaps    xmm1, xmm0          
        shufps    xmm1, xmm1, $B1     
        xorps     xmm1, xmm3
        addps     xmm0, xmm1
        mulps     xmm0, xmm4          
        movaps    xmm1, xmm0          
        shufps    xmm1, xmm1, $49     
        shufps    xmm0, xmm0, $E3     
        xorps     xmm0, xmm5
        subps     xmm1, xmm0          
        movlps    xmm0, [eax+ebx*8+8]
        movhps    xmm0, [eax+8]          
        movaps    xmm2, xmm0          
        subps     xmm0, xmm1          
        addps     xmm2, xmm1          
        movhps    [eax+8], xmm2
        movlps    [eax+ebx*8+8], xmm2
        movhps    [eax+esi*8+8], xmm0
        movlps    [eax+ebp*8+8], xmm0

        add       edx, -32
        mov       DWORD PTR [esp+12], ecx
        mov       DWORD PTR [esp+16], edi
        mov       DWORD PTR [esp+20], edx
@@B14:
        lea       eax, DWORD PTR [edi+8]

        movlps    xmm0, [edi+esi*8+8]
        movhps    xmm0, [edi+ebp*8+8]       
        movaps    xmm1, xmm0
        movaps    xmm6, [edx+16]
        movaps    xmm7, [edx]
        mulps     xmm0, xmm6       
        mulps     xmm1, xmm7       
        shufps    xmm1, xmm1, $B1  
        xorps     xmm1, xmm3
        addps     xmm0, xmm1       
        movaps    xmm1, xmm0       
        shufps    xmm1, xmm1, $3C  
        shufps    xmm0, xmm0, $96  
        xorps     xmm1, xmm5
        addps     xmm0, xmm1       
        movlps    xmm1, [edi+8]
        movhps    xmm1, [edi+ebx*8+8]       
        movaps    xmm2, xmm1
        subps     xmm2, xmm0       
        addps     xmm1, xmm0       
        movlps    [edi+8], xmm1
        movhps    [edi+ebx*8+8], xmm1
        movlps    [edi+esi*8+8], xmm2
        movhps    [edi+ebp*8+8], xmm2

        add       edx, -32
        mov       edi, eax
        dec       ecx
        jne       @@B14

        add       esp, 24
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure c32_SSE(a : PComplexArray);
begin
  cpass_SSE(a,d32SSE,4,Params);
  c8_SSE(@a^[16],Params);
  c8_SSE(@a^[24],Params);
  c16_SSE(a,Params);
end;

procedure c64_SSE(a : PComplexArray);
begin
  cpass_SSE(a,d64SSE,8,Params);
  c16_SSE(@a^[32],Params);
  c16_SSE(@a^[48],Params);
  c32_SSE(a);
end;

procedure c128_SSE(a : PComplexArray);
begin
  cpass_SSE(a,d128SSE,16,Params);
  c32_SSE(@a^[64]);
  c32_SSE(@a^[96]);
  c64_SSE(a);
end;

procedure c256_SSE(a : PComplexArray);
begin
  cpass_SSE(a,d256SSE,32,Params);
  c64_SSE(@a^[128]);
  c64_SSE(@a^[192]);
  c128_SSE(a);
end;

procedure c512_SSE(a : PComplexArray);
begin
  cpass_SSE(a,d512SSE,64,Params);
  c128_SSE(@a^[384]);
  c128_SSE(@a^[256]);
  c256_SSE(a);
end;
procedure c1024_SSE(a : PComplexArray);
begin
  cpassbig_SSE(a,d1024SSE,128,Params);
  c256_SSE(@a^[768]);
  c256_SSE(@a^[512]);
  c512_SSE(a);
end;

procedure c2048_SSE(a : PComplexArray);
begin
  cpassbig_SSE(a,d2048SSE,256,Params);
  c512_SSE(@a^[1536]);
  c512_SSE(@a^[1024]);
  c1024_SSE(a);
end;

procedure c4096_SSE(a : PComplexArray);
begin
  cpassbig_SSE(a,d4096SSE,512,Params);
  c1024_SSE(@a^[3072]);
  c1024_SSE(@a^[2048]);
  c2048_SSE(a);
end;

procedure c8192_SSE(a : PComplexArray);
begin
  cpassbig_SSE(a,d8192SSE,1024,Params);
  c2048_SSE(@a^[6144]);
  c2048_SSE(@a^[4096]);
  c4096_SSE(a);
end;

procedure u32_SSE(a : PComplexArray);
begin
  u16_SSE(a,Params);
  u8_SSE(@a^[16],Params);
  u8_SSE(@a^[24],Params);
  upass_SSE(a,d32SSE,4,Params);
end;

procedure u64_SSE(a : PComplexArray);
begin
  u32_SSE(a);
  u16_SSE(@a^[32],Params);
  u16_SSE(@a^[48],Params);
  upass_SSE(a,d64SSE,8,Params);
end;

procedure u128_SSE(a : PComplexArray);
begin
  u64_SSE(a);
  u32_SSE(@a^[64]);
  u32_SSE(@a^[96]);
  upass_SSE(a,d128SSE,16,Params);
end;

procedure u256_SSE(a : PComplexArray);
begin
  u128_SSE(a);
  u64_SSE(@a^[128]);
  u64_SSE(@a^[192]);
  upass_SSE(a,d256SSE,32,Params);
end;

procedure u512_SSE(a : PComplexArray);
begin
  u256_SSE(a);
  u128_SSE(@a^[256]);
  u128_SSE(@a^[384]);
  upass_SSE(a,d512SSE,64,Params);
end;

procedure u1024_SSE(a : PComplexArray);
begin
  u512_SSE(a);
  u256_SSE(@a^[512]);
  u256_SSE(@a^[768]);
  upassbig_SSE(a,d1024SSE,128,Params);
end;

procedure u2048_SSE(a : PComplexArray);
begin
  u1024_SSE(a);
  u512_SSE(@a^[1024]);
  u512_SSE(@a^[1536]);
  upassbig_SSE(a,d2048SSE,256,Params);
end;

procedure u4096_SSE(a : PComplexArray);
begin
  u2048_SSE(a);
  u1024_SSE(@a^[2048]);
  u1024_SSE(@a^[3072]);
  upassbig_SSE(a,d4096SSE,512,Params);
end;

procedure u8192_SSE(a : PComplexArray);
begin
  u4096_SSE(a);
  u2048_SSE(@a^[4096]);
  u2048_SSE(@a^[6144]);
  upassbig_SSE(a,d8192SSE,1024,Params);
end;

procedure fft2__SSE(a : PComplexArray);
begin
  c2_SSE(a,Params);
end;

procedure ifft2__SSE(a : PComplexArray);
begin
  u2_SSE(a,Params);
end;

procedure fft4__SSE(a : PComplexArray);
begin
  c4_SSE(a,Params);
end;

procedure ifft4__SSE(a : PComplexArray);
begin
  u4_SSE(a,Params);
end;

procedure fft8__SSE(a : PComplexArray);
begin
  c8_SSE(a,Params);
end;

procedure ifft8__SSE(a : PComplexArray);
begin
  u8_SSE(a,Params);
end;

procedure fft16__SSE(a : PComplexArray);
begin
  c16_SSE(a,Params);
end;

procedure ifft16__SSE(a : PComplexArray);
begin
  u16_SSE(a,Params);
end;

procedure fft32__SSE(a : PComplexArray);
begin
  c32_SSE(a);
end;

procedure ifft32__SSE(a : PComplexArray);
begin
  u32_SSE(a);
end;

procedure fft64__SSE(a : PComplexArray);
begin
  c64_SSE(a);
end;

procedure ifft64__SSE(a : PComplexArray);
begin
  u64_SSE(a);
end;

procedure fft128__SSE(a : PComplexArray);
begin
  c128_SSE(a);
end;

procedure ifft128__SSE(a : PComplexArray);
begin
  u128_SSE(a);
end;

procedure fft256__SSE(a : PComplexArray);
begin
  c256_SSE(a);
end;

procedure ifft256__SSE(a : PComplexArray);
begin
  u256_SSE(a);
end;

procedure fft512__SSE(a : PComplexArray);
begin
  c512_SSE(a);
end;

procedure ifft512__SSE(a : PComplexArray);
begin
  u512_SSE(a);
end;

procedure fft1024__SSE(a : PComplexArray);
begin
  c1024_SSE(a);
end;

procedure ifft1024__SSE(a : PComplexArray);
begin
  u1024_SSE(a);
end;

procedure fft2048__SSE(a : PComplexArray);
begin
  c2048_SSE(a);
end;

procedure ifft2048__SSE(a : PComplexArray);
begin
  u2048_SSE(a);
end;

procedure fft4096__SSE(a : PComplexArray);
begin
  c4096_SSE(a);
end;

procedure ifft4096__SSE(a : PComplexArray);
begin
  u4096_SSE(a);
end;

procedure fft8192__SSE(a : PComplexArray);
begin
  c8192_SSE(a);
end;

procedure ifft8192__SSE(a : PComplexArray);
begin
  u8192_SSE(a);
end;

procedure c2_FPU(a : PComplexArray);
asm
        fld       DWORD PTR [eax]
        fld       st(0)
        fld       DWORD PTR [eax+8]
        fadd      st(1), st
        fsubp     st(2), st
        fld       DWORD PTR [eax+12]
        fld       DWORD PTR [eax+4]
        fld       st(0)
        fadd      st, st(2)
        fxch      st(2)
        fsubp     st(1), st
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fstp      DWORD PTR [eax+4]
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fstp      DWORD PTR [eax+12]                           
end;

procedure c4_FPU(a : PComplexArray);
asm
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+16]
        fld       st(0)
        fld       DWORD PTR [eax+8]
        fadd      DWORD PTR [eax+24]
        fadd      st(1), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+20]
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+8]
        fsub      DWORD PTR [eax+24]
        fxch      st(3)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+20]
        fld       st(0)
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fsubp     st(1), st
        fld       DWORD PTR [eax+12]
        fsub      DWORD PTR [eax+28]
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       st(2)
        fadd      st, st(4)
        fstp      DWORD PTR [eax+20]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+16]
        fxch      st(3)
        fsubp     st(2), st
        faddp     st(2), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+24]
end;

procedure c8_FPU(a : PComplexArray);
asm
        sub       esp, 16
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+32]
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+32]
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+36]
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+36]
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+16]
        fsub      DWORD PTR [eax+48]
        fld       DWORD PTR [eax+16]
        fadd      DWORD PTR [eax+48]
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax+20]
        fld       DWORD PTR [eax+52]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+20]
        fld       st(3)
        fsub      st, st(1)
        fst       DWORD PTR [esp]
        fstp      DWORD PTR [eax+32]
        fld       st(2)
        fadd      st, st(2)
        fst       DWORD PTR [esp+4]
        fstp      DWORD PTR [eax+36]
        faddp     st(3), st
        fxch      st(2)
        fst       DWORD PTR [esp+8]
        fstp      DWORD PTR [eax+48]
        fsubrp    st(1), st
        fst       DWORD PTR [esp+12]
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [eax+40]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fld       DWORD PTR [eax+44]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+24]
        fld       st(0)
        fld       DWORD PTR [eax+56]
        fsub      st(1), st
        fld       DWORD PTR [eax+28]
        fld       st(0)
        fld       DWORD PTR [eax+60]
        fsub      st(1), st
        fxch      st(3)
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+28]
        fld       st(3)
        fsub      st, st(2)
        fxch      st(2)
        faddp     st(4), st
        fld       st(2)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(3), st
        fld       st(1)
        fsub      st, st(3)
        fmul      DWORD PTR SqrtHalf
        fxch      st(3)
        faddp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)
        fmul      DWORD PTR SqrtHalf
        fxch      st(4)
        faddp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+48]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+52]
        fsub      st, st(4)
        fstp      DWORD PTR [eax+60]
        fld       DWORD PTR [esp+8]
        faddp     st(1), st
        fstp      DWORD PTR [eax+48]
        fld       DWORD PTR [esp+12]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+32]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+36]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [esp]
        faddp     st(1), st
        fstp      DWORD PTR [eax+32]
        fld       DWORD PTR [esp+4]
        faddp     st(1), st
        fstp      DWORD PTR [eax+36]
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+16]
        fld       st(0)
        fld       DWORD PTR [eax+8]
        fadd      DWORD PTR [eax+24]
        fadd      st(1), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+20]
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+8]
        fsub      DWORD PTR [eax+24]
        fxch      st(3)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+20]
        fld       st(0)
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fsubp     st(1), st
        fld       DWORD PTR [eax+12]
        fsub      DWORD PTR [eax+28]
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       st(2)
        fadd      st, st(4)
        fstp      DWORD PTR [eax+20]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+16]
        fxch      st(3)
        fsubp     st(2), st
        faddp     st(2), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+24]
        add       esp, 16
end;

procedure c16_FPU(a : PComplexArray);
asm
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+64]
        fld       DWORD PTR [eax+64]
        fadd      DWORD PTR [eax]
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+68]
        fld       DWORD PTR [eax+68]
        fadd      DWORD PTR [eax+4]
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+32]
        fsub      DWORD PTR [eax+96]
        fld       DWORD PTR [eax+96]
        fadd      DWORD PTR [eax+32]
        fstp      DWORD PTR [eax+32]
        fld       DWORD PTR [eax+36]
        fsub      DWORD PTR [eax+100]
        fld       DWORD PTR [eax+100]
        fadd      DWORD PTR [eax+36]
        fstp      DWORD PTR [eax+36]
        fld       st(3)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+64]
        fld       st(2)
        fsub      st, st(2)
        fstp      DWORD PTR [eax+100]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+96]
        faddp     st(1), st
        fstp      DWORD PTR [eax+68]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [eax+72]
        fsub      st(1), st
        fld       DWORD PTR [eax+8]
        faddp     st(1), st
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fsub      DWORD PTR [eax+76]
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [eax+76]
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+44]
        fsub      DWORD PTR [eax+108]
        fld       DWORD PTR [eax+44]
        fadd      DWORD PTR [eax+108]
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+40]
        fsub      DWORD PTR [eax+104]
        fld       DWORD PTR [eax+40]
        fadd      DWORD PTR [eax+104]
        fstp      DWORD PTR [eax+40]
        fld       st(3)
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st
        fld       st(0)
        fadd      st, st(3)
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR d16re
        fmul      st, st(4)
        fld       DWORD PTR nd16im
        fmul      st, st(2)
        faddp     st(1), st
        fstp      DWORD PTR [eax+72]
        fld       DWORD PTR d16re
        fld       DWORD PTR nd16im
        fmul      st, st(3)
        fld       st(4)
        fmul      st, st(2)
        faddp     st(1), st
        fstp      DWORD PTR [eax+108]
        fld       DWORD PTR d16im
        fmul      st(5), st
        fxch      st(1)
        fmul      st(2), st
        fxch      st(2)
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax+76]
        fmulp     st(1), st
        fxch      st(2)
        fmulp     st(1), st
        faddp     st(1), st
        fstp      DWORD PTR [eax+104]
        fld       DWORD PTR [eax+16]
        fsub      DWORD PTR [eax+80]
        fld       DWORD PTR [eax+16]
        fadd      DWORD PTR [eax+80]
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax+20]
        fsub      DWORD PTR [eax+84]
        fld       DWORD PTR [eax+20]
        fadd      DWORD PTR [eax+84]
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [eax+52]
        fsub      DWORD PTR [eax+116]
        fld       DWORD PTR [eax+52]
        fadd      DWORD PTR [eax+116]
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+48]
        fsub      DWORD PTR [eax+112]
        fld       DWORD PTR [eax+48]
        fadd      DWORD PTR [eax+112]
        fstp      DWORD PTR [eax+48]
        fld       st(0)
        fadd      st, st(3)
        fxch      st(1)
        fsubp     st(3), st
        fld       st(3)
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st
        fld       st(1)
        fadd      st, st(3)
        fmul      DWORD PTR SqrtHalf
        fstp      DWORD PTR [eax+112]
        fld       DWORD PTR SqrtHalf
        fxch      st(2)
        fsubp     st(3), st
        fxch      st(1)
        fmul      st(2), st
        fxch      st(2)
        fstp      DWORD PTR [eax+116]
        fld       st(2)
        fsub      st, st(1)
        fmulp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+80]
        faddp     st(1), st
        fmul      DWORD PTR SqrtHalf
        fstp      DWORD PTR [eax+84]
        fld       DWORD PTR [eax+24]
        fld       DWORD PTR [eax+88]
        fsub      st(1), st
        fld       DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [eax+28]
        fsub      DWORD PTR [eax+92]
        fld       DWORD PTR [eax+28]
        fadd      DWORD PTR [eax+92]
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [eax+60]
        fsub      DWORD PTR [eax+124]
        fld       DWORD PTR [eax+60]
        fadd      DWORD PTR [eax+124]
        fstp      DWORD PTR [eax+60]
        fld       DWORD PTR [eax+56]
        fsub      DWORD PTR [eax+120]
        fld       DWORD PTR [eax+56]
        fadd      DWORD PTR [eax+120]
        fstp      DWORD PTR [eax+56]
        fld       st(3)
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st
        fld       st(0)
        fadd      st, st(3)
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR d16im
        fmul      st, st(4)
        fld       DWORD PTR nd16re
        fmul      st, st(2)
        faddp     st(1), st
        fstp      DWORD PTR [eax+88]
        fld       DWORD PTR d16im
        fld       DWORD PTR nd16re
        fmul      st, st(3)
        fld       st(4)
        fmul      st, st(2)
        faddp     st(1), st
        fstp      DWORD PTR [eax+124]
        fld       DWORD PTR d16re
        fmul      st(5), st
        fxch      st(1)
        fmul      st(2), st
        fxch      st(2)
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax+92]
        fmulp     st(1), st
        fxch      st(2)
        fmulp     st(1), st
        faddp     st(1), st
        fstp      DWORD PTR [eax+120]
        sub       esp, 16
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+32]
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+32]
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+36]
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+36]
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+16]
        fsub      DWORD PTR [eax+48]
        fld       DWORD PTR [eax+16]
        fadd      DWORD PTR [eax+48]
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax+20]
        fld       DWORD PTR [eax+52]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+20]
        fld       st(3)
        fsub      st, st(1)
        fst       DWORD PTR [esp]
        fstp      DWORD PTR [eax+32]
        fld       st(2)
        fadd      st, st(2)
        fst       DWORD PTR [esp+4]
        fstp      DWORD PTR [eax+36]
        faddp     st(3), st
        fxch      st(2)
        fst       DWORD PTR [esp+8]
        fstp      DWORD PTR [eax+48]
        fsubrp    st(1), st
        fst       DWORD PTR [esp+12]
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [eax+40]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fld       DWORD PTR [eax+44]
        fld       st(1)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+24]
        fld       st(0)
        fld       DWORD PTR [eax+56]
        fsub      st(1), st
        fld       DWORD PTR [eax+28]
        fld       st(0)
        fld       DWORD PTR [eax+60]
        fsub      st(1), st
        fxch      st(3)
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+28]
        fld       st(3)
        fsub      st, st(2)
        fxch      st(2)
        faddp     st(4), st
        fld       st(2)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(3), st
        fld       st(1)
        fsub      st, st(3)
        fmul      DWORD PTR SqrtHalf
        fxch      st(3)
        faddp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)
        fmul      DWORD PTR SqrtHalf
        fxch      st(4)
        faddp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+48]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+52]
        fsub      st, st(4)
        fstp      DWORD PTR [eax+60]
        fld       DWORD PTR [esp+8]
        faddp     st(1), st
        fstp      DWORD PTR [eax+48]
        fld       DWORD PTR [esp+12]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+32]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+36]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [esp]
        faddp     st(1), st
        fstp      DWORD PTR [eax+32]
        fld       DWORD PTR [esp+4]
        faddp     st(1), st
        fstp      DWORD PTR [eax+36]
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+16]
        fld       st(0)
        fld       DWORD PTR [eax+8]
        fadd      DWORD PTR [eax+24]
        fadd      st(1), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+20]
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+8]
        fsub      DWORD PTR [eax+24]
        fxch      st(3)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+20]
        fld       st(0)
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fsubp     st(1), st
        fld       DWORD PTR [eax+12]
        fsub      DWORD PTR [eax+28]
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       st(2)
        fadd      st, st(4)
        fstp      DWORD PTR [eax+20]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+16]
        fxch      st(3)
        fsubp     st(2), st
        faddp     st(2), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+24]
        add       esp, 16
        fld       DWORD PTR [eax+64]
        fadd      DWORD PTR [eax+80]
        fld       st(0)
        fld       DWORD PTR [eax+72]
        fadd      DWORD PTR [eax+88]
        fadd      st(1), st
        fld       DWORD PTR [eax+64]
        fsub      DWORD PTR [eax+80]
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [eax+68]
        fsub      DWORD PTR [eax+84]
        fxch      st(2)
        fstp      DWORD PTR [eax+64]
        fld       DWORD PTR [eax+72]
        fsub      DWORD PTR [eax+88]
        fxch      st(3)
        fstp      DWORD PTR [eax+72]
        fld       DWORD PTR [eax+68]
        fadd      DWORD PTR [eax+84]
        fld       st(0)
        fld       DWORD PTR [eax+76]
        fadd      DWORD PTR [eax+92]
        fadd      st(1), st
        fxch      st(1)
        fstp      DWORD PTR [eax+68]
        fsubp     st(1), st
        fld       DWORD PTR [eax+76]
        fsub      DWORD PTR [eax+92]
        fxch      st(1)
        fstp      DWORD PTR [eax+76]
        fld       st(2)
        fadd      st, st(4)
        fstp      DWORD PTR [eax+84]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+80]
        fxch      st(3)
        fsubp     st(2), st
        faddp     st(2), st
        fstp      DWORD PTR [eax+92]
        fstp      DWORD PTR [eax+88]
        fld       DWORD PTR [eax+96]
        fadd      DWORD PTR [eax+112]
        fld       st(0)
        fld       DWORD PTR [eax+104]
        fadd      DWORD PTR [eax+120]
        fadd      st(1), st
        fld       DWORD PTR [eax+96]
        fsub      DWORD PTR [eax+112]
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [eax+100]
        fsub      DWORD PTR [eax+116]
        fxch      st(2)
        fstp      DWORD PTR [eax+96]
        fld       DWORD PTR [eax+104]
        fsub      DWORD PTR [eax+120]
        fxch      st(3)
        fstp      DWORD PTR [eax+104]
        fld       DWORD PTR [eax+100]
        fadd      DWORD PTR [eax+116]
        fld       st(0)
        fld       DWORD PTR [eax+108]
        fadd      DWORD PTR [eax+124]
        fadd      st(1), st
        fxch      st(1)
        fstp      DWORD PTR [eax+100]
        fsubp     st(1), st
        fld       DWORD PTR [eax+108]
        fsub      DWORD PTR [eax+124]
        fxch      st(1)
        fstp      DWORD PTR [eax+108]
        fld       st(2)
        fadd      st, st(4)
        fstp      DWORD PTR [eax+116]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+112]
        fxch      st(3)
        fsubp     st(2), st
        faddp     st(2), st
        fstp      DWORD PTR [eax+124]
        fstp      DWORD PTR [eax+120]
end;

procedure cpass_FPU(a,w : PComplexArray; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       ebx, DWORD PTR [ecx+ecx]
        mov       DWORD PTR [esp+8], eax
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, ebx
        add       ebx, eax
        lea       edi, DWORD PTR [ecx-1]
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        lea       ebp, DWORD PTR [eax+ecx]
        lea       esi, DWORD PTR [ebx+ecx]
        
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [ebx]
        fld       DWORD PTR [ebx]
        fadd      DWORD PTR [eax]
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [ebx+4]
        fld       DWORD PTR [ebx+4]
        fadd      DWORD PTR [eax+4]
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [ebp]
        fsub      DWORD PTR [esi]
        fld       DWORD PTR [esi]
        fadd      DWORD PTR [ebp]
        fstp      DWORD PTR [ebp]
        fld       DWORD PTR [ebp+4]
        fsub      DWORD PTR [esi+4]
        fld       DWORD PTR [esi+4]
        fadd      DWORD PTR [ebp+4]
        fstp      DWORD PTR [ebp+4]
        fld       st(3)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(4), st
        fstp      DWORD PTR [ebx]
        fld       st(1)
        fsub      st, st(1)
        fstp      DWORD PTR [esi+4]
        faddp     st(1), st
        fxch      st(1)
        fstp      DWORD PTR [esi]
        fstp      DWORD PTR [ebx+4]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [ebx+8]
        fsub      st(1), st
        fld       st(1)
        fld       DWORD PTR [eax+8]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fsub      DWORD PTR [ebx+12]
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [ebx+12]
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [ebp+12]
        fsub      DWORD PTR [esi+12]
        fsub      st(2), st
        fld       DWORD PTR [ebp+12]
        fadd      DWORD PTR [esi+12]
        fxch      st(4)
        faddp     st(1), st
        fxch      st(3)
        fstp      DWORD PTR [ebp+12]
        fld       DWORD PTR [ebp+8]
        fsub      DWORD PTR [esi+8]
        fld       st(0)
        fadd      st, st(2)
        fld       DWORD PTR [ebp+8]
        fadd      DWORD PTR [esi+8]
        fxch      st(2)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [ebp+8]
        fld       DWORD PTR [edx]
        fmul      st, st(3)
        fld       DWORD PTR [edx+4]
        fmul      st, st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [ebx+8]
        fld       DWORD PTR [edx+4]
        fmul      st, st(4)
        fld       DWORD PTR [edx]
        fmul      st, st(3)
        fsubrp    st(1), st
        fstp      DWORD PTR [esi+12]
        fxch      st(2)
        fmul      DWORD PTR [edx+4]
        fxch      st(2)
        fmul      DWORD PTR [edx]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [ebx+12]
        fxch      st(1)
        fmul      DWORD PTR [edx]
        fxch      st(1)
        fmul      DWORD PTR [edx+4]
        faddp     st(1), st
        fstp      DWORD PTR [esi+8]
        mov       DWORD PTR [esp+4], ecx
        jmp       @@B92
@@B93:
        mov       ebp, DWORD PTR [esp]
        mov       ebx, ecx
        mov       eax, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebp
        lea       ebp, DWORD PTR [ebp+eax]
        lea       esi, DWORD PTR [ecx+eax]
@@B92:
        mov       eax, DWORD PTR [esp+8]
        lea       ecx, DWORD PTR [eax+16]
        mov       DWORD PTR [esp], ecx
        lea       ecx, DWORD PTR [ebx+16]
        fld       DWORD PTR [eax+16]
        fld       DWORD PTR [ebx+16]
        fsub      st(1), st
        fld       st(1)
        fld       DWORD PTR [eax+16]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax+20]
        fsub      DWORD PTR [ebx+20]
        fld       DWORD PTR [eax+20]
        fadd      DWORD PTR [ebx+20]
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [ebp+20]
        fsub      DWORD PTR [esi+20]
        fsub      st(2), st
        fld       DWORD PTR [ebp+20]
        fadd      DWORD PTR [esi+20]
        fxch      st(4)
        faddp     st(1), st
        fxch      st(3)
        fstp      DWORD PTR [ebp+20]
        fld       DWORD PTR [ebp+16]
        fsub      DWORD PTR [esi+16]
        fld       DWORD PTR [ebp+16]
        fadd      DWORD PTR [esi+16]
        fstp      DWORD PTR [ebp+16]
        fld       st(0)
        fadd      st, st(2)
        fxch      st(1)
        fsubp     st(2), st
        fld       DWORD PTR [edx+8]
        fmul      st, st(3)
        fld       DWORD PTR [edx+12]
        fmul      st, st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [ebx+16]
        fld       DWORD PTR [edx+12]
        fmul      st, st(4)
        fld       DWORD PTR [edx+8]
        fmul      st, st(3)
        fsubrp    st(1), st
        fstp      DWORD PTR [esi+20]
        fxch      st(2)
        fmul      DWORD PTR [edx+12]
        fxch      st(2)
        fmul      DWORD PTR [edx+8]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [ebx+20]
        fxch      st(1)
        fmul      DWORD PTR [edx+8]
        fxch      st(1)
        fmul      DWORD PTR [edx+12]
        faddp     st(1), st
        fstp      DWORD PTR [esi+16]
        add       edx, 16
        dec       edi
        fld       DWORD PTR [eax+24]
        fld       DWORD PTR [ebx+24]
        fsub      st(1), st
        fld       DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [eax+28]
        fsub      DWORD PTR [ebx+28]
        fld       DWORD PTR [eax+28]
        fadd      DWORD PTR [ebx+28]
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [ebp+28]
        fsub      DWORD PTR [esi+28]
        fld       DWORD PTR [ebp+28]
        fadd      DWORD PTR [esi+28]
        fstp      DWORD PTR [ebp+28]
        fld       DWORD PTR [ebp+24]
        fsub      DWORD PTR [esi+24]
        fld       st(0)
        fld       DWORD PTR [ebp+24]
        fadd      DWORD PTR [esi+24]
        fxch      st(1)
        fadd      st, st(4)
        fxch      st(1)
        fstp      DWORD PTR [ebp+24]
        fld       st(4)
        fsub      st, st(3)
        fxch      st(5)
        faddp     st(3), st
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [edx]
        fmul      st, st(4)
        fld       DWORD PTR [edx+4]
        fmul      st, st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [ebx+24]
        fld       DWORD PTR [edx+4]
        fmul      st, st(2)
        fld       DWORD PTR [edx]
        fmul      st, st(4)
        fsubrp    st(1), st
        fstp      DWORD PTR [esi+28]
        fxch      st(3)
        fmul      DWORD PTR [edx+4]
        fxch      st(3)
        fmul      DWORD PTR [edx]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [ebx+28]
        fxch      st(1)
        fmul      DWORD PTR [edx]
        fxch      st(1)
        fmul      DWORD PTR [edx+4]
        faddp     st(1), st
        fstp      DWORD PTR [esi+24]
        jne       @@B93
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure cpassbig_FPU(a,w : PComplexArray; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 32
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+esi*8]
        fld       DWORD PTR [eax+esi*8]
        fadd      DWORD PTR [eax]
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+esi*8+4]
        fld       DWORD PTR [eax+esi*8+4]
        fadd      DWORD PTR [eax+4]
        fstp      DWORD PTR [eax+4]
        lea       ebp, DWORD PTR [ecx+ecx]
        lea       ebx, DWORD PTR [ecx+ecx]
        add       ebx, ebx
        sub       ebx, ecx
        add       ebx, ebx
        fld       DWORD PTR [eax+ebp*8]
        fsub      DWORD PTR [eax+ebx*8]
        fld       DWORD PTR [eax+ebx*8]
        fadd      DWORD PTR [eax+ebp*8]
        fstp      DWORD PTR [eax+ebp*8]
        fld       DWORD PTR [eax+ebp*8+4]                       
        fsub      DWORD PTR [eax+ebx*8+4]
        fld       DWORD PTR [eax+ebx*8+4]
        fadd      DWORD PTR [eax+ebp*8+4]
        fstp      DWORD PTR [eax+ebp*8+4]                       
        fld       st(3)
        fsub      st, st(1)
        fstp      DWORD PTR [eax+esi*8]
        fld       st(2)                                         
        fsub      st, st(2)
        fstp      DWORD PTR [eax+ebx*8+4]
        faddp     st(3), st
        fxch      st(2)                                         
        fstp      DWORD PTR [eax+ebx*8]
        faddp     st(1), st
        fstp      DWORD PTR [eax+esi*8+4]
        dec       ecx                                           
        mov       edi, ecx
        mov       DWORD PTR [esp+12], edx
        mov       DWORD PTR [esp+16], eax
        mov       DWORD PTR [esp+8], ecx
@@B122:
        lea       ecx, DWORD PTR [eax+8]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [eax+esi*8+8]
        fsub      st(1), st
        fld       DWORD PTR [eax+8]
        faddp     st(1), st
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]                            
        fsub      DWORD PTR [eax+esi*8+12]
        fld       DWORD PTR [eax+12]                            
        fadd      DWORD PTR [eax+esi*8+12]
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+ebp*8+12]
        fsub      DWORD PTR [eax+ebx*8+12]                      
        fld       DWORD PTR [eax+ebp*8+12]
        fadd      DWORD PTR [eax+ebx*8+12]                      
        fstp      DWORD PTR [eax+ebp*8+12]
        fld       DWORD PTR [eax+ebp*8+8]
        fsub      DWORD PTR [eax+ebx*8+8]                       
        fld       DWORD PTR [eax+ebp*8+8]
        fadd      DWORD PTR [eax+ebx*8+8]
        fstp      DWORD PTR [eax+ebp*8+8]
        fld       st(3)                                         
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st                                     
        fld       st(0)
        fadd      st, st(3)                                     
        fxch      st(1)
        fsubp     st(3), st
        fld       DWORD PTR [edx]
        fmul      st, st(4)                                     
        fld       DWORD PTR [edx+4]
        fmul      st, st(2)                                     
        fsubp     st(1), st                                     
        fstp      DWORD PTR [eax+esi*8+8]
        fld       DWORD PTR [edx+4]
        fmul      st, st(2)                                     
        fld       DWORD PTR [edx]
        fmul      st, st(4)                                     
        fsubrp    st(1), st
        fstp      DWORD PTR [eax+ebx*8+12]
        fld       DWORD PTR [edx+4]
        fmulp     st(4), st
        fld       DWORD PTR [edx]
        fmulp     st(1), st
        faddp     st(3), st                                     
        fxch      st(2)
        fstp      DWORD PTR [eax+esi*8+12]
        fld       DWORD PTR [edx]
        fmulp     st(2), st                                     
        fld       DWORD PTR [edx+4]
        fmulp     st(1), st                                     
        faddp     st(1), st
        fstp      DWORD PTR [eax+ebx*8+8]
        mov       eax, ecx
        add       edx, 8
        dec       edi
        jne       @@B122
@@B123:
        mov       DWORD PTR [esp], ecx
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+8], ecx
        mov       ecx, DWORD PTR [esp]
        lea       edi, DWORD PTR [ecx+8]
        mov       DWORD PTR [esp+4], edi
        fld       DWORD PTR [ecx+8]
        fsub      DWORD PTR [eax+esi*8+8]
        fld       DWORD PTR [ecx+8]
        fadd      DWORD PTR [eax+esi*8+8]                       
        fstp      DWORD PTR [ecx+8]
        fld       DWORD PTR [ecx+12]                            
        fsub      DWORD PTR [eax+esi*8+12]
        fld       DWORD PTR [ecx+12]
        fadd      DWORD PTR [eax+esi*8+12]
        fstp      DWORD PTR [ecx+12]                            
        fld       DWORD PTR [eax+ebp*8+12]
        fsub      DWORD PTR [eax+ebx*8+12]                      
        fld       DWORD PTR [eax+ebp*8+12]
        fadd      DWORD PTR [eax+ebx*8+12]                      
        fstp      DWORD PTR [eax+ebp*8+12]
        fld       DWORD PTR [eax+ebp*8+8]
        fsub      DWORD PTR [eax+ebx*8+8]                       
        fld       DWORD PTR [eax+ebp*8+8]
        fadd      DWORD PTR [eax+ebx*8+8]
        fstp      DWORD PTR [eax+ebp*8+8]                       
        fld       st(0)                                         
        fadd      st, st(3)
        fxch      st(1)
        fsubp     st(3), st                                     
        fld       st(3)
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st
        fld       st(1)
        fadd      st, st(3)                                     
        fmul      DWORD PTR SqrtHalf
        fstp      DWORD PTR [eax+ebx*8+8]
        fld       DWORD PTR SqrtHalf
        fxch      st(2)
        fsubp     st(3), st
        fxch      st(1)
        fmul      st(2), st
        fxch      st(2)
        fstp      DWORD PTR [eax+ebx*8+12]
        fld       st(2)
        fsub      st, st(1)                                     
        fmulp     st(2), st
        fxch      st(1)                                         
        fstp      DWORD PTR [eax+esi*8+8]
        faddp     st(1), st
        fmul      DWORD PTR SqrtHalf
        fstp      DWORD PTR [eax+esi*8+12]
        add       edx, -8
        mov       ecx, DWORD PTR [esp+8]
        mov       DWORD PTR [esp+20], ecx
        mov       DWORD PTR [esp+24], edx
        mov       DWORD PTR [esp+28], edi
        mov       eax, edi                                      
@@B124:
        lea       edi, DWORD PTR [eax+8]
        fld       DWORD PTR [eax+8]
        fld       DWORD PTR [eax+esi*8+8]                       
        fsub      st(1), st
        fld       DWORD PTR [eax+8]
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]                            
        fsub      DWORD PTR [eax+esi*8+12]
        fld       DWORD PTR [eax+12]
        fadd      DWORD PTR [eax+esi*8+12]
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+ebp*8+12]
        fsub      DWORD PTR [eax+ebx*8+12]                      
        fld       DWORD PTR [eax+ebp*8+12]
        fadd      DWORD PTR [eax+ebx*8+12]
        fstp      DWORD PTR [eax+ebp*8+12]
        fld       DWORD PTR [eax+ebp*8+8]                       
        fsub      DWORD PTR [eax+ebx*8+8]
        fld       DWORD PTR [eax+ebp*8+8]
        fadd      DWORD PTR [eax+ebx*8+8]
        fstp      DWORD PTR [eax+ebp*8+8]
        fld       st(3)
        fsub      st, st(2)
        fxch      st(4)
        faddp     st(2), st
        fld       st(0)
        fadd      st, st(3)                                     
        fxch      st(1)
        fsubp     st(3), st                                     
        fld       DWORD PTR [edx+4]
        fmul      st, st(4)
        fld       DWORD PTR [edx]
        fmul      st, st(2)                                     
        fsubp     st(1), st
        fstp      DWORD PTR [eax+esi*8+8]
        fld       DWORD PTR [edx]
        fmul      st, st(2)
        fld       DWORD PTR [edx+4]
        fmul      st, st(4)                                     
        fsubrp    st(1), st
        fstp      DWORD PTR [eax+ebx*8+12]
        fld       DWORD PTR [edx]
        fmulp     st(4), st
        fld       DWORD PTR [edx+4]
        fmulp     st(1), st
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+esi*8+12]
        fld       DWORD PTR [edx+4]
        fmulp     st(2), st
        fld       DWORD PTR [edx]
        fmulp     st(1), st
        faddp     st(1), st
        fstp      DWORD PTR [eax+ebx*8+8]
        mov       eax, edi
        add       edx, -8
        dec       ecx
        jne       @@B124
@@B125:
        add       esp, 32
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure u2_FPU(a : PComplexArray);
begin
  c2_FPU(a);
end;

procedure u4_FPU(a : PComplexArray);
asm
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+8]
        fld       st(0)
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+24]
        fadd      DWORD PTR [eax+16]
        fadd      st(2), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+8]
        fxch      st(1)
        fsubp     st(4), st
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       st(0)
        fld       DWORD PTR [eax+20]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+12]
        fxch      st(1)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       st(2)
        fld       DWORD PTR [eax+20]
        fsub      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+24]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(5), st
        fxch      st(5)
        fstp      DWORD PTR [eax+16]
        fstp      DWORD PTR [eax+8]
        fxch      st(2)
        fstp      DWORD PTR [eax+24]
        fld       st(1)
        fadd      st, st(3)
        fstp      DWORD PTR [eax+12]
        fxch      st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+20]                            
end;

procedure u8_FPU(a : PComplexArray);
asm
        sub       esp, 8
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+8]
        fld       st(0)
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+24]
        fadd      DWORD PTR [eax+16]
        fadd      st(2), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+8]
        fxch      st(1)
        fsubp     st(4), st
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       st(0)
        fld       DWORD PTR [eax+20]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+12]
        fxch      st(1)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       st(2)
        fld       DWORD PTR [eax+20]
        fsub      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+24]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(5), st
        fxch      st(5)
        fstp      DWORD PTR [eax+16]
        fstp      DWORD PTR [eax+8]
        fxch      st(2)
        fstp      DWORD PTR [eax+24]
        fld       st(1)
        fadd      st, st(3)
        fstp      DWORD PTR [eax+12]
        fxch      st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [eax+32]
        fadd      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+32]
        fsub      DWORD PTR [eax+40]
        fst       DWORD PTR [eax+40]
        fld       DWORD PTR [eax+36]
        fadd      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+36]
        fsub      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+48]
        fadd      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+48]
        fsub      DWORD PTR [eax+56]
        fst       DWORD PTR [esp]
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+52]
        fadd      DWORD PTR [eax+60]
        fld       DWORD PTR [eax+52]
        fsub      DWORD PTR [eax+60]
        fstp      DWORD PTR [esp+4]
        fld       st(3)
        fsub      st, st(1)
        fld       st(2)
        fsub      st, st(7)
        fxch      st(3)
        faddp     st(7), st
        fxch      st(1)
        faddp     st(4), st
        fld       DWORD PTR [eax+16]
        fld       st(0)
        fsub      st, st(2)
        fstp      DWORD PTR [eax+48]
        faddp     st(1), st
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax]
        fld       st(0)
        fsub      st, st(6)
        fstp      DWORD PTR [eax+32]
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fld       st(0)
        fsub      st, st(3)
        fstp      DWORD PTR [eax+36]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+20]
        fld       st(0)
        fsub      st, st(4)
        fstp      DWORD PTR [eax+52]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+20]
        fld       st(0)
        fadd      st, st(2)
        fmul      DWORD PTR SqrtHalf
        fxch      st(1)
        fsubp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [esp]
        fld       st(0)
        fld       DWORD PTR [esp+4]
        fsub      st(1), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fxch      st(2)
        faddp     st(1), st
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)
        fxch      st(2)
        faddp     st(4), st
        fld       st(2)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(3), st
        fld       DWORD PTR [eax+28]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+60]
        fld       DWORD PTR [eax+28]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [eax+24]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [eax+8]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+8]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+12]
        faddp     st(1), st
        fstp      DWORD PTR [eax+12]
        add       esp, 8
end;

procedure u16_FPU(a : PComplexArray);
asm
        sub       esp, 8
        fld       DWORD PTR [eax]
        fadd      DWORD PTR [eax+8]
        fld       st(0)
        fld       DWORD PTR [eax+4]
        fadd      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+24]
        fadd      DWORD PTR [eax+16]
        fadd      st(2), st
        fld       DWORD PTR [eax]
        fsub      DWORD PTR [eax+8]
        fxch      st(1)
        fsubp     st(4), st
        fxch      st(2)
        fstp      DWORD PTR [eax]
        fld       st(0)
        fld       DWORD PTR [eax+20]
        fadd      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+4]
        fsub      DWORD PTR [eax+12]
        fxch      st(1)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       st(2)
        fld       DWORD PTR [eax+20]
        fsub      DWORD PTR [eax+28]
        fadd      st(1), st
        fld       DWORD PTR [eax+24]
        fsub      DWORD PTR [eax+16]
        fxch      st(1)
        fsubp     st(5), st
        fxch      st(5)
        fstp      DWORD PTR [eax+16]
        fstp      DWORD PTR [eax+8]
        fxch      st(2)
        fstp      DWORD PTR [eax+24]
        fld       st(1)
        fadd      st, st(3)
        fstp      DWORD PTR [eax+12]
        fxch      st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [eax+28]
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [eax+32]
        fadd      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+32]
        fsub      DWORD PTR [eax+40]
        fst       DWORD PTR [eax+40]
        fld       DWORD PTR [eax+36]
        fadd      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+36]
        fsub      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+48]
        fadd      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+48]
        fsub      DWORD PTR [eax+56]
        fst       DWORD PTR [esp]
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+52]
        fadd      DWORD PTR [eax+60]
        fld       DWORD PTR [eax+52]
        fsub      DWORD PTR [eax+60]
        fstp      DWORD PTR [esp+4]
        fld       st(3)
        fsub      st, st(1)
        fld       st(2)
        fsub      st, st(7)
        fxch      st(3)
        faddp     st(7), st
        fxch      st(1)
        faddp     st(4), st
        fld       DWORD PTR [eax+16]
        fld       st(0)
        fsub      st, st(2)
        fstp      DWORD PTR [eax+48]
        faddp     st(1), st
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax]
        fld       st(0)
        fsub      st, st(6)
        fstp      DWORD PTR [eax+32]
        faddp     st(5), st
        fxch      st(4)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        fld       st(0)
        fsub      st, st(3)
        fstp      DWORD PTR [eax+36]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+20]
        fld       st(0)
        fsub      st, st(4)
        fstp      DWORD PTR [eax+52]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+20]
        fld       st(0)
        fadd      st, st(2)
        fmul      DWORD PTR SqrtHalf
        fxch      st(1)
        fsubp     st(2), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [esp]
        fld       st(0)
        fld       DWORD PTR [esp+4]
        fsub      st(1), st
        fxch      st(1)
        fmul      DWORD PTR SqrtHalf
        fxch      st(2)
        faddp     st(1), st
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)
        fxch      st(2)
        faddp     st(4), st
        fld       st(2)
        fsub      st, st(1)
        fxch      st(1)
        faddp     st(3), st
        fld       DWORD PTR [eax+28]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+60]
        fld       DWORD PTR [eax+28]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [eax+24]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+24]
        faddp     st(1), st
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [eax+8]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+8]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+12]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+12]
        faddp     st(1), st
        fstp      DWORD PTR [eax+12]
        add       esp, 8
        fld       DWORD PTR [eax+64]
        fadd      DWORD PTR [eax+72]
        fld       st(0)
        fld       DWORD PTR [eax+68]
        fadd      DWORD PTR [eax+76]
        fld       DWORD PTR [eax+88]
        fadd      DWORD PTR [eax+80]
        fadd      st(2), st
        fld       DWORD PTR [eax+64]
        fsub      DWORD PTR [eax+72]
        fxch      st(1)
        fsubp     st(4), st
        fxch      st(2)
        fstp      DWORD PTR [eax+64]
        fld       st(0)
        fld       DWORD PTR [eax+84]
        fadd      DWORD PTR [eax+92]
        fadd      st(1), st
        fld       DWORD PTR [eax+68]
        fsub      DWORD PTR [eax+76]
        fxch      st(1)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [eax+68]
        fld       st(2)
        fld       DWORD PTR [eax+84]
        fsub      DWORD PTR [eax+92]
        fadd      st(1), st
        fld       DWORD PTR [eax+88]
        fsub      DWORD PTR [eax+80]
        fxch      st(1)
        fsubp     st(5), st
        fxch      st(5)
        fstp      DWORD PTR [eax+80]
        fstp      DWORD PTR [eax+72]
        fxch      st(2)
        fstp      DWORD PTR [eax+88]
        fld       st(1)
        fadd      st, st(3)
        fstp      DWORD PTR [eax+76]
        fxch      st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [eax+92]
        fstp      DWORD PTR [eax+84]
        fld       DWORD PTR [eax+96]
        fadd      DWORD PTR [eax+104]
        fld       st(0)
        fld       DWORD PTR [eax+100]
        fadd      DWORD PTR [eax+108]
        fld       DWORD PTR [eax+120]
        fadd      DWORD PTR [eax+112]
        fadd      st(2), st
        fld       DWORD PTR [eax+96]
        fsub      DWORD PTR [eax+104]
        fxch      st(1)
        fsubp     st(4), st
        fxch      st(2)
        fstp      DWORD PTR [eax+96]
        fld       st(0)
        fld       DWORD PTR [eax+116]
        fadd      DWORD PTR [eax+124]
        fadd      st(1), st
        fld       DWORD PTR [eax+100]
        fsub      DWORD PTR [eax+108]
        fxch      st(1)
        fsubp     st(3), st
        fxch      st(1)
        fstp      DWORD PTR [eax+100]
        fld       st(2)
        fld       DWORD PTR [eax+116]
        fsub      DWORD PTR [eax+124]
        fadd      st(1), st
        fld       DWORD PTR [eax+120]
        fsub      DWORD PTR [eax+112]
        fxch      st(1)
        fsubp     st(5), st
        fxch      st(5)
        fstp      DWORD PTR [eax+112]
        fstp      DWORD PTR [eax+104]
        fxch      st(2)
        fstp      DWORD PTR [eax+120]
        fld       st(1)
        fadd      st, st(3)
        fstp      DWORD PTR [eax+108]
        fxch      st(2)
        fsubp     st(1), st
        fstp      DWORD PTR [eax+124]
        fstp      DWORD PTR [eax+116]
        fld       DWORD PTR [eax+64]
        fadd      DWORD PTR [eax+96]
        fld       DWORD PTR [eax+68]
        fadd      DWORD PTR [eax+100]
        fld       DWORD PTR [eax+68]
        fsub      DWORD PTR [eax+100]
        fld       DWORD PTR [eax+96]
        fsub      DWORD PTR [eax+64]
        fld       DWORD PTR [eax]
        fsub      st, st(4)
        fstp      DWORD PTR [eax+64]
        fld       DWORD PTR [eax+4]
        fsub      st, st(3)
        fstp      DWORD PTR [eax+68]
        fld       DWORD PTR [eax+32]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+96]
        fld       DWORD PTR [eax+36]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+100]
        fld       DWORD PTR [eax]
        faddp     st(4), st
        fxch      st(3)
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+32]
        faddp     st(1), st
        fstp      DWORD PTR [eax+32]
        fld       DWORD PTR [eax+36]
        faddp     st(1), st
        fstp      DWORD PTR [eax+36]
        fld       DWORD PTR [eax+80]
        fadd      DWORD PTR [eax+84]
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+84]
        fsub      DWORD PTR [eax+80]
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+112]
        fsub      DWORD PTR [eax+116]
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+116]
        fadd      DWORD PTR [eax+112]
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)
        fld       st(3)
        fsub      st, st(2)
        fxch      st(3)
        faddp     st(5), st
        fxch      st(1)
        faddp     st(3), st
        fld       DWORD PTR [eax+52]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+116]
        fld       DWORD PTR [eax+52]
        faddp     st(1), st
        fstp      DWORD PTR [eax+52]
        fld       DWORD PTR [eax+48]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+112]
        fld       DWORD PTR [eax+48]
        faddp     st(1), st
        fstp      DWORD PTR [eax+48]
        fld       DWORD PTR [eax+16]
        fsub      st, st(2)
        fstp      DWORD PTR [eax+80]
        fld       DWORD PTR [eax+16]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+16]
        fld       DWORD PTR [eax+20]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+84]
        fld       DWORD PTR [eax+20]
        faddp     st(1), st
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [eax+72]
        fmul      DWORD PTR d16re
        fld       DWORD PTR [eax+76]
        fmul      DWORD PTR d16im
        fld       DWORD PTR [eax+104]
        fmul      DWORD PTR d16re
        fld       DWORD PTR [eax+76]
        fmul      DWORD PTR d16re
        fld       DWORD PTR [eax+72]
        fmul      DWORD PTR d16im
        fld       DWORD PTR [eax+108]
        fmul      DWORD PTR d16re
        fxch      st(4)
        faddp     st(5), st
        fld       DWORD PTR [eax+108]
        fmul      DWORD PTR nd16im
        faddp     st(3), st
        fsubp     st(1), st
        fld       DWORD PTR [eax+104]
        fmul      DWORD PTR d16im
        faddp     st(3), st
        fld       st(3)
        fadd      st, st(2)
        fxch      st(4)
        fsubp     st(2), st
        fld       st(0)
        fsub      st, st(3)
        fxch      st(3)
        faddp     st(1), st
        fld       DWORD PTR [eax+12]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+76]
        fld       DWORD PTR [eax+40]
        fsub      st, st(3)
        fstp      DWORD PTR [eax+104]
        fld       DWORD PTR [eax+8]
        fsub      st, st(4)
        fstp      DWORD PTR [eax+72]
        fld       DWORD PTR [eax+40]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+40]
        fld       DWORD PTR [eax+12]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+8]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+8]
        fld       DWORD PTR [eax+44]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+108]
        fld       DWORD PTR [eax+44]
        faddp     st(1), st
        fstp      DWORD PTR [eax+44]
        fld       DWORD PTR [eax+88]
        fmul      DWORD PTR d16im
        fld       DWORD PTR [eax+92]
        fmul      DWORD PTR d16re
        fld       DWORD PTR [eax+120]
        fmul      DWORD PTR d16im
        fld       DWORD PTR [eax+92]
        fmul      DWORD PTR d16im
        fld       DWORD PTR [eax+88]
        fmul      DWORD PTR d16re
        fld       DWORD PTR [eax+124]
        fmul      DWORD PTR d16im
        fxch      st(4)
        faddp     st(5), st
        fld       DWORD PTR [eax+124]
        fmul      DWORD PTR nd16re
        faddp     st(3), st
        fsubp     st(1), st
        fld       DWORD PTR [eax+120]
        fmul      DWORD PTR d16re
        faddp     st(3), st
        fld       st(3)
        fadd      st, st(2)
        fxch      st(4)
        fsubp     st(2), st
        fld       st(0)
        fsub      st, st(3)
        fxch      st(3)
        faddp     st(1), st
        fld       DWORD PTR [eax+28]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+92]
        fld       DWORD PTR [eax+56]
        fsub      st, st(3)
        fstp      DWORD PTR [eax+120]
        fld       DWORD PTR [eax+24]
        fsub      st, st(4)
        fstp      DWORD PTR [eax+88]
        fld       DWORD PTR [eax+56]
        faddp     st(3), st
        fxch      st(2)
        fstp      DWORD PTR [eax+56]
        fld       DWORD PTR [eax+28]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [eax+24]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [eax+60]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+124]
        fld       DWORD PTR [eax+60]
        faddp     st(1), st
        fstp      DWORD PTR [eax+60]
end;

procedure upass_FPU(a,w : PComplexArray; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 12
        lea       edi, DWORD PTR [ecx+ecx]
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, edi
        add       edi, eax
        lea       esi, DWORD PTR [ecx-1]
        mov       DWORD PTR [esp+8], edi
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx
        add       ecx, ecx                                      
        lea       ebx, DWORD PTR [edi+ecx]
        fld       DWORD PTR [edi]
        fadd      DWORD PTR [ebx]                               
        lea       ebp, DWORD PTR [eax+ecx]
        fld       DWORD PTR [edi+4]                             
        fadd      DWORD PTR [ebx+4]                             
        fld       DWORD PTR [edi+4]                             
        fsub      DWORD PTR [ebx+4]
        fld       DWORD PTR [ebx]                               
        fsub      DWORD PTR [edi]
        fld       DWORD PTR [eax]                               
        fsub      st, st(4)
        fstp      DWORD PTR [edi]
        fld       DWORD PTR [eax+4]                             
        fsub      st, st(3)                                     
        fstp      DWORD PTR [edi+4]
        fld       DWORD PTR [ebp]
        fsub      st, st(2)                                     
        fstp      DWORD PTR [ebx]
        fld       DWORD PTR [ebp+4]
        fsub      st, st(1)
        fstp      DWORD PTR [ebx+4]                             
        fld       DWORD PTR [eax]
        faddp     st(4), st                                     
        fxch      st(3)                                         
        fstp      DWORD PTR [eax]
        fld       DWORD PTR [eax+4]                             
        faddp     st(2), st
        fxch      st(1)                                         
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [ebp]                               
        faddp     st(1), st                                     
        fstp      DWORD PTR [ebp]
        fld       DWORD PTR [ebp+4]
        faddp     st(1), st
        fstp      DWORD PTR [ebp+4]
        fld       DWORD PTR [edi+8]                             
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [edi+12]
        fmul      DWORD PTR [edx+4]
        faddp     st(1), st
        fld       DWORD PTR [ebx+8]                             
        fmul      DWORD PTR [edx]                               
        fld       DWORD PTR [edi+12]
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [edi+8]                             
        fmul      DWORD PTR [edx+4]
        fsubp     st(1), st                                     
        fld       DWORD PTR [ebx+12]                            
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [edx+4]
        fld       DWORD PTR [ebx+12]                            
        fmul      st, st(1)                                     
        fsubp     st(4), st
        fld       st(3)                                         
        fld       DWORD PTR [ebx+8]                             
        fmulp     st(2), st                                     
        fxch      st(1)
        faddp     st(2), st
        fadd      st, st(4)                                     
        fxch      st(4)
        fsubp     st(3), st
        fld       st(1)                                         
        fsub      st, st(1)                                     
        fxch      st(2)
        faddp     st(1), st
        fld       DWORD PTR [eax+12]                            
        fsub      st, st(1)                                     
        fstp      DWORD PTR [edi+12]
        fld       DWORD PTR [ebp+8]                             
        fsub      st, st(2)
        fstp      DWORD PTR [ebx+8]
        fld       DWORD PTR [eax+8]
        fsub      st, st(4)
        fstp      DWORD PTR [edi+8]
        fld       DWORD PTR [ebp+8]                             
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [ebp+8]                             
        fld       DWORD PTR [eax+12]                            
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+12]
        fld       DWORD PTR [eax+8]                             
        faddp     st(2), st                                     
        fxch      st(1)
        fstp      DWORD PTR [eax+8]                             
        fld       DWORD PTR [ebp+12]                            
        fsub      st, st(1)
        fstp      DWORD PTR [ebx+12]
        fld       DWORD PTR [ebp+12]
        faddp     st(1), st                                     
        fstp      DWORD PTR [ebp+12]
        mov       DWORD PTR [esp+4], ecx                        
        jmp       @@B42
@@B43:
        mov       eax, edi
        mov       ebx, DWORD PTR [esp]
        mov       ecx, DWORD PTR [esp+4]
        mov       DWORD PTR [esp+8], ebx                        
        lea       ebx, DWORD PTR [ebx+ecx]                      
        lea       ebp, DWORD PTR [edi+ecx]                      
@@B42:
        mov       ecx, DWORD PTR [esp+8]
        lea       edi, DWORD PTR [ecx+16]                       
        mov       DWORD PTR [esp], edi                          
        lea       edi, DWORD PTR [eax+16]
        fld       DWORD PTR [ecx+16]                            
        fmul      DWORD PTR [edx+8]                             
        fld       DWORD PTR [ecx+20]                            
        fmul      DWORD PTR [edx+12]                            
        faddp     st(1), st
        fld       DWORD PTR [ebx+16]                            
        fmul      DWORD PTR [edx+8]
        fld       DWORD PTR [ecx+20]
        fmul      DWORD PTR [edx+8]
        fld       DWORD PTR [ecx+16]
        fmul      DWORD PTR [edx+12]
        fsubp     st(1), st                                     
        fld       DWORD PTR [ebx+20]
        fmul      DWORD PTR [edx+8]                             
        fld       DWORD PTR [edx+12]
        add       edx, 16
        dec       esi
        fld       DWORD PTR [ebx+20]                            
        fmul      st, st(1)
        fsubp     st(4), st                                     
        fld       DWORD PTR [ebx+16]                            
        fmulp     st(1), st                                     
        faddp     st(1), st                                     
        fld       st(2)
        fadd      st, st(4)                                     
        fxch      st(4)                                         
        fsubp     st(3), st                                     
        fld       st(1)
        fsub      st, st(1)
        fxch      st(2)                                         
        faddp     st(1), st
        fld       DWORD PTR [eax+20]
        fsub      st, st(1)                                     
        fstp      DWORD PTR [ecx+20]
        fld       DWORD PTR [ebp+16]                            
        fsub      st, st(2)                                     
        fstp      DWORD PTR [ebx+16]
        fld       DWORD PTR [eax+16]
        fsub      st, st(4)                                     
        fstp      DWORD PTR [ecx+16]
        fld       DWORD PTR [ebp+16]
        faddp     st(2), st                                     
        fxch      st(1)
        fstp      DWORD PTR [ebp+16]
        fld       DWORD PTR [eax+20]                            
        faddp     st(1), st
        fstp      DWORD PTR [eax+20]
        fld       DWORD PTR [eax+16]
        faddp     st(2), st                                     
        fxch      st(1)                                         
        fstp      DWORD PTR [eax+16]                            
        fld       DWORD PTR [ebp+20]                            
        fsub      st, st(1)
        fstp      DWORD PTR [ebx+20]                            
        fld       DWORD PTR [ebp+20]
        faddp     st(1), st                                     
        fstp      DWORD PTR [ebp+20]
        fld       DWORD PTR [ecx+24]                            
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [ecx+28]
        fmul      DWORD PTR [edx+4]
        faddp     st(1), st
        fld       DWORD PTR [ebx+24]                            
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [ecx+28]                            
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [ecx+24]
        fmul      DWORD PTR [edx+4]                             
        fsubp     st(1), st
        fld       DWORD PTR [ebx+28]                            
        fmul      DWORD PTR [edx]                               
        fld       DWORD PTR [edx+4]
        fld       DWORD PTR [ebx+28]                            
        fmul      st, st(1)
        fsubp     st(4), st
        fld       DWORD PTR [ebx+24]
        fmulp     st(1), st
        faddp     st(1), st                                     
        fld       st(2)
        fadd      st, st(4)                                     
        fxch      st(4)
        fsubp     st(3), st                                     
        fld       st(1)                                         
        fsub      st, st(1)                                     
        fxch      st(2)
        faddp     st(1), st
        fld       DWORD PTR [eax+28]                            
        fsub      st, st(1)
        fstp      DWORD PTR [ecx+28]                            
        fld       DWORD PTR [ebp+24]
        fsub      st, st(2)
        fstp      DWORD PTR [ebx+24]                            
        fld       DWORD PTR [eax+24]
        fsub      st, st(4)
        fstp      DWORD PTR [ecx+24]
        fld       DWORD PTR [ebp+24]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [ebp+24]
        fld       DWORD PTR [eax+28]
        faddp     st(1), st
        fstp      DWORD PTR [eax+28]
        fld       DWORD PTR [eax+24]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+24]
        fld       DWORD PTR [ebp+28]
        fsub      st, st(1)
        fstp      DWORD PTR [ebx+28]
        fld       DWORD PTR [ebp+28]
        faddp     st(1), st
        fstp      DWORD PTR [ebp+28]
        jne       @@B43
        add       esp, 12
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure upassbig_FPU(a,w : PComplexArray; n : integer);
asm
        push      edi
        push      esi
        push      ebp
        push      ebx
        sub       esp, 24
        lea       esi, DWORD PTR [ecx+ecx]
        add       esi, esi
        fld       DWORD PTR [eax+esi*8]
        lea       ebp, DWORD PTR [ecx+ecx]
        add       ebp, ebp
        sub       ebp, ecx
        add       ebp, ebp
        fadd      DWORD PTR [eax+ebp*8]
        fld       DWORD PTR [eax+esi*8+4]
        fadd      DWORD PTR [eax+ebp*8+4]
        fld       DWORD PTR [eax+esi*8+4]
        fsub      DWORD PTR [eax+ebp*8+4]
        fld       DWORD PTR [eax+ebp*8]
        fsub      DWORD PTR [eax+esi*8]                         
        fld       DWORD PTR [eax]                               
        fsub      st, st(4)
        fstp      DWORD PTR [eax+esi*8]
        fld       DWORD PTR [eax+4]                             
        fsub      st, st(3)                                     
        fstp      DWORD PTR [eax+esi*8+4]                       
        lea       ebx, DWORD PTR [ecx+ecx]                      
        fld       DWORD PTR [eax+ebx*8]                         
        fsub      st, st(2)                                     
        fstp      DWORD PTR [eax+ebp*8]
        fld       DWORD PTR [eax+ebx*8+4]                       
        fsub      st, st(1)                                     
        fstp      DWORD PTR [eax+ebp*8+4]                       
        fld       DWORD PTR [eax]
        faddp     st(4), st                                     
        fxch      st(3)
        fstp      DWORD PTR [eax]                               
        fld       DWORD PTR [eax+4]
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [eax+4]
        fld       DWORD PTR [eax+ebx*8]                         
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+ebx*8]                         
        fld       DWORD PTR [eax+ebx*8+4]                       
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+ebx*8+4]
        dec       ecx                                           
        mov       edi, ecx                                      
        mov       DWORD PTR [esp+4], eax                        
        mov       DWORD PTR [esp+8], edx                        
        mov       DWORD PTR [esp], ecx                          
        mov       ecx, eax                                      
@@B12:
        fld       DWORD PTR [ecx+esi*8+8]
        fmul      DWORD PTR [edx]                               
        fld       DWORD PTR [ecx+esi*8+12]
        fmul      DWORD PTR [edx+4]                             
        fld       DWORD PTR [ecx+ebp*8+8]                       
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [ecx+esi*8+12]                      
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [ecx+esi*8+8]
        fmul      DWORD PTR [edx+4]                             
        fld       DWORD PTR [ecx+ebp*8+12]                      
        fmul      DWORD PTR [edx]                               
        fxch      st(4)                                         
        faddp     st(5), st                                     
        fld       DWORD PTR [edx+4]                             
        fld       DWORD PTR [ecx+ebp*8+12]                      
        fmul      st, st(1)
        fsubp     st(4), st                                     
        fxch      st(1)                                         
        fsubp     st(2), st
        fld       DWORD PTR [ecx+ebp*8+8]                       
        fmulp     st(1), st
        faddp     st(3), st                                     
        fld       st(1)
        fadd      st, st(4)
        fxch      st(4)                                         
        fsubp     st(2), st                                     
        fld       st(0)                                         
        fsub      st, st(3)                                     
        fxch      st(1)
        faddp     st(3), st                                     
        lea       eax, DWORD PTR [ecx+8]                        
        fld       DWORD PTR [ecx+12]
        fsub      st, st(3)                                     
        fstp      DWORD PTR [ecx+esi*8+12]                      
        fld       DWORD PTR [ecx+ebx*8+8]
        fsub      st, st(1)                                     
        fstp      DWORD PTR [ecx+ebp*8+8]                       
        fld       DWORD PTR [ecx+8]
        fsub      st, st(4)                                     
        fstp      DWORD PTR [ecx+esi*8+8]
        fld       DWORD PTR [ecx+ebx*8+8]                       
        faddp     st(1), st                                     
        fstp      DWORD PTR [ecx+ebx*8+8]
        fld       DWORD PTR [ecx+12]                            
        faddp     st(2), st                                     
        fxch      st(1)                                         
        fstp      DWORD PTR [ecx+12]
        fld       DWORD PTR [ecx+8]                             
        faddp     st(2), st                                     
        fxch      st(1)                                         
        fstp      DWORD PTR [ecx+8]
        fld       DWORD PTR [ecx+ebx*8+12]                      
        fsub      st, st(1)                                     
        fstp      DWORD PTR [ecx+ebp*8+12]                      
        fld       DWORD PTR [ecx+ebx*8+12]                      
        faddp     st(1), st
        fstp      DWORD PTR [ecx+ebx*8+12]
        add       edx, 8                                        
        mov       ecx, eax
        dec       edi                                           
        jne       @@B12
@@B13:
        mov       ecx, DWORD PTR [esp]
        fld       DWORD PTR [eax+esi*8+8]
        fadd      DWORD PTR [eax+esi*8+12]                      
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+esi*8+12]
        fsub      DWORD PTR [eax+esi*8+8]
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+ebp*8+8]                       
        fsub      DWORD PTR [eax+ebp*8+12]
        fmul      DWORD PTR SqrtHalf
        fld       DWORD PTR [eax+ebp*8+12]
        fadd      DWORD PTR [eax+ebp*8+8]                       
        fmul      DWORD PTR SqrtHalf
        fld       st(1)
        fsub      st, st(4)                                     
        fld       st(3)
        fsub      st, st(2)                                     
        fxch      st(3)
        faddp     st(5), st
        fxch      st(1)                                         
        faddp     st(3), st                                     
        fld       DWORD PTR [eax+ebx*8+12]
        fsub      st, st(1)                                     
        fstp      DWORD PTR [eax+ebp*8+12]
        fld       DWORD PTR [eax+ebx*8+12]                      
        faddp     st(1), st
        fstp      DWORD PTR [eax+ebx*8+12]                      
        fld       DWORD PTR [eax+ebx*8+8]                       
        fsub      st, st(1)
        fstp      DWORD PTR [eax+ebp*8+8]                       
        fld       DWORD PTR [eax+ebx*8+8]                       
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+ebx*8+8]
        lea       edi, DWORD PTR [eax+8]                        
        fld       DWORD PTR [eax+8]
        fsub      st, st(2)                                     
        fstp      DWORD PTR [eax+esi*8+8]                       
        fld       DWORD PTR [eax+8]
        faddp     st(2), st
        fxch      st(1)                                         
        fstp      DWORD PTR [eax+8]                             
        fld       DWORD PTR [eax+12]
        fsub      st, st(1)
        fstp      DWORD PTR [eax+esi*8+12]                      
        fld       DWORD PTR [eax+12]                            
        faddp     st(1), st                                     
        fstp      DWORD PTR [eax+12]                            
        add       edx, -8                                       
        mov       DWORD PTR [esp+12], ecx                       
        mov       DWORD PTR [esp+16], edi
        mov       DWORD PTR [esp+20], edx                       
@@B14:
        fld       DWORD PTR [edi+esi*8+8]                       
        fmul      DWORD PTR [edx+4]
        fld       DWORD PTR [edi+esi*8+12]
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [edi+ebp*8+8]
        fmul      DWORD PTR [edx+4]                             
        fld       DWORD PTR [edi+esi*8+12]                      
        fmul      DWORD PTR [edx+4]
        fld       DWORD PTR [edi+esi*8+8]                       
        fmul      DWORD PTR [edx]
        fld       DWORD PTR [edi+ebp*8+12]                      
        fmul      DWORD PTR [edx+4]                             
        fxch      st(4)                                         
        faddp     st(5), st
        fld       DWORD PTR [edx]                               
        fld       DWORD PTR [edi+ebp*8+12]                      
        fmul      st, st(1)                                     
        fsubp     st(4), st                                     
        fxch      st(1)
        fsubp     st(2), st                                     
        fld       DWORD PTR [edi+ebp*8+8]
        fmulp     st(1), st
        faddp     st(3), st                                     
        fld       st(1)
        fadd      st, st(4)
        fxch      st(4)                                         
        fsubp     st(2), st                                     
        fld       st(0)                                         
        fsub      st, st(3)
        fxch      st(1)                                         
        faddp     st(3), st                                     
        lea       eax, DWORD PTR [edi+8]                        
        fld       DWORD PTR [edi+12]
        fsub      st, st(3)                                     
        fstp      DWORD PTR [edi+esi*8+12]                      
        fld       DWORD PTR [edi+ebx*8+8]                       
        fsub      st, st(1)                                     
        fstp      DWORD PTR [edi+ebp*8+8]
        fld       DWORD PTR [edi+8]
        fsub      st, st(4)
        fstp      DWORD PTR [edi+esi*8+8]                       
        fld       DWORD PTR [edi+ebx*8+8]                       
        faddp     st(1), st
        fstp      DWORD PTR [edi+ebx*8+8]                       
        fld       DWORD PTR [edi+12]                            
        faddp     st(2), st
        fxch      st(1)
        fstp      DWORD PTR [edi+12]
        fld       DWORD PTR [edi+8]                             
        faddp     st(2), st                                     
        fxch      st(1)                                         
        fstp      DWORD PTR [edi+8]                             
        fld       DWORD PTR [edi+ebx*8+12]
        fsub      st, st(1)                                     
        fstp      DWORD PTR [edi+ebp*8+12]
        fld       DWORD PTR [edi+ebx*8+12]                      
        faddp     st(1), st
        fstp      DWORD PTR [edi+ebx*8+12]                      
        add       edx, -8
        mov       edi, eax                                      
        dec       ecx
        jne       @@B14
@@B15:
        add       esp, 24
        pop       ebx
        pop       ebp
        pop       esi
        pop       edi
end;

procedure c32_FPU(a : PComplexArray);
begin
  cpass_FPU(a,@d32,4);
  c8_FPU(@a^[16]);
  c8_FPU(@a^[24]);
  c16_FPU(a);
end;

procedure c64_FPU(a : PComplexArray);
begin
  cpass_FPU(a,@d64,8);
  c16_FPU(@a^[32]);
  c16_FPU(@a^[48]);
  c32_FPU(a);
end;

procedure c128_FPU(a : PComplexArray);
begin
  cpass_FPU(a,@d128,16);
  c32_FPU(@a^[64]);
  c32_FPU(@a^[96]);
  c64_FPU(a);
end;

procedure c256_FPU(a : PComplexArray);
begin
  cpass_FPU(a,@d256,32);
  c64_FPU(@a^[128]);
  c64_FPU(@a^[192]);
  c128_FPU(a);
end;

procedure c512_FPU(a : PComplexArray);
begin
  cpass_FPU(a,@d512,64);
  c128_FPU(@a^[384]);
  c128_FPU(@a^[256]);
  c256_FPU(a);
end;
procedure c1024_FPU(a : PComplexArray);
begin
  cpassbig_FPU(a,@d1024,128);
  c256_FPU(@a^[768]);
  c256_FPU(@a^[512]);
  c512_FPU(a);
end;

procedure c2048_FPU(a : PComplexArray);
begin
  cpassbig_FPU(a,@d2048,256);
  c512_FPU(@a^[1536]);
  c512_FPU(@a^[1024]);
  c1024_FPU(a);
end;

procedure c4096_FPU(a : PComplexArray);
begin
  cpassbig_FPU(a,@d4096,512);
  c1024_FPU(@a^[3072]);
  c1024_FPU(@a^[2048]);
  c2048_FPU(a);
end;

procedure c8192_FPU(a : PComplexArray);
begin
  cpassbig_FPU(a,@d8192,1024);
  c2048_FPU(@a^[6144]);
  c2048_FPU(@a^[4096]);
  c4096_FPU(a);
end;

procedure u32_FPU(a : PComplexArray);
begin
  u16_FPU(a);
  u8_FPU(@a^[16]);
  u8_FPU(@a^[24]);
  upass_FPU(a,@d32,4);
end;

procedure u64_FPU(a : PComplexArray);
begin
  u32_FPU(a);
  u16_FPU(@a^[32]);
  u16_FPU(@a^[48]);
  upass_FPU(a,@d64,8);
end;

procedure u128_FPU(a : PComplexArray);
begin
  u64_FPU(a);
  u32_FPU(@a^[64]);
  u32_FPU(@a^[96]);
  upass_FPU(a,@d128,16);
end;

procedure u256_FPU(a : PComplexArray);
begin
  u128_FPU(a);
  u64_FPU(@a^[128]);
  u64_FPU(@a^[192]);
  upass_FPU(a,@d256,32);
end;

procedure u512_FPU(a : PComplexArray);
begin
  u256_FPU(a);
  u128_FPU(@a^[256]);
  u128_FPU(@a^[384]);
  upass_FPU(a,@d512,64);
end;

procedure u1024_FPU(a : PComplexArray);
begin
  u512_FPU(a);
  u256_FPU(@a^[512]);
  u256_FPU(@a^[768]);
  upassbig_FPU(a,@d1024,128);
end;

procedure u2048_FPU(a : PComplexArray);
begin
  u1024_FPU(a);
  u512_FPU(@a^[1024]);
  u512_FPU(@a^[1536]);
  upassbig_FPU(a,@d2048,256);
end;

procedure u4096_FPU(a : PComplexArray);
begin
  u2048_FPU(a);
  u1024_FPU(@a^[2048]);
  u1024_FPU(@a^[3072]);
  upassbig_FPU(a,@d4096,512);
end;

procedure u8192_FPU(a : PComplexArray);
begin
  u4096_FPU(a);
  u2048_FPU(@a^[4096]);
  u2048_FPU(@a^[6144]);
  upassbig_FPU(a,@d8192,1024);
end;

procedure fft2__FPU(a : PComplexArray);
begin
  c2_FPU(a);
end;

procedure ifft2__FPU(a : PComplexArray);
begin
  u2_FPU(a);
end;

procedure fft4__FPU(a : PComplexArray);
begin
  c4_FPU(a);
end;

procedure ifft4__FPU(a : PComplexArray);
begin
  u4_FPU(a);
end;

procedure fft8__FPU(a : PComplexArray);
begin
  c8_FPU(a);
end;

procedure ifft8__FPU(a : PComplexArray);
begin
  u8_FPU(a);
end;

procedure fft16__FPU(a : PComplexArray);
begin
  c16_FPU(a);
end;

procedure ifft16__FPU(a : PComplexArray);
begin
  u16_FPU(a);
end;

procedure fft32__FPU(a : PComplexArray);
begin
  c32_FPU(a);
end;

procedure ifft32__FPU(a : PComplexArray);
begin
  u32_FPU(a);
end;

procedure fft64__FPU(a : PComplexArray);
begin
  c64_FPU(a);
end;

procedure ifft64__FPU(a : PComplexArray);
begin
  u64_FPU(a);
end;

procedure fft128__FPU(a : PComplexArray);
begin
  c128_FPU(a);
end;

procedure ifft128__FPU(a : PComplexArray);
begin
  u128_FPU(a);
end;

procedure fft256__FPU(a : PComplexArray);
begin
  c256_FPU(a);
end;

procedure ifft256__FPU(a : PComplexArray);
begin
  u256_FPU(a);
end;

procedure fft512__FPU(a : PComplexArray);
begin
  c512_FPU(a);
end;

procedure ifft512__FPU(a : PComplexArray);
begin
  u512_FPU(a);
end;

procedure fft1024__FPU(a : PComplexArray);
begin
  c1024_FPU(a);
end;

procedure ifft1024__FPU(a : PComplexArray);
begin
  u1024_FPU(a);
end;

procedure fft2048__FPU(a : PComplexArray);
begin
  c2048_FPU(a);
end;

procedure ifft2048__FPU(a : PComplexArray);
begin
  u2048_FPU(a);
end;

procedure fft4096__FPU(a : PComplexArray);
begin
  c4096_FPU(a);
end;

procedure ifft4096__FPU(a : PComplexArray);
begin
  u4096_FPU(a);
end;

procedure fft8192__FPU(a : PComplexArray);
begin
  c8192_FPU(a);
end;

procedure ifft8192__FPU(a : PComplexArray);
begin
  u8192_FPU(a);
end;

procedure Init3DNowCosineTable;

  procedure CopyFromTo(pFrom: PComplex; pTo: PComplex3DNow);
  begin
    pTo^.re1 := pFrom^.re;
    pTo^.re2 := pFrom^.re;
    pTo^.im1 := pFrom^.im;
    pTo^.im2 := pFrom^.im;
  end;

var
  i : integer;
begin
  for i := 0 to 6 do CopyFromTo(@d32[i],@d323DNow[i]);
  for i := 0 to 14 do CopyFromTo(@d64[i],@d643DNow[i]);
  for i := 0 to 30 do CopyFromTo(@d128[i],@d1283DNow[i]);
  for i := 0 to 62 do CopyFromTo(@d256[i],@d2563DNow[i]);
  for i := 0 to 126 do CopyFromTo(@d512[i],@d5123DNow[i]);
  for i := 0 to 126 do CopyFromTo(@d1024[i],@d10243DNow[i]);
  for i := 0 to 254 do CopyFromTo(@d2048[i],@d20483DNow[i]);
  for i := 0 to 510 do CopyFromTo(@d4096[i],@d40963DNow[i]);
  for i := 0 to 1022 do CopyFromTo(@d8192[i],@d81923DNow[i]);
end;

procedure Init3DNowMemory;
var
  CplxSize : Integer;
begin
  CplxSize := SizeOf(TComplex3DNow);
  d323DNow := AllocMem(CplxSize * 7);
  d643DNow := AllocMem(CplxSize * 15);
  d1283DNow := AllocMem(CplxSize * 31);
  d2563DNow := AllocMem(CplxSize * 63);
  d5123DNow := AllocMem(CplxSize * 127);
  d10243DNow := AllocMem(CplxSize * 127);
  d20483DNow := AllocMem(CplxSize * 255);
  d40963DNow := AllocMem(CplxSize * 511);
  d81923DNow := AllocMem(CplxSize * 1023);
end;

procedure FreeAllocatedMemory;
begin
  if Assigned(d32SSE) then FreeMem(d32SSE);
  if Assigned(d64SSE) then FreeMem(d64SSE);
  if Assigned(d128SSE) then FreeMem(d128SSE);
  if Assigned(d256SSE) then FreeMem(d256SSE);
  if Assigned(d512SSE) then FreeMem(d512SSE);
  if Assigned(d1024SSE) then FreeMem(d1024SSE);
  if Assigned(d2048SSE) then FreeMem(d2048SSE);
  if Assigned(d4096SSE) then FreeMem(d4096SSE);
  if Assigned(d8192SSE) then FreeMem(d8192SSE);
  if Assigned(d323DNow) then FreeMem(d323DNow);
  if Assigned(d643DNow) then FreeMem(d643DNow);
  if Assigned(d1283DNow) then FreeMem(d1283DNow);
  if Assigned(d2563DNow) then FreeMem(d2563DNow);
  if Assigned(d5123DNow) then FreeMem(d5123DNow);
  if Assigned(d10243DNow) then FreeMem(d10243DNow);
  if Assigned(d20483DNow) then FreeMem(d20483DNow);
  if Assigned(d40963DNow) then FreeMem(d40963DNow);
  if Assigned(d81923DNow) then FreeMem(d81923DNow);
end;

procedure InitSSECosineTable;
(* works only on Aligned Memory
  procedure CopyFromTo(pFrom : PChar; pTo : PChar);
  asm
         movss      xmm1, DWORD PTR [pFrom]
         shufps     xmm1, xmm1, $0
         movaps     DQWORD PTR [pTo], xmm1
         movss      xmm1, DWORD PTR [pFrom+4]
         shufps     xmm1, xmm1, $0
         movaps     DQWORD PTR [pTo+16], xmm1
  end;
*)
  procedure CopyFromTo(pFrom: PComplex; pTo: PComplexSSE);
  begin
    pTo^.re1 := pFrom^.re;
    pTo^.re2 := pFrom^.re;
    pTo^.re3 := pFrom^.re;
    pTo^.re4 := pFrom^.re;

    pTo^.im1 := pFrom^.im;
    pTo^.im2 := pFrom^.im;
    pTo^.im3 := pFrom^.im;
    pTo^.im4 := pFrom^.im;
  end;

var
  i : integer;
begin
  for i := 0 to 6 do CopyFromTo(@d32[i],@d32SSE[i]);
  for i := 0 to 14 do CopyFromTo(@d64[i],@d64SSE[i]);
  for i := 0 to 30 do CopyFromTo(@d128[i],@d128SSE[i]);
  for i := 0 to 62 do CopyFromTo(@d256[i],@d256SSE[i]);
  for i := 0 to 126 do CopyFromTo(@d512[i],@d512SSE[i]);
  for i := 0 to 126 do CopyFromTo(@d1024[i],@d1024SSE[i]);
  for i := 0 to 254 do CopyFromTo(@d2048[i],@d2048SSE[i]);
  for i := 0 to 510 do CopyFromTo(@d4096[i],@d4096SSE[i]);
  for i := 0 to 1022 do CopyFromTo(@d8192[i],@d8192SSE[i]);
end;

procedure InitSSEMemory;
begin
  d32SSE := AllocMem(SizeOf(TComplexSSE) * 7);
  d64SSE := AllocMem(SizeOf(TComplexSSE) * 15);
  d128SSE := AllocMem(SizeOf(TComplexSSE) * 31);
  d256SSE := AllocMem(SizeOf(TComplexSSE) * 63);
  d512SSE := AllocMem(SizeOf(TComplexSSE) * 127);
  d1024SSE := AllocMem(SizeOf(TComplexSSE) * 127);
  d2048SSE := AllocMem(SizeOf(TComplexSSE) * 255);
  d4096SSE := AllocMem(SizeOf(TComplexSSE) * 511);
  d8192SSE := AllocMem(SizeOf(TComplexSSE) * 1023);
  Params := AllocMem(SizeOf(Single) * 24);

  Move(InvertSSE_HL,Params^,16);
  Move(SqrtHalfSSE,Params[16],16);
  Move(d16reSSE,Params[32],16);
  Move(d16imSSE,Params[48],16);
  Move(InvertSSE_MHL,Params[64],16);
  Move(InvertSSE_LL,Params[80],16);
end;

procedure scalec(a : PComplexArray; n : integer; u : Single);
asm
        push      ebx
        shl       edx, 3
        xor       ebx, ebx
        cmp       hasSSE, 0
        je        @@3DNOW
@@CSSE:
        cmp       edx, 16
        jl        @@3DNOW
        movss     xmm7, u
        shufps    xmm7, xmm7, $0
@@Loop2:
        movaps    xmm0, [eax+ebx]
        mulps     xmm0, xmm7
        movaps    [eax+ebx], xmm0
        add       ebx, 16
        cmp       ebx, edx
        jl        @@Loop2
        jmp       @@End
@@3DNOW:
        cmp       has3DNow, 0
        je        @@CFPU
        femms
        movd      mm7, u
        punpckldq mm7, mm7
@@Loop1:
        movq      mm0, [eax+ebx]
        pfmul     mm0, mm7
        movq      [eax+ebx], mm0
        add       ebx, 8
        cmp       ebx, edx
        jl        @@Loop1
        femms
        jmp       @@End
@@CFPU:
        fld       [eax+ebx]
        fmul      u
        fstp      [eax+ebx]
        add       ebx, 4
        cmp       ebx, edx
        jl        @@CFPU
@@End:
        pop       ebx
end;

procedure ReOrderFFT(a : PComplexArray; Size : integer; RevBin : PWORDArray; Inverse : Boolean; SwapBuffer : Pointer);
var
  tmp : PComplexArray;
  i : integer;
  FreeBuffer : Boolean;
begin
  if Assigned(SwapBuffer) then
  begin
    FreeBuffer := False;
    tmp := SwapBuffer;
  end else
  begin
    FreeBuffer := True;
    tmp := AllocMem(Size shl 3);
  end;
  Move(a^,tmp^,Size shl 3);
  if not Inverse then for i := 0 to Size -1 do a[i] := tmp[RevBin[i]]
                 else for i := 0 to Size -1 do a[RevBin[i]] := tmp[i];
  if FreeBuffer then FreeMem(tmp);
end;

procedure dspDoFFT(Cplx : PComplexArray; FFTSize : integer; Inverse,Scale,ReOrder : Boolean; SwapBuffer : Pointer = nil);
begin
  if not Inverse then
  begin
    case FFTSize of
      2:
      begin
        fft2(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins2,Inverse,SwapBuffer);
      end;
      4:
      begin
        fft4(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins4,Inverse,SwapBuffer);
      end;
      8:
      begin
        fft8(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins8,Inverse,SwapBuffer);
      end;
      16:
      begin
        fft16(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins16,Inverse,SwapBuffer);
      end;
      32:
      begin
        fft32(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins32,Inverse,SwapBuffer);
      end;
      64:
      begin
        fft64(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins64,Inverse,SwapBuffer);
      end;
      128:
      begin
        fft128(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins128,Inverse,SwapBuffer);
      end;
      256:
      begin
        fft256(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins256,Inverse,SwapBuffer);
      end;
      512:
      begin
        fft512(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins512,Inverse,SwapBuffer);
      end;
      1024:
      begin
        fft1024(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins1024,Inverse,SwapBuffer);
      end;
      2048:
      begin
        fft2048(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins2048,Inverse,SwapBuffer);
      end;
      4096:
      begin
        fft4096(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins4096,Inverse,SwapBuffer);
      end;
      8192:
      begin
        fft8192(Cplx);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins8192,Inverse,SwapBuffer);
      end;
    end;
  end else
  begin
    case FFTSize of
      2:
      begin
        if Scale   then scalec(Cplx,2,ScaleFFT2);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins2,Inverse,SwapBuffer);
        ifft2(Cplx);
      end;
      4:
      begin
        if Scale   then scalec(Cplx,4,ScaleFFT4);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins4,Inverse,SwapBuffer);
        ifft4(Cplx);
      end;
      8:
      begin
        if Scale   then scalec(Cplx,8,ScaleFFT8);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins8,Inverse,SwapBuffer);
        ifft8(Cplx);
      end;
      16:
      begin
        if Scale   then scalec(Cplx,16,ScaleFFT16);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins16,Inverse,SwapBuffer);
        ifft16(Cplx);
      end;
      32:
      begin
        if Scale   then scalec(Cplx,32,ScaleFFT32);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins32,Inverse,SwapBuffer);
        ifft32(Cplx);
      end;
      64:
      begin
        if Scale   then scalec(Cplx,64,ScaleFFT64);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins64,Inverse,SwapBuffer);
        ifft64(Cplx);
      end;
      128:
      begin
        if Scale   then scalec(Cplx,128,ScaleFFT128);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins128,Inverse,SwapBuffer);
        ifft128(Cplx);
      end;
      256:
      begin
        if Scale   then scalec(Cplx,256,ScaleFFT256);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins256,Inverse,SwapBuffer);
        ifft256(Cplx);
      end;
      512:
      begin
        if Scale   then scalec(Cplx,512,ScaleFFT512);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins512,Inverse,SwapBuffer);
        ifft512(Cplx);
      end;
      1024:
      begin
        if Scale   then scalec(Cplx,1024,ScaleFFT1024);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins1024,Inverse,SwapBuffer);
        ifft1024(Cplx);
      end;
      2048:
      begin
        if Scale   then scalec(Cplx,2048,ScaleFFT2048);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins2048,Inverse,SwapBuffer);
        ifft2048(Cplx);
      end;
      4096:
      begin
        if Scale   then scalec(Cplx,4096,ScaleFFT4096);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins4096,Inverse,SwapBuffer);
        ifft4096(Cplx);
      end;
      8192:
      begin
        if Scale   then scalec(Cplx,8192,ScaleFFT8192);
        if ReOrder then ReOrderFFT(Cplx,FFTSize,@RevBins8192,Inverse,SwapBuffer);
        ifft8192(Cplx);
      end;
    end;
  end;
end;

constructor TDCFFT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCplx := AllocMem(8192 * SizeOf(TComplex));
  fSwapBuffer := AllocMem(8192 * SizeOf(TComplex));
  Flush;
  fFFTSize := fts512;
  fReOrder := True;
  fScale := True;
end;

destructor TDCFFT.Destroy;
begin
  FreeMemory(fCplx);
  FreeMemory(fSwapBuffer);
  inherited Destroy;
end;

procedure TDCFFT.FFT;
begin
  dspDoFFT(fCplx,1 shl (integer(fFFTSize) + 1),False,fScale,fReOrder,fSwapBuffer);
end;

procedure TDCFFT.IFFT;
begin
  dspDoFFT(fCplx,1 shl (integer(fFFTSize) + 1),True,fScale,fReOrder,fSwapBuffer);
end;

function TDCFFT.GetFFTSize : integer;
begin
  Result := 1 shl (integer(fFFTSize) + 1);
end;

procedure TDCFFT.Flush;
begin
  FillChar(fCplx^,8192 * SizeOf(TComplex),0);
end;

procedure InitializeDCFFT;
begin
  InitRevBitTable;
  InitSinCosTable;

  if has3DNowExt then
  begin
    @fft2 := @fft2__3DNow;
    @ifft2 := @ifft2__3DNow;
    @fft4 := @fft4__3DNowExt;
    @ifft4 := @ifft4__3DNowExt;
    @fft8 := @fft8__3DNowExt;
    @ifft8 := @ifft8__3DNowExt;
    @fft16 := @fft16__3DNowExt;
    @ifft16 := @ifft16__3DNowExt;
    @fft32 := @fft32__3DNowExt;
    @ifft32 := @ifft32__3DNowExt;
    @fft64 := @fft64__3DNowExt;
    @ifft64 := @ifft64__3DNowExt;
    @fft128 := @fft128__3DNowExt;
    @ifft128 := @ifft128__3DNowExt;
    @fft256 := @fft256__3DNowExt;
    @ifft256 := @ifft256__3DNowExt;
    @fft512 := @fft512__3DNowExt;
    @ifft512 := @ifft512__3DNowExt;
    @fft1024 := @fft1024__3DNowExt;
    @ifft1024 := @ifft1024__3DNowExt;
    @fft2048 := @fft2048__3DNowExt;
    @ifft2048 := @ifft2048__3DNowExt;
    @fft4096 := @fft4096__3DNowExt;
    @ifft4096 := @ifft4096__3DNowExt;
    @fft8192 := @fft8192__3DNowExt;
    @ifft8192 := @ifft8192__3DNowExt;
    Init3DNowMemory;
    Init3DNowCosineTable;
  end else
  if has3DNow then
  begin
    @fft2 := @fft2__3DNow;
    @ifft2 := @ifft2__3DNow;
    @fft4 := @fft4__3DNow;
    @ifft4 := @ifft4__3DNow;
    @fft8 := @fft8__3DNow;
    @ifft8 := @ifft8__3DNow;
    @fft16 := @fft16__3DNow;
    @ifft16 := @ifft16__3DNow;
    @fft32 := @fft32__3DNow;
    @ifft32 := @ifft32__3DNow;
    @fft64 := @fft64__3DNow;
    @ifft64 := @ifft64__3DNow;
    @fft128 := @fft128__3DNow;
    @ifft128 := @ifft128__3DNow;
    @fft256 := @fft256__3DNow;
    @ifft256 := @ifft256__3DNow;
    @fft512 := @fft512__3DNow;
    @ifft512 := @ifft512__3DNow;
    @fft1024 := @fft1024__3DNow;
    @ifft1024 := @ifft1024__3DNow;
    @fft2048 := @fft2048__3DNow;
    @ifft2048 := @ifft2048__3DNow;
    @fft4096 := @fft4096__3DNow;
    @ifft4096 := @ifft4096__3DNow;
    @fft8192 := @fft8192__3DNow;
    @ifft8192 := @ifft8192__3DNow;
    Init3DNowMemory;
    Init3DNowCosineTable;
  end else
  if hasSSE then
  begin
    @fft2 := @fft2__SSE;
    @ifft2 := @ifft2__SSE;
    @fft4 := @fft4__SSE;
    @ifft4 := @ifft4__SSE;
    @fft8 := @fft8__SSE;
    @ifft8 := @ifft8__SSE;
    @fft16 := @fft16__SSE;
    @ifft16 := @ifft16__SSE;
    @fft32 := @fft32__SSE;
    @ifft32 := @ifft32__SSE;
    @fft64 := @fft64__SSE;
    @ifft64 := @ifft64__SSE;
    @fft128 := @fft128__SSE;
    @ifft128 := @ifft128__SSE;
    @fft256 := @fft256__SSE;
    @ifft256 := @ifft256__SSE;
    @fft512 := @fft512__SSE;
    @ifft512 := @ifft512__SSE;
    @fft1024 := @fft1024__SSE;
    @ifft1024 := @ifft1024__SSE;
    @fft2048 := @fft2048__SSE;
    @ifft2048 := @ifft2048__SSE;
    @fft4096 := @fft4096__SSE;
    @ifft4096 := @ifft4096__SSE;
    @fft8192 := @fft8192__SSE;
    @ifft8192 := @ifft8192__SSE;
    InitSSEMemory;
    InitSSECosineTable;
  end else
  begin
    @fft2  := @fft2__FPU;
    @ifft2 := @ifft2__FPU;
    @fft4  := @fft4__FPU;
    @ifft4 := @ifft4__FPU;
    @fft8 := @fft8__FPU;
    @ifft8 := @ifft8__FPU;
    @fft16 := @fft16__FPU;
    @ifft16 := @ifft16__FPU;
    @fft32 := @fft32__FPU;
    @ifft32 := @ifft32__FPU;
    @fft64 := @fft64__FPU;
    @ifft64 := @ifft64__FPU;
    @fft128 := @fft128__FPU;
    @ifft128 := @ifft128__FPU;
    @fft256 := @fft256__FPU;
    @ifft256 := @ifft256__FPU;
    @fft512 := @fft512__FPU;
    @ifft512 := @ifft512__FPU;
    @fft1024 := @fft1024__FPU;
    @ifft1024 := @ifft1024__FPU;
    @fft2048 := @fft2048__FPU;
    @ifft2048 := @ifft2048__FPU;
    @fft4096 := @fft4096__FPU;
    @ifft4096 := @ifft4096__FPU;
    @fft8192 := @fft8192__FPU;
    @ifft8192 := @ifft8192__FPU;
  end;
end;

procedure FinalizeDCFFT;
begin
  FreeAllocatedMemory;
end;

initialization
  InitializeDCFFT;

finalization
  FinalizeDCFFT;

end.

