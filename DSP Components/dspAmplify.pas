
    (*********************************************************************
     *  dspAmplify.pas                                                   *
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
  @abstract(DSP Filter that Amplifys Audio Data.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspAmplify;

interface

uses
  Classes, dspConst, dspUtils, Math, Forms, dspInterfaces, Windows;

type
  { TDCAmplify - Class to Amplify Audiodata. Amplification can be done seperate
    on each Channel. 32 Bit Float Data is tuned to use 3DNow/SSE instructions.
    When using a SSE capable CPU, make sure that Data is aligned on a 16 Byte
    boundary. The Buffer is checked for 16byte Alignment, if this is not present,
    then the default FPU will be used. }
  TDCAmplify = class(TComponent, IDCAmplify)
  protected
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fVolume : array[0..MaxChannels -1] of Cardinal;
    {@exclude}
    procedure SetVolume(Channel : Byte; Volume : integer);
    {@exclude}
    function GetVolume(Channel : Byte) : integer;
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
  public
    { Creates an Instance of TDCAmplify. }
    constructor Create(AOwner: TComponent); override;
    { Destroys the Instance. }
    destructor Destroy; override;
    { Call this Method to Process Audio Data. Buffer must be aligned on a 16 Byte
      boundary. Use the 'GetAlignedMemory' to create such a Buffer. Float is needed
      for 32 Bit Buffers, to determane whether the Buffer is Float or Integer. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Sets the Amplification for a Channel. Default Value is 10000, which means
      that no Amplification occours. A Value of 20000 raises the Amplification
      by 2. If Seperate is True, then every Channel will be Amplified by its
      Channel Amplification Value. If Seperate is False, then every Channel is
      Amplified with the Value of Channel 0. }
    property Volume[Channel : Byte] : integer read GetVolume write SetVolume;
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Volume(aChannel: Byte; out aVolume: integer): HRESULT; stdcall;
    {@exclude}
    function set_Volume(aChannel: Byte; aVolume: integer): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
  published
    { Enables or Disables seperate Amplification. If Enabled every Channel will
      be Amplified by it´s own Amplification Value. If Disabled every Channel
      will be Amplified with the Amplification Value of Channel 0. }
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
    property SampleSize : Cardinal read fSampleSize write fSampleSize;
  end;

implementation

constructor TDCAmplify.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to MaxChannels -1 do fVolume[i] := 10000;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
end;

destructor TDCAmplify.Destroy;
begin
  inherited Destroy;
end;

procedure TDCAmplify.SetVolume(Channel : Byte; Volume : integer);
begin
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  fVolume[Channel] := Volume;
end;

function TDCAmplify.GetVolume(Channel : Byte) : integer;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) then Exit;
  Result := fVolume[Channel];
end;

procedure TDCAmplify.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
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

procedure TDCAmplify.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
const
  NormalLevel = 10000;
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24  : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  NumSamples : integer;
  i, c : integer;
  Vol : Single;
  nVol: Integer;
  PackedSize : Integer;
begin
  if Seperate then
  begin
    NumSamples := Size div (Bits div 8) div Channels;
    case Bits of
      8:
      begin
        Buf8 := PByteArray(Buffer);
        for c := 0 to Channels -1 do
          for i := 0 to NumSamples -1 do Buf8^[i * Channels + c] := clip_8(integer(Buf8^[i * Channels + c] - 128) * Volume[c] div 10000) + 128;
      end;
      16:
      begin
        Buf16 := PSmallIntArray(Buffer);
        for c := 0 to Channels -1 do
          for i := 0 to NumSamples -1 do Buf16^[i * Channels + c] := clip_16(integer(Buf16^[i * Channels + c]) * Volume[c] div 10000);
      end;
      24:
      begin
        Buf24 := PInteger24Array(Buffer);
        for c := 0 to Channels -1 do
          for i := 0 to NumSamples -1 do Buf24^[i * Channels + c] := cvt32BitTo24(clip_24(int64(cvt24BitTo32(Buf24^[i * Channels + c])) * Volume[c] div 10000));
      end;
      32:
      begin
        if Float then
        begin
          Buf32 := PFloatArray(Buffer);
          for c := 0 to Channels -1 do
          begin
            Vol := Volume[c] / 10000;
            for i := 0 to NumSamples -1 do Buf32^[i * Channels + c] := Buf32^[i * Channels + c] * Vol;
          end;
        end else
        begin
          Buf32i := PIntegerArray(Buffer);
          for c := 0 to Channels -1 do
            for i := 0 to NumSamples -1 do Buf32i^[i * Channels + c] := clip_32(Int64(Buf32i^[i * Channels + c]) * Volume[c] div 10000);
        end;
      end;
    end;
  end else
  begin
    NumSamples := Size div (Bits div 8);
    case Bits of
      8:
      begin
        Buf8 := PByteArray(Buffer);
        for i := 0 to NumSamples -1 do Buf8^[i] := clip_8(integer(Buf8^[i] - 128) * Volume[0] div 10000) + 128;
      end;
      16:
      begin
        nVol := Volume[0];
        asm //  improved by Laurent Taupin   laurent@vlproduction.com
            push    esi
            push    edi
            push    ebx

            mov     esi,Buffer
            mov     edi,Buffer             // takes buffer pointer;)
            mov     ecx,Size               // gets buffer size
            shr     ecx,1                  // divided by 2 if in 16 bits

         @@Loop16Bits:
            lodsw
            cwde

            imul    nVol
            mov     ebx,NormalLevel
            idiv    ebx

            cmp     eax,32767
            jle     @@Lower16
            mov     ax,32767               // take care of the sign !!
            jmp     @@Finished16
         @@Lower16:
            cmp     eax,-32768
            jge     @@Finished16
            mov     ax,-32768              // take care of the sign !!
         @@Finished16:
            stosw
            Loop    @@Loop16Bits
         @@Finished:
            pop     ebx
            pop     edi
            pop     esi
        end;
      end;
      24:
      begin
        Buf24 := PInteger24Array(Buffer);
        for i := 0 to NumSamples -1 do Buf24^[i] := cvt32BitTo24(clip_24(int64(cvt24BitTo32(Buf24^[i])) * Volume[0] div 10000));
      end;
      32:
      begin
        if Float then
        begin
          Vol := Volume[0] / 10000;
          if has3DNow then
          begin
            PackedSize := Size and -8;
            asm
              femms                     // Fast MMX Enter/Leave
              mov       eax, Buffer     // Move Buffer Pointer into EAX
              xor       ecx, ecx        // Set ECX to Zero
              prefetch  [eax]           // give the mmu a heads-up
              movd      mm1, Vol        // --- | Vol
              punpckldq mm1, mm1        // Vol | Vol
              @@Loop2:
              movq      mm2, [eax+ecx]  // Spl1 | Spl2
              pfmul     mm2, mm1        // Spl1 * Vol | Spl2 * Vol
              movq      [eax+ecx], mm2  // Store Amplified Sample back to RAM
              add       ecx, 8
              prefetch  [eax+ecx]     // Inform mmu about next Sample Position
              cmp       ecx, PackedSize // Loop until ECX = PackedSize
              jl        @@Loop2
              femms                     // Fast MMX Enter/Leave
              mov       ecx, PackedSize // Do some Processing on the FPU if
              cmp       ecx, Size       // Size doesn´t fit into 8byte Chunks
              jz        @@End2
              fld       [eax+ecx]
              fmul      Vol
              fstp      [eax+ecx]
              @@End2:
            end;
          end else
          if hasSSE and IsValidSSEBuffer(Buffer) then
          begin
            PackedSize := Size and -16;
            asm
              mov       eax, Buffer     // Move Buffer Pointer into EAX
              xor       ecx, ecx        // Set ECX to Zero
              movss     xmm1, Vol       // --- | --- | --- | Vol
              shufps    xmm1, xmm1, $0  // Vol | Vol | Vol | Vol
              @@Loop1:
              movaps    xmm2, [eax+ecx] // Spl1 | Spl2 | Spl3 | Spl4  Since the Buffer is Aligned to 16Byte, MOVAPS can be used instead of MOVUPS
              mulps     xmm2, xmm1      // Spl1 * Vol | Spl2 * Vol | Spl3 * Vol | Spl4 * Vol
              movaps    [eax+ecx], xmm2 // Store Amplified Sample back to RAM
              add       ecx, 16
              cmp       ecx, PackedSize // Loop until ECX = PackedSize
              jl        @@Loop1
              mov       ecx, PackedSize // Do some Processing on the FPU if
              cmp       ecx, Size       // Size doesn´t fit into 16byte Chunks
              jz        @@End1
              @@Loop11:
              fld       [eax+ecx]
              fmul      Vol
              fstp      [eax+ecx]
              add       ecx, 4
              cmp       ecx, Size
              jl        @@Loop11
              @@End1:
            end
          end else
          if hasSSE then
          begin
            PackedSize := Size and -16;
            asm
              mov       eax, Buffer     // Move Buffer Pointer into EAX
              xor       ecx, ecx        // Set ECX to Zero
              movss     xmm1, Vol       // --- | --- | --- | Vol
              shufps    xmm1, xmm1, $0  // Vol | Vol | Vol | Vol
              @@Loop1:
              movups    xmm2, [eax+ecx] // Spl1 | Spl2 | Spl3 | Spl4  Unaligned call
              mulps     xmm2, xmm1      // Spl1 * Vol | Spl2 * Vol | Spl3 * Vol | Spl4 * Vol
              movups    [eax+ecx], xmm2 // Store Amplified Sample back to RAM
              add       ecx, 16
              cmp       ecx, PackedSize // Loop until ECX = PackedSize
              jl        @@Loop1
              mov       ecx, PackedSize // Do some Processing on the FPU if
              cmp       ecx, Size       // Size doesn´t fit into 16byte Chunks
              jz        @@End1
              @@Loop11:
              fld       [eax+ecx]
              fmul      Vol
              fstp      [eax+ecx]
              add       ecx, 4
              cmp       ecx, Size
              jl        @@Loop11
              @@End1:
            end
          end else
          begin
            asm
              mov       eax, Buffer     // Move Buffer Pointer into EAX
              xor       ecx, ecx        // Set ECX to Zero
              @@Loop3:
              fld       [eax+ecx]
              fmul      Vol
              fstp      [eax+ecx]
              add       ecx, 4
              cmp       ecx, Size // Loop until ECX = PackedSize
              jl        @@Loop3
            end;
          end;
        end else
        begin
          Buf32i := PIntegerArray(Buffer);
          for i := 0 to NumSamples -1 do Buf32i^[i] := clip_32(Int64(Buf32i^[i]) * Volume[0] div 10000);
        end;
      end;
    end;
  end;
end;
(*** IDCAmplify ***************************************************************)
function TDCAmplify.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCAmplify.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCAmplify.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCAmplify.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCAmplify.get_Volume(aChannel: Byte; out aVolume: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  aVolume := GetVolume(aChannel);
end;

function TDCAmplify.set_Volume(aChannel: Byte; aVolume: integer): HRESULT; stdcall;
begin
  Result := S_OK;
  SetVolume(aChannel,aVolume);
end;

end.

