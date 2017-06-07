
    (*********************************************************************
     *  visSpectrum.pas                                                  *
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
  @abstract(Used to get some Values for Visual Drawing of a Spectrum.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit visSpectrum;

interface

uses
  Classes, dspConst, dspUtils, dspFastFourier, Math;

type

  { TDCSpectrum - Designed to retrieve some Values to do Visual drawing of a Spectrum.
    Output has always the same scaling on every Buffertype. }
  TDCSpectrum = class(TComponent)
  private
    {@exclude}
    fLogarithmic : Boolean;
    {@exclude}
    fWindow : TWindowMode;
    {@exclude}
    fMinY : integer;
    {@exclude}
    fMaxY : integer;
    {@exclude}
    fFFT : TDCFFT;
    {@exclude}
    fFFTSize : integer;
    {@exclude}
    fMixChannels : Boolean;
    {@exclude}
    fBuffer : TVisualBuffer;
    {@exclude}
    fNotify : TDCVisualNotifyEvent;
    {@exclude}
    fPreEmphesis : Byte;
    {@exclude}
    fEnablePreEmphesis : Boolean;
    {@exclude}
    fSampleSkip : Byte;
    {@exclude}
    procedure SetFFTSize(FFTSize : TDCFFTSize);
    {@exclude}
    function GetFFTSize : TDCFFTSize;
    {@exclude}
    procedure DoPreEmphesis(Buffer : PComplexArray; Value : Single);
    {@exclude}
    function GetSampleSkip : Byte;
    {@exclude}
    procedure SetSampleSkip(Skip : Byte);
  public
    { TDCSpectrum Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCSpectrum Destructor }
    destructor Destroy; override;
    { Processes an amount of Data. if the "OnSpectrumData" isn´t Assigned then
      this procedure Exits. Output is send as a Pointer to a TVisualBuffer that
      also informs how much Samples, Channels and how the Scaling is done.}
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    { Sets every Value of the Buffer to Zero. It´s not really needed to do this. }
    procedure Flush;
    { This is Buffer holds the Result of the Process Call. Usefull if you´re not
      using the OnSpectrumData Event. }
    property Buffer : TVisualBuffer read fBuffer;
  published
    { Sets the Minimum Value of the Range the Output Samples will be. This Value
      MUST be lower then MaxY! default = 0 }
    property MinY : integer read fMinY write fMinY;
    { Sets the Maximum Value of the Range the Output Samples will be. This Value
      MUST be higher then MinY! default = 100 }
    property MaxY : integer read fMaxY write fMaxY;
    { If Enabled then every Channel will be Mixed to one (first) Output Channel.
      If Disabled then every Channel gets his own Buffer. default = True }
    property MixChannels : Boolean read fMixChannels write fMixChannels;
    { This Event will be raised after the Process procedure has been called.
      You MUST Assign it or no Buffer will be Processed! }
    property OnSpectrumData : TDCVisualNotifyEvent read fNotify write fNotify;
    { Sets the FFT Size that will be used at the Transformation. Can be any Value
      of TDCFFTSize. Note that only half of the FFT Size is send to the Output
      Buffer. eg: Calling Process with a 512 Point FFT will Result with an Output
      Buffer of 256 Samples. For accurate Values a FFT Size of 512 or higher should
      be chosen. default = fts1024 }
    property FFTSize : TDCFFTSize read GetFFTSize write SetFFTSize;
    { PreEmphesis makes the Amplification Values of the Samples in all Frequencies
      more constant, otherwise lower Frequencies will have a much higher Amplification
      Value then higher Frequencies. default = True }
    property PreEmphesis : Byte read fPreEmphesis write fPreEmphesis;
    { Enables or Disables PreEmphesis. default = True }
    property PreEmphesisEnabled : Boolean read fEnablePreEmphesis write fEnablePreEmphesis;
    { Skips an amount of Samples. This makes the Visual drawing smoother.
      default = 1 }
    property SampleSkip : Byte read GetSampleSkip write SetSampleSkip;
    { Specifys the Windowing Mode. Set to Rectangular to disable it. }
    property WindowMode : TWindowMode read fWindow write fWindow;
    { A logarithmic Value will Display silence Parts more aggressive. }
    property Logarithmic : Boolean read fLogarithmic write fLogarithmic;
  end;

implementation

constructor TDCSpectrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFFT := TDCFFT.Create(Self);
  fFFT.Scale := False;
  fFFT.ReOrder := True;
  fMixChannels := True;
  fSampleSkip := 1;
  fWindow := wmRectangular;
  fMinY := 0;
  fMaxY := 100;
  fEnablePreEmphesis := True;
  fPreEmphesis := 90;
  fLogarithmic := True;
  SetFFTSize(fts1024);
  Flush;
end;

destructor TDCSpectrum.Destroy;
begin
  fFFT.Free;
  inherited Destroy;
end;

procedure TDCSpectrum.SetFFTSize(FFTSize : TDCFFTSize);
begin
  fFFT.FFTSize := FFTSize;
  fFFTSize := 1 shl (integer(fFFT.FFTSize) + 1);
end;

function TDCSpectrum.GetFFTSize : TDCFFTSize;
begin
  Result := fFFT.FFTSize;
end;

procedure TDCSpectrum.DoPreEmphesis(Buffer : PComplexArray; Value : Single);
var
  i : Integer;
  F, AC0 : Single;
begin
  f := Value * Buffer^[0].re;
  for i := 1 to fFFTSize -1 do
  begin
    ac0 := Value * Buffer^[i].re;
    Buffer^[i].re := Buffer^[i].re - F;
    F := ac0;
  end;
end;

procedure TDCSpectrum.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32i : PIntegerArray;
  Buf32 : PFloatArray;
  pChannels : integer;
  NumSamples : integer;
  i, c : integer;
  tmp : integer;
  ch : integer;
  Mul : Single;
  fRange : integer;
  Skip : integer;
  tmp2 : Double;
begin
  if (Buffer = nil) or (Size = 0) or (Bits = 0) or (Channels = 0) then Exit;
  Skip := fSampleSkip;
  if Skip < 1 then Skip := 1;
  NumSamples := Size div (Bits div 8) div Channels div (fSampleSkip + 1);
  if Channels > MaxChannels then ch := MaxChannels else ch := Channels;
  if MixChannels then pChannels := 1 else pChannels := ch;
  if NumSamples > (fFFTSize shr 1) then NumSamples := fFFTSize shr 1;

  Mul := 1.0;

  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      if MixChannels then
      begin
        fFFT.Flush;
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            tmp := tmp + ((Buf8^[(i * Channels * Skip) + c] - 128) * 256);
          end;
          fFFT.Complex[i].re := tmp div ch;
        end;
        for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
        if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
        fFFT.FFT;
        for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[0,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
      end else
      begin
        for c := 0 to ch -1 do
        begin
          fFFT.Flush;
          for i := 0 to NumSamples -1 do fFFT.Complex[i].re := ((Buf8^[(i * Channels * Skip) + c] - 128) * 256);
          for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
          if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
          fFFT.FFT;
          for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[c,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      if MixChannels then
      begin
        fFFT.Flush;
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            tmp := tmp + (Buf16^[(i * Channels * Skip) + c]);
          end;
          fFFT.Complex[i].re := tmp div ch;
        end;
        for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
        if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
        fFFT.FFT;
        for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[0,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
      end else
      begin
        for c := 0 to ch -1 do
        begin
          fFFT.Flush;
          for i := 0 to NumSamples -1 do fFFT.Complex[i].re := (Buf16^[(i * Channels * Skip) + c]);
          for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
          if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
          fFFT.FFT;
          for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[c,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      if MixChannels then
      begin
        fFFT.Flush;
        for i := 0 to NumSamples -1 do
        begin
          tmp := 0;
          for c := 0 to ch -1 do
          begin
            tmp := tmp + (Cvt24BitTo32(Buf24^[(i * Channels * Skip) + c]) div 256);
          end;
          fFFT.Complex[i].re := tmp div ch;
        end;
        for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
        if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
        fFFT.FFT;
        for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[0,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
      end else
      begin
        for c := 0 to ch -1 do
        begin
          fFFT.Flush;
          for i := 0 to NumSamples -1 do fFFT.Complex[i].re := (Cvt24BitTo32(Buf24^[(i * Channels * Skip) + c]) div 256);
          for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
          if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
          fFFT.FFT;
          for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[c,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
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
          fFFT.Flush;
          for i := 0 to NumSamples -1 do
          begin
            tmp := 0;
            for c := 0 to ch -1 do
            begin
              tmp := tmp + (Round(Buf32^[(i * Channels * Skip) + c] * 32767));
            end;
            fFFT.Complex[i].re := tmp div ch;
          end;
          for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
          if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
          fFFT.FFT;
          for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[0,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
        end else
        begin
          for c := 0 to ch -1 do
          begin
            fFFT.Flush;
            for i := 0 to NumSamples -1 do fFFT.Complex[i].re := (Round(Buf32^[(i * Channels * Skip) + c] * 32767));
            for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
            if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
            fFFT.FFT;
            for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[c,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        if MixChannels then
        begin
          fFFT.Flush;
          for i := 0 to NumSamples -1 do
          begin
            tmp := 0;
            for c := 0 to ch -1 do
            begin
              tmp := tmp + (Buf32i^[(i * Channels * Skip) + c] div 65536);
            end;
            fFFT.Complex[i].re := tmp div ch;
          end;
          for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
          if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
          fFFT.FFT;
          for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[0,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
        end else
        begin
          for c := 0 to ch -1 do
          begin
            fFFT.Flush;
            for i := 0 to NumSamples -1 do fFFT.Complex[i].re := (Buf32i^[(i * Channels * Skip) + c] div 65536);
            for i := 0 to fFFTSize -1 do fFFT.Complex[i].re := GetWindowingValue(fFFT.Complex[i].re,i,fFFTSize,fWindow);
            if fEnablePreEmphesis then DoPreEmphesis(fFFT.Complex,fPreEmphesis / 100);
            fFFT.FFT;
            for i := 0 to ((fFFTSize shr 1) -1) do fBuffer[c,i] := (Round(FFTSum(fFFt.Complex[i].re,fFFt.Complex[i].im)));
          end;
        end;
      end;
    end;
  end;

  case fFFT.FFTSize of
    fts2:    Mul := 11.0;
    fts4:    Mul := 10.0;
    fts8:    Mul := 9.0;
    fts16:   Mul := 8.0;
    fts32:   Mul := 7.0;
    fts64:   Mul := 6.0;
    fts128:  Mul := 5.0;
    fts256:  Mul := 4.0;
    fts512:  Mul := 3.2;
    fts1024: Mul := 2.4;
    fts2048: Mul := 1.5;
    fts4096: Mul := 1.2;
    fts8192: Mul := 1.0;
  end;

  fRange := fMaxY - fMinY;

  for c := 0 to ch -1 do
    for i := 0 to fFFTSize shr 1 -1 do
    begin
      tmp2 := fBuffer[c,i] * Mul - 50000;
      if tmp2 < 0 then tmp2 := 0;
      tmp2 := tmp2 * fRange / 700000;
      if fLogarithmic then tmp2 := sqrt(tmp2 * fRange);
      fBuffer[c,i] := Round(tmp2 + fMinY);
    end;

  if Assigned(fNotify) then fNotify(Self,@fBuffer,fMinY,fMaxY,NumSamples,pChannels);
end;

procedure TDCSpectrum.Flush;
begin
  FillChar(fBuffer,SizeOf(TVisualBuffer),0);
end;

function TDCSpectrum.GetSampleSkip : Byte;
begin
  Result := fSampleSkip;
end;

procedure TDCSpectrum.SetSampleSkip(Skip : Byte);
begin
  fSampleSkip := EnsureRange(Skip,1,255);
end;

end.

