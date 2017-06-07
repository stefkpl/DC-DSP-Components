
    (*********************************************************************
     *  dspEqualizer.pas                                                 *
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
  @abstract(DSP Filter that Equalize specific Channels.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspEqualizer;

interface

uses
  Classes, dspConst, dspUtils, dspFastFourier, Math, Forms, dspInterfaces,
  Windows;

type
  { TDCEqualizer - Equalizer Component that can Equalize from 1 (2 Point FFT)
    up to 4096 Bands (8192 Point FFT). Each Channel can be equalized seperate. }
  TDCEqualizer = class(TComponent, IDCEqualizer)
  protected
    {@exclude}
    fFFT : TDCFFT;
    {@exclude}
    fCoeff : array[0..MaxChannels -1] of TDCFFT;
    {@exclude}
    fFFTSize : integer;
    {@exclude}
    fFFTSize_3_4 : integer;
    {@exclude}
    fFFTSize_1_2 : integer;
    {@exclude}
    fFFTSize_1_4 : integer;
    {@exclude}
    fFFTSize_1_8 : integer;
    {@exclude}
    fSeperate : Boolean;
    {@exclude}
    fEnabled : Boolean;
    {@exclude}
    fProcessMessages : Boolean;
    {@exclude}
    fSampleSize : Cardinal;
    {@exclude}
    fPreviousOutput : TEqualizerPrevOut;
    {@exclude}
    fNeedCoeff : Boolean;
    {@exclude}
    fBands : TEqualizerBands;
    {@exclude}
    procedure SetFFTSize(FFTSize : TDCFFTSize);
    {@exclude}
    function GetFFTSize : TDCFFTSize;
    {@exclude}
    procedure SetBand(Channel : Byte; Index : WORD; Amp : ShortInt);
    {@exclude}
    function GetBand(Channel : Byte; Index : WORD) : ShortInt;
    {@exclude}
    procedure BuildFilterCoefficients(Channels : Byte; Seperate : Boolean);
    {@exclude}
    procedure DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    procedure SetEnabled (Enabled : Boolean);
    {@exclude}
    procedure SetSeperate (Seperate : Boolean);
  public
    { TDCEqualizer Constructor }
    constructor Create(AOwner: TComponent); override;
    { TDCEqualizer Destructor }
    destructor Destroy; override;
    { Resets the temporary stored Samples that has been Processed last. Use this
      if a seek occours. }
    procedure Flush;
    { Sets the Amplification Value for a specific Channel and Frequency. Values
      must be in Range of -128 (~ -20db) to 127 (~ +20db). }
    property Band[Channel : Byte; Index : WORD] : ShortInt read getBand write SetBand;
    { Call this to Process an amount of Data. }
    procedure Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
    {@exclude}
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    {@exclude}
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    {@exclude}
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
    {@exclude}
    function get_Band(aChannel: Byte; aIndex: Word; out aBand: ShortInt): HRESULT; stdcall;
    {@exclude}
    function set_Band(aChannel: Byte; aIndex: Word; aBand: ShortInt): HRESULT; stdcall;
  published
    { Enables or Disables seperate equalization. If Enabled every Channel will
      be Equalized by it´s own Amplification Value. If Disabled every Channel
      will be Equalized with the Amplification Value of Channel 0. }
    property Seperate : Boolean read fSeperate write SetSeperate;
    { Enables or Disables the Filter. }
    property Enabled : Boolean read fEnabled write SetEnabled;
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
    { Sets the FFT Size that will be used for the Equalization. The number of
      Bands the Equalizer will have is always half of the FFT Size. }
    property FFTSize : TDCFFTSize read GetFFTSize write SetFFTSize;
  end;

implementation

uses SysUtils;

constructor TDCEqualizer.Create(AOwner: TComponent);
var
  i, c : integer;
begin
  inherited Create(AOwner);
  fFFT := TDCFFT.Create(Self);
  fFFT.Scale := True;
  fFFT.ReOrder := False;
  fSeperate := False;
  fEnabled := False;
  fProcessMessages := False;
  fSampleSize := DefaultSampleSize;
  for c := 0 to MaxChannels -1 do
  begin
    for i := 0 to 8191 do fBands[c,i] := 2.0;
    fCoeff[c] := TDCFFT.Create(Self);
    fCoeff[c].Scale := True;
    fCoeff[c].ReOrder := False;
  end;
  SetFFTSize(fts1024);
  fNeedCoeff := True;
end;

destructor TDCEqualizer.Destroy;
var
  i : integer;
begin
  for i := 0 to MaxChannels -1 do FreeAndNil(fCoeff[i]);
  fFFT.Free;
  inherited Destroy;
end;

procedure TDCEqualizer.SetFFTSize(FFTSize : TDCFFTSize);
var
  i : integer;
begin
  fFFT.FFTSize := FFTSize;
  for i := 0 to MaxChannels -1 do fCoeff[i].FFTSize := FFTSize;
  fFFTSize := 1 shl (integer(fFFT.FFTSize) + 1);
  fFFTSize_3_4 := fFFTSize * 3 div 4;
  fFFTSize_1_2 := fFFTSize div 2;
  fFFTSize_1_4 := fFFTSize div 4;
  fFFTSize_1_8 := fFFTSize div 8;
end;

function TDCEqualizer.GetFFTSize : TDCFFTSize;
begin
  Result := fFFT.FFTSize;
end;

procedure TDCEqualizer.Flush;
begin
  FillChar(fPreviousOutput,SizeOf(TEqualizerPrevOut),0);
end;

procedure TDCEqualizer.SetSeperate (Seperate : Boolean);
begin
  if fSeperate = Seperate then Exit;
  fNeedCoeff := True;
  fSeperate := Seperate;
end;

procedure TDCEqualizer.SetBand(Channel : Byte; Index : WORD; Amp : ShortInt);
begin
  if not InRange(Channel,0,MaxChannels -1) or not InRange(Index,0,8191) then Exit;
  fBands[Channel,Index] := 2 * Power(10.0,Amp/100.0);
  fNeedCoeff := True;
end;

function TDCEqualizer.GetBand(Channel : Byte; Index : WORD) : ShortInt;
begin
  Result := 0;
  if not InRange(Channel,0,MaxChannels -1) or not InRange(Index,0,8191) then Exit;
  Result := Round(100.0 * log10(fBands[Channel,Index]/2));
end;

procedure TDCEqualizer.SetEnabled (Enabled : Boolean);
begin
  if fEnabled = Enabled then Exit;
  if Enabled then Flush;
  fEnabled := Enabled;
end;

procedure TDCEqualizer.BuildFilterCoefficients(Channels : Byte; Seperate : Boolean);
var
  i, j, c : Integer;
begin
  if not fSeperate then Channels := 1;
  fNeedCoeff := False;
  for c := 0 to Channels -1 do
  begin
    fCoeff[c].Flush;
    for i := 0 to fFFTSize_1_2 -1 do fCoeff[c].Complex[i].re := fBands[c][i];
    fCoeff[c].ReOrder := True;
    fCoeff[c].IFFT;
    fCoeff[c].ReOrder := False;
    for i := 0 to (fFFTSize -1) do
    begin
      j := i + fFFTSize div 8;
      if j >= fFFTSize then j := j - fFFTSize;
      fCoeff[c].Complex[j].im := fCoeff[c].Complex[i].re;
    end;
    for i:= 0 to fFFTSize_1_4 do fCoeff[c].Complex[i].re := fCoeff[c].Complex[i].im;
    for i := fFFTSize_1_4 +1 to (fFFTSize -1) do  fCoeff[c].Complex[i].re := 0;
    for i := 0 to (fFFTSize -1) do  fCoeff[c].Complex[i].re :=  fCoeff[c].Complex[i].re * (0.5 - 0.5 * cos(2 * PI * i / (fFFTSize_1_4 +2)));
    fCoeff[c].FFT;
  end;
end;

procedure TDCEqualizer.Process(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  SplitBuffer : PChar;
  SizeLeft : integer;
  SplitSize : integer;
  CurrentSize : integer;
begin
  if not fEnabled then Exit;
  if fNeedCoeff then BuildFilterCoefficients(Channels,Seperate);
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

procedure TDCEqualizer.DoDSP(Buffer : Pointer; Size : Integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  Buf8  : PByteArray;
  Buf16 : PSmallIntArray;
  Buf24 : PInteger24Array;
  Buf32 : PFloatArray;
  Buf32i : PIntegerArray;
  samplesLeft,
  c,i,z : integer;
  CurrentPos : integer;
  Swap : single;
  NumSamples : integer;
begin
  NumSamples := Size div Channels div (Bits div 8);
  case Bits of
    8:
    begin
      Buf8 := PByteArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        samplesLeft := NumSamples;
        while (samplesLeft > 0) do
        begin
          CurrentPos := (NumSamples - samplesLeft) * Channels + c;
          fFFT.Flush;
          for i := 0 to fFFTSize_3_4 -1 do
              if (samplesLeft < fFFTSize_3_4)
                then if i < samplesLeft
                  then fFFT.Complex[i].re := Buf8^[CurrentPos + (i * Channels)] - 128
                  else
                else fFFT.Complex[i].re := Buf8^[CurrentPos + (i * Channels)] - 128;
          fFFT.FFT;
          if Seperate then z := c else z := 0;
          for i := 0 to fFFTSize -1 do
          begin
            Swap := fFFT.Complex[i].im;
            fFFT.Complex[i].im := (fFFT.Complex[i].re * fCoeff[z].Complex[i].im + fFFT.Complex[i].im * fCoeff[z].Complex[i].re);
            fFFT.Complex[i].re := (fFFT.Complex[i].re * fCoeff[z].Complex[i].re - Swap * fCoeff[z].Complex[i].im);
          end;
          fFFT.IFFT;
          for i := 0 to fFFTSize_1_4 -1 do fFFT.Complex[i].re:= fFFT.Complex[i].re + fPreviousOutput[c][i];
          if (samplesLeft < fFFTSize_3_4) then for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[samplesleft + i].re
                                         else for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[fFFTSize_3_4 + i].re;
          for i := 0 to fFFTSize_3_4 -1 do
            if (samplesLeft < fFFTSize_3_4)
              then if i < samplesLeft
                then Buf8^[CurrentPos + (i * Channels)] := clip_8(Round(fFFT.Complex[i].re)) + 128
                else
              else Buf8^[CurrentPos + (i * Channels)] := clip_8(Round(fFFT.Complex[i].re)) + 128;
          dec(samplesLeft,fFFTSize_3_4);
        end;
      end;
    end;
    16:
    begin
      Buf16 := PSmallIntArray(Buffer);
      for c := 0 to Channels -1 do
      begin
        samplesLeft := NumSamples;
        while (samplesLeft > 0) do
        begin
          CurrentPos := ((NumSamples - samplesLeft) * Channels) + c;
          fFFT.Flush;
          for i := 0 to fFFTSize_3_4 -1 do
              if (samplesLeft < fFFTSize_3_4)
                then if i < samplesLeft
                  then fFFT.Complex[i].re := Buf16^[CurrentPos + (i * Channels)]
                  else
                else fFFT.Complex[i].re := Buf16^[CurrentPos + (i * Channels)];
          fFFT.FFT;
          if Seperate then z := c else z := 0;
          for i := 0 to fFFTSize -1 do
          begin
            Swap := fFFT.Complex[i].im;
            fFFT.Complex[i].im := (fFFT.Complex[i].re * fCoeff[z].Complex[i].im + fFFT.Complex[i].im * fCoeff[z].Complex[i].re);
            fFFT.Complex[i].re := (fFFT.Complex[i].re * fCoeff[z].Complex[i].re - Swap * fCoeff[z].Complex[i].im);
          end;
          fFFT.IFFT;
          for i := 0 to fFFTSize_1_4 -1 do fFFT.Complex[i].re:= fFFT.Complex[i].re + fPreviousOutput[c][i];
          if (samplesLeft < fFFTSize_3_4) then for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[samplesleft + i].re
                                         else for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[fFFTSize_3_4 + i].re;
          for i := 0 to fFFTSize_3_4 -1 do
            if (samplesLeft < fFFTSize_3_4)
              then if i < samplesLeft
                then Buf16^[CurrentPos + (i * Channels)] := clip_16(Round(fFFT.Complex[i].re))
                else
              else Buf16^[CurrentPos + (i * Channels)] := clip_16(Round(fFFT.Complex[i].re));
          dec(samplesLeft,fFFTSize_3_4);
        end;
      end;
    end;
    24:
    begin
      Buf24 := PInteger24Array(Buffer);
      for c := 0 to Channels -1 do
      begin
        samplesLeft := NumSamples;
        while (samplesLeft > 0) do
        begin
          CurrentPos := ((NumSamples - samplesLeft) * Channels) + c;
          fFFT.Flush;
          for i := 0 to fFFTSize_3_4 -1 do
              if (samplesLeft < fFFTSize_3_4)
                then if i < samplesLeft
                  then fFFT.Complex[i].re := Cvt24BitTo32(Buf24^[CurrentPos + (i * Channels)])
                  else
                else fFFT.Complex[i].re := Cvt24BitTo32(Buf24^[CurrentPos + (i * Channels)]);
          fFFT.FFT;
          if Seperate then z := c else z := 0;
          for i := 0 to fFFTSize -1 do
          begin
            Swap := fFFT.Complex[i].im;
            fFFT.Complex[i].im := (fFFT.Complex[i].re * fCoeff[z].Complex[i].im + fFFT.Complex[i].im * fCoeff[z].Complex[i].re);
            fFFT.Complex[i].re := (fFFT.Complex[i].re * fCoeff[z].Complex[i].re - Swap * fCoeff[z].Complex[i].im);
          end;
          fFFT.IFFT;
          for i := 0 to fFFTSize_1_4 -1 do fFFT.Complex[i].re:= fFFT.Complex[i].re + fPreviousOutput[c][i];
          if (samplesLeft < fFFTSize_3_4) then for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[samplesleft + i].re
                                         else for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[fFFTSize_3_4 + i].re;
          for i := 0 to fFFTSize_3_4 -1 do
            if (samplesLeft < fFFTSize_3_4)
              then if i < samplesLeft
                then Buf24^[CurrentPos + (i * Channels)] := Cvt32BitTo24(clip_24(Round(fFFT.Complex[i].re)))
                else
              else Buf24^[CurrentPos + (i * Channels)] := Cvt32BitTo24(clip_24(Round(fFFT.Complex[i].re)));
          dec(samplesLeft,fFFTSize_3_4);
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
          samplesLeft := NumSamples;
          while (samplesLeft > 0) do
          begin
            CurrentPos := ((NumSamples - samplesLeft) * Channels) + c;
            fFFT.Flush;
            for i := 0 to fFFTSize_3_4 -1 do
                if (samplesLeft < fFFTSize_3_4)
                  then if i < samplesLeft
                    then fFFT.Complex[i].re := Buf32^[CurrentPos + (i * Channels)]
                    else
                  else fFFT.Complex[i].re := Buf32^[CurrentPos + (i * Channels)];
            fFFT.FFT;
            if Seperate then z := c else z := 0;
            for i := 0 to fFFTSize -1 do
            begin
              Swap := fFFT.Complex[i].im;
              fFFT.Complex[i].im := (fFFT.Complex[i].re * fCoeff[z].Complex[i].im + fFFT.Complex[i].im * fCoeff[z].Complex[i].re);
              fFFT.Complex[i].re := (fFFT.Complex[i].re * fCoeff[z].Complex[i].re - Swap * fCoeff[z].Complex[i].im);
            end;
            fFFT.IFFT;
            for i := 0 to fFFTSize_1_4 -1 do fFFT.Complex[i].re:= fFFT.Complex[i].re + fPreviousOutput[c][i];
            if (samplesLeft < fFFTSize_3_4) then for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[samplesleft + i].re
                                           else for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[fFFTSize_3_4 + i].re;
            for i := 0 to fFFTSize_3_4 -1 do
              if (samplesLeft < fFFTSize_3_4)
                then if i < samplesLeft
                  then Buf32^[CurrentPos + (i * Channels)] := fFFT.Complex[i].re
                  else
                else Buf32^[CurrentPos + (i * Channels)] := fFFT.Complex[i].re;
            dec(samplesLeft,fFFTSize_3_4);
          end;
        end;
      end else
      begin
        Buf32i := PIntegerArray(Buffer);
        for c := 0 to Channels -1 do
        begin
          samplesLeft := NumSamples;
          while (samplesLeft > 0) do
          begin
            CurrentPos := ((NumSamples - samplesLeft) * Channels) + c;
            fFFT.Flush;
            for i := 0 to fFFTSize_3_4 -1 do
                if (samplesLeft < fFFTSize_3_4)
                  then if i < samplesLeft
                    then fFFT.Complex[i].re := Buf32i^[CurrentPos + (i * Channels)] / 32768
                    else
                  else fFFT.Complex[i].re := Buf32i^[CurrentPos + (i * Channels)] / 32768;
            fFFT.FFT;
            if Seperate then z := c else z := 0;
            for i := 0 to fFFTSize -1 do
            begin
              Swap := fFFT.Complex[i].im;
              fFFT.Complex[i].im := (fFFT.Complex[i].re * fCoeff[z].Complex[i].im + fFFT.Complex[i].im * fCoeff[z].Complex[i].re);
              fFFT.Complex[i].re := (fFFT.Complex[i].re * fCoeff[z].Complex[i].re - Swap * fCoeff[z].Complex[i].im);
            end;
            fFFT.IFFT;
            for i := 0 to fFFTSize_1_4 -1 do fFFT.Complex[i].re:= fFFT.Complex[i].re + fPreviousOutput[c][i];
            if (samplesLeft < fFFTSize_3_4) then for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[samplesleft + i].re
                                           else for i := 0 to fFFTSize_1_4 - 1 do fPreviousOutput[c][i]:= fFFT.Complex[fFFTSize_3_4 + i].re;
            for i := 0 to fFFTSize_3_4 -1 do
              if (samplesLeft < fFFTSize_3_4)
                then if i < samplesLeft
                  then Buf32i^[CurrentPos + (i * Channels)] := clip_32(Int64(Round(fFFT.Complex[i].re)) * 32768)
                  else
                else Buf32i^[CurrentPos + (i * Channels)] := clip_32(Int64(Round(fFFT.Complex[i].re)) * 32768);
            dec(samplesLeft,fFFTSize_3_4);
          end;
        end;
      end;
    end;
  end;
end;
(*** IDCEqualizer *************************************************************)
function TDCEqualizer.get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aEnabled := fEnabled;
end;

function TDCEqualizer.set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fEnabled := aEnabled;
end;

function TDCEqualizer.get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  aSeperate := fSeperate;
end;

function TDCEqualizer.set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
begin
  Result := S_OK;
  fSeperate := aSeperate;
end;

function TDCEqualizer.get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
begin
  Result := S_OK;
  aFFTSize := GetFFTSize;
end;

function TDCEqualizer.set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
begin
  Result := S_OK;
  SetFFTSize(aFFTSize);
end;

function TDCEqualizer.get_Band(aChannel: Byte; aIndex: Word; out aBand: ShortInt): HRESULT; stdcall;
begin
  Result := S_OK;
  aBand := GetBand(aChannel,aIndex);
end;

function TDCEqualizer.set_Band(aChannel: Byte; aIndex: Word; aBand: ShortInt): HRESULT; stdcall;
begin
  Result := S_OK;
  SetBand(aChannel,aIndex,aBand);
end;

end.
