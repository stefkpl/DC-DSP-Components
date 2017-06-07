
    (*********************************************************************
     *  dspConst.pas                                                     *
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
  @abstract(Contains Constants and Types that are used by the DSP Routines.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspConst;

interface

uses
  Windows;

const
  { Defines the Maximum Number of Channels that will be used for DSP Processing.<br>
    default = 10 }
  MaxChannels = 10;
  { Used by the Echo/Delay Filter to specify the Maximum Echos. }
  MaxDelays = 10;
  { Used by the Inverse 2 Point FFT to Scaledown the Samples. }
  ScaleFFT2 = 1 / 2;
  { Used by the Inverse 4 Point FFT to Scaledown the Samples. }
  ScaleFFT4 = ScaleFFT2 / 2;
  { Used by the Inverse 8 Point FFT to Scaledown the Samples. }
  ScaleFFT8 = ScaleFFT4 / 2;
  { Used by the Inverse 16 Point FFT to Scaledown the Samples. }
  ScaleFFT16 = ScaleFFT8 / 2;
  { Used by the Inverse 32 Point FFT to Scaledown the Samples. }
  ScaleFFT32 = ScaleFFT16 / 2;
  { Used by the Inverse 64 Point FFT to Scaledown the Samples. }
  ScaleFFT64 = ScaleFFT32 / 2;
  { Used by the Inverse 128 Point FFT to Scaledown the Samples. }
  ScaleFFT128 = ScaleFFT64 / 2;
  { Used by the Inverse 256 Point FFT to Scaledown the Samples. }
  ScaleFFT256 = ScaleFFT128 / 2;
  { Used by the Inverse 512 Point FFT to Scaledown the Samples. }
  ScaleFFT512 = ScaleFFT256 / 2;
  { Used by the Inverse 1024 Point FFT to Scaledown the Samples. }
  ScaleFFT1024 = ScaleFFT512 / 2;
  { Used by the Inverse 2048 Point FFT to Scaledown the Samples. }
  ScaleFFT2048 = ScaleFFT1024 / 2;
  { Used by the Inverse 4096 Point FFT to Scaledown the Samples. }
  ScaleFFT4096 = ScaleFFT2048 / 2;
  { Used by the Inverse 8192 Point FFT to Scaledown the Samples. }
  ScaleFFT8192 = ScaleFFT4096 / 2;

  { Used by DSP Filters to Set the default Split Size. This is needed
    when a large Buffer arrives and the Application would't respond until the
    DSP is done. If the SampleSize of an Filter is Set to Value > 0 then the
    Buffer will be splitted at this amount of Samples and processed until
    all Samples are processed. After every Process, Application.ProcessMessages
    is called to make the Application respond to Windows.
    DefaultSampleSize is just a default Value. The SampleSize can be changed
    on every Filter seperate. }
  DefaultSampleSize = 8192;

  { The maximum number of Samples that the Visual Output Buffer contains. }
  MaxVisualSamples = 8192;

  { Used internal by TDCPhaser. Informs about how many samples are processed
    before the LFO Value is computed again. }
  PhaserLFOSkipSamples = 20;
  { Used internal by TDCPhaser. Used to compute the LFO Value. }
  PhaserLFOShape = 4;

  {@exclude}
  Two_Pi = 2 * Pi;
  {@exclude}
  Four_Pi = 2 * Two_Pi;

type
  { Record that stores Information about an Audiostream. }
  PDSStream = ^TDSStream;
  TDSStream = record
    Size,
    Frequency,
    Channels,
    Bits : integer;
    Float : BOOL;
    SPDIF : BOOL;
    DTS : BOOL;
  end;

  { Specifys the BitRate used by TDCBitrateConverter. }
  TDCBitRate = (
    br8BitInteger, br16BitInteger, br24BitInteger, br32BitInteger, br32BitFloat
  );

  { Specifys the WindowMode used by GetWindowingValue. }
  TWindowMode = (
    wmRectangular, wmHanning, wmBartlett, wmHamming, wmGaussian, wmBlackman, wmBlackmanHarris
  );

  { Used by TDCFFT to setup the FFT Size. }
  TDCFFTSize = (
    fts2, fts4, fts8, fts16, fts32, fts64, fts128, fts256,
    fts512, fts1024, fts2048, fts4096, fts8192
  );

  { Pointer to TComplex. }
  PComplex = ^TComplex;
  { Used as a Buffer Sample by TDCFFT to setup real and imag Values. }
  TComplex = record
    re,
    im : Single;
  end;
  { Pointer to an array of 8192 TComplex Values. }
  PComplexArray = ^TComplexArray;
  { Array of 8192 TComplex Values. }
  TComplexArray = array[0..8191] of TComplex;
  { Array of 8192 TComplex Values. }

  { Used internal by the 3DNow FFT. }
  PComplex3DNow = ^TComplex3DNow;

  { Used internal by the 3DNow FFT. }
  TComplex3DNow = record
    re1,re2,
    im1,im2 : Single;
  end;
  { Used internal by the 3DNow FFT. }
  PComplexArray3DNow = ^TComplexArray3DNow;
  { Used internal by the 3DNow FFT. }
  TComplexArray3DNow = array[0..255] of TComplex3DNow;

  { Used internal by the SSE FFT. }
  PComplexSSE = ^TComplexSSE;
  { Used internal by the SSE FFT. }
  TComplexSSE = record
    re1,re2,re3,re4,
    im1,im2,im3,im4 : Single;
  end;
  { Used internal by the SSE FFT. }
  PComplexArraySSE = ^TComplexArraySSE;
  { Used internal by the SSE FFT. }
  TComplexArraySSE = array[0..255] of TComplexSSE;

  { Indicates a 24Bit Sample. Use Cvt24BitTo32 and Cvt32BitTo24 to convert those Samples to 32Bit and back to 24Bit }
  T24BitSample = record
    a0 : Byte;
    a1 : Byte;
    a2 : ShortInt;
  end;
  { Pointer to a 24Bit Sample. }
  P24BitSample = ^T24BitSample;

  { Used internal by the FFT to setup Reverse Bins Variables. Pointer to 16 Bit WORD Array. }
  PWORDArray = ^TWORDArray;
  { Used internal by the FFT to setup Reverse Bins Variables. 16 Bit WORD Array. }
  TWORDArray = array [0..255] of WORD;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to unsigned 8 Bit Integer. }
  PByteArray = ^TByteArray;
  { Used internal by the DSP Components to access the Audio Buffer. unsigned 8 Bit Integer. }
  TByteArray = array [0..255] of Byte;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to 16 Bit Integer. }
  PSmallIntArray = ^TSmallIntArray;
  { Used internal by the DSP Components to access the Audio Buffer. 16 Bit Integer. }
  TSmallIntArray = array [0..255] of SmallInt;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to 24 Bit Integer. }
  PInteger24Array = ^TInteger24Array;
  { Used internal by the DSP Components to access the Audio Buffer. 24 Bit Integer. }
  TInteger24Array = array[0..255] of T24BitSample;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to 32 Bit Integer. }
  PIntegerArray = ^TIntegerArray;
  { Used internal by the DSP Components to access the Audio Buffer. 32 Bit Integer. }
  TIntegerArray = array[0..255] of integer;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to 32 Bit Float. }
  PFloatArray = ^TFloatArray;
  { Used internal by the DSP Components to access the Audio Buffer. 32Bit Float. }
  TFloatArray = array [0..255] of Single;
  { Used internal by the DSP Components to access the Audio Buffer. Pointer to 64 Bit Float. }
  PDoubleArray = ^TDoubleArray;
  { Used internal by the DSP Components to access the Audio Buffer. 64Bit Float. }
  TDoubleArray = array [0..255] of Double;


  { Used internal by the Equalizer Component to setup Equalizer Bands and Amplification. }
  TEqualizerBands = array[0..MaxChannels -1] of array[0..8191] of Single;
  { Used internal by the Equalizer Component to temporary store previous processed Samples. }
  TEqualizerPrevOut = array[0..MaxChannels -1] of array[0..2047] of Single;
  { Used internal by the Lowpass, Highpass, ... Components to temporary store previous processed Samples. }
  TPass = array[0..2] of array[0..MaxChannels -1] of Single;
  { Used internal by the Flanger Component to temporary store previous processed Samples. }
  TFlangerPrevOut = array[0..MaxChannels -1] of array[0..100000] of Single;

  { Used internal by ChannelOrder to setup the Channel Order. }
  TChannelOrder = array[0..MaxChannels -1] of Byte;

  { Used internal by the TDCWaveform and TDCSpectrum Class. Pointer to TVisualBuffer. }
  PVisualBuffer = ^TVisualBuffer;
  { Used internal by the TDCWaveform and TDCSpectrum Class. MaxVisualSamples(8192) * (MaxChannels(10)-1) Array of integer. }
  TVisualBuffer = array[0..MaxChannels -1] of array[0..MaxVisualSamples -1] of Integer;
  { Used internal by the TDCWaveform and TDCSpectrum Class to Send an Event that the Visual Buffer has been Processed. }
  TDCVisualNotifyEvent = procedure(Sender : TObject; Data : PVisualBuffer; MinY, MaxY, NumSamples, Channels : integer) of object;

implementation

end.
