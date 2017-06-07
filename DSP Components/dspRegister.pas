
    (*********************************************************************
     *  dspRegister.pas                                                  *
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

unit dspRegister;

interface

uses
  Classes,
  dmoChorus,
  dmoCompressor,
  dmoDistortion,
  dmoEcho,
  dmoFlanger,
  dmoGargle,
  dmoI3DL2Reverb,
  dmoParamEQ,
  dmoWavesReverb,
  dspCompressor,
  dspConst,
  dspAmplify,
  dspBandpass,
  dspBitrateConverter,
  dspChannelOrder,
  dspDynamicAmplify,
  dspDownMix,
  dspEchoDelay,
  dspEqualizer,
  dspFastFourier,
  dspFlanger,
  dspHighpass,
  dspLowpass,
  dspNotch,
  dspPhaser,
  dspPhaseInvert,
  dspPitchScale,
  dspPitchShift,
  dspParametricEQ,
  dspReverse,
  dspSound3D,
  dspTempo,
  dspTrebleEnhancer,
  dspTrueBass,
  visSpectrum,
  visWaveform,
  waDSPWrapper,
  waDSPThreadedWrapper,
  waVisualWrapper;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DC-DSP Components', [
    TDCAmplify, TDCBandPass, TDCBitrateConverter, TDCChannelOrder, TDCCompressor,
    TDCDownMix, TDCDynamicAmplify, TDCEchoDelay, TDCEqualizer, TDCFFT, TDCFlanger,
    TDCHighpass, TDCLowpass, TDCNotch, TDCParametricEQ, TDCPhaseInvert, TDCPhaser,
    TDCPitchScale, TDCPitchShift, TDCReverse, TDCSound3D, TDCTempo, TDCTrebleEnhancer,
    TDCTrueBass
  ]);

  RegisterComponents('DC-DMO Wrappers', [
    TDCDMOChorus, TDCDMOCompressor, TDCDMODistortion, TDCDMOEcho, TDCDMOFlanger,
    TDCDMOGargle, TDCDMOI3DL2Reverb, TDCDMOParamEQ, TDCDMOWavesReverb
  ]);

  RegisterComponents('DC-DSP Tools', [
    TDCSpectrum, TDCWaveform, TDCWADSPWrapper, TDCWAVisualWrapper,
    TDCWADSPThreadedWrapper
  ]);
end;

end.
