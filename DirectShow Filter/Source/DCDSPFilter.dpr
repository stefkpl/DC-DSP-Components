
    (*********************************************************************
     *  DCDSPFilter.dpr                                                  *
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

library DCDSPFilter;
{$E ax}
{$I Compiler.inc}
{$R Resource.res}
//{$R Resource.res}

uses
  FastMM4,
  FastMove,
  BaseClass,
  DSFilter in 'DSFilter.pas'
{$IFDEF WITH_CONFIG}
  ,
  Config in 'Config.pas'
{$ENDIF}
{$IFDEF WITH_TRAYICON}
  ,
  formPropertyPage in 'Property Pages\formPropertyPage.pas' {frmPropertyPage}
{$ENDIF}
{$IFDEF WITH_PROPERTYPAGES}
  ,
  formDMOChorus in 'Property Pages\formDMOChorus.pas' {frmDMOChorus},
  formDMOCompressor in 'Property Pages\formDMOCompressor.pas' {frmDMOCompressor},
  formDMODistortion in 'Property Pages\formDMODistortion.pas' {frmDMODistortion},
  formDMOEcho in 'Property Pages\formDMOEcho.pas' {frmDMOEcho},
  formDMOFlanger in 'Property Pages\formDMOFlanger.pas' {frmDMOFlanger},
  formDMOGargle in 'Property Pages\formDMOGargle.pas' {frmDMOGargle},
  formDMOI3DL2Reverb in 'Property Pages\formDMOI3DL2Reverb.pas' {frmDMOI3DL2Reverb},
  formDMOParamEQ in 'Property Pages\formDMOParamEQ.pas' {frmDMOParamEQ},
  formDMOWavesReverb in 'Property Pages\formDMOWavesReverb.pas' {frmDMOWavesReverb},
  formDSPAmplify in 'Property Pages\formDSPAmplify.pas' {frmDSPAmplify},
  formDSPBandPass in 'Property Pages\formDSPBandPass.pas' {frmDSPBandPass},
  formDSPChannelOrder in 'Property Pages\formDSPChannelOrder.pas' {frmDSPChannelOrder},
  formDSPCompressor in 'Property Pages\formDSPCompressor.pas' {frmDSPCompressor},
  formDSPDownMix in 'Property Pages\formDSPDownMix.pas' {frmDSPDownMix},
  formDSPDynamicAmplify in 'Property Pages\formDSPDynamicAmplify.pas' {frmDSPDynamicAmplify},
  formDSPEchoDelay in 'Property Pages\formDSPEchoDelay.pas' {frmDSPEchoDelay},
  formDSPEqualizer in 'Property Pages\formDSPEqualizer.pas' {frmDSPEqualizer},
  formDSPFlanger in 'Property Pages\formDSPFlanger.pas' {frmDSPFlanger},
  formDSPHighPass in 'Property Pages\formDSPHighPass.pas' {frmDSPHighPass},
  formDSPLowPass in 'Property Pages\formDSPLowPass.pas' {frmDSPLowPass},
  formDSPNotch in 'Property Pages\formDSPNotch.pas' {frmDSPNotch},
  formDSPParametricEQ in 'Property Pages\formDSPParametricEQ.pas' {frmDSPParametricEQ},
  formDSPPhaseInvert in 'Property Pages\formDSPPhaseInvert.pas' {frmDSPPhaseInvert},
  formDSPPhaser in 'Property Pages\formDSPPhaser.pas' {frmDSPPhaser},
  formDSPPitchScale in 'Property Pages\formDSPPitchScale.pas' {frmDSPPitchScale},
  formDSPPitchShift in 'Property Pages\formDSPPitchShift.pas' {frmDSPPitchShift},
  formDSPSound3D in 'Property Pages\formDSPSound3D.pas' {frmDSPSound3D},
  formDSPTempo in 'Property Pages\formDSPTempo.pas' {frmDSPTempo},
  formDSPTrebleEnhancer in 'Property Pages\formDSPTrebleEnhancer.pas' {frmDSPTrebleEnhancer},
  formDSPTrueBass in 'Property Pages\formDSPTrueBass.pas' {frmDSPTrueBass},
  PropAbout in 'Property Pages\PropAbout.pas' {FormPropAbout},
  PropDSP in 'Property Pages\PropDSP.pas' {FormPropDSP},
  PropOptions in 'Property Pages\PropOptions.pas' {FormPropOptions},
  PropWinamp in 'Property Pages\PropWinamp.pas' {FormPropWinamp},
  PropWinampVis in 'Property Pages\PropWinampVis.pas' {FormPropWinampVis},
  dspInterfaces in '..\..\DSP Components\dspInterfaces.pas',
  dmoInterfaces in '..\..\DSP Components\dmoInterfaces.pas'
{$ENDIF}
  ;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer
{$IFDEF WITH_CONFIG}
  ,configure
{$ENDIF}
  ;

begin

end.

