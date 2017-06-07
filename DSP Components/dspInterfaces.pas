
    (*********************************************************************
     *  dspInterfaces.pas                                                *
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
  @abstract(Interface Declaration Unit of DSP Filters.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit dspInterfaces;

interface

uses
  Windows, dspConst;

const
  { IDCAmplify identifier GUID. }
  IID_IDCAmplify        : TGUID = '{3FB0116F-52EE-4286-BF3A-65C0055EAA45}';
  { IDCBandPass identifier GUID. }
  IID_IDCBandPass       : TGUID = '{0D608545-CEAE-4E19-A6B4-CE516D415C71}';
  { IDCChannelOrder identifier GUID. }
  IID_IDCChannelOrder   : TGUID = '{567017E8-EBC3-444A-BAAC-7BEA3841EF2E}';
  { IDCCompressor identifier GUID. }
  IID_IDCCompressor     : TGUID = '{E93CF724-B897-4A50-825B-AC8FFDCA937B}';
  { IDCDownMix identifier GUID. }
  IID_IDCDownMix        : TGUID = '{9496B84F-BC7B-4230-889D-1ADCC790D237}';
  { IDCDynamicAmplify identifier GUID. }
  IID_IDCDynamicAmplify : TGUID = '{0B60A7EC-A76F-4526-AE29-FF1FD8EA7DC3}';
  { IDCEchoDelay identifier GUID. }
  IID_IDCEchoDelay      : TGUID = '{F5803607-1B25-422C-B55A-22305FC40C37}';
  { IDCEqualizer identifier GUID. }
  IID_IDCEqualizer      : TGUID = '{14D4A709-77ED-459B-B1E9-E4E4C84261BD}';
  { IDCFlanger identifier GUID. }
  IID_IDCFlanger        : TGUID = '{EB6F78D4-A20D-4115-8924-89A3571DD587}';
  { IDCHighPass identifier GUID. }
  IID_IDCHighPass       : TGUID = '{614F092A-50E0-4BEF-9CD3-B390EB5958A8}';
  { IDCLowPass identifier GUID. }
  IID_IDCLowPass        : TGUID = '{FCAE49B0-005F-4F21-8B7F-555AB3196073}';
  { IDCNotch identifier GUID. }
  IID_IDCNotch          : TGUID = '{35D3996E-A312-4A8D-BEBA-05A26DBB2F34}';
  { IDCParametricEQ identifier GUID. }
  IID_IDCParametricEQ   : TGUID = '{A51C1B92-5C17-40FB-A1E2-685F329D97CF}';
  { IDCPhaseInvert identifier GUID. }
  IID_IDCPhaseInvert    : TGUID = '{0102BF81-AEFC-4ACC-9135-DAF0669151BD}';
  { IDCPhaser identifier GUID. }
  IID_IDCPhaser         : TGUID = '{0CFA4706-977A-4B8C-B12F-DDB29B873C3F}';
  { IDCPitchScale identifier GUID. }
  IID_IDCPitchScale     : TGUID = '{3484EA9C-4598-469C-9EDB-4AEF85D835A8}';
  { IDCPitchShift identifier GUID. }
  IID_IDCPitchShift     : TGUID = '{96979014-AD4E-49F9-A9DE-3BD3A55A4454}';
  { IDCSound3D identifier GUID. }
  IID_IDCSound3D        : TGUID = '{E83F7666-DD0A-4358-A0F0-75E1B8F52062}';
  { IDCTempo identifier GUID. }
  IID_IDCTempo          : TGUID = '{E25E423C-866D-49CD-B950-CE12A17ED26D}';
  { IDCTrebleEnhancer identifier GUID. }
  IID_IDCTrebleEnhancer : TGUID = '{45694F89-26E6-48EC-B2CD-22D7E74BD1C3}';
  { IDCTrueBass identifier GUID. }
  IID_IDCTrueBass       : TGUID = '{74EB17C2-ED23-4175-B5B7-26395030BF5D}';

type
  { IDCAmplify - Interface to Control an Instance of TDCAmplify from other
    programming Languages. }
  IDCAmplify = interface(IUnknown)
    ['{3FB0116F-52EE-4286-BF3A-65C0055EAA45}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    { Shows the Amplification for a Channel. }
    function get_Volume(aChannel: Byte; out aVolume: integer): HRESULT; stdcall;
    { Sets the Amplification for a Channel. Default Value is 10000, which means
      that no Amplification occours. A Value of 20000 raises the Amplification
      by 2. }
    function set_Volume(aChannel: Byte; aVolume: integer): HRESULT; stdcall;
  end;

  { IDCBandPass - Interface to Control an Instance of TDCBandPass from other
    programming Languages. }
  IDCBandPass = interface(IUnknown)
    ['{0D608545-CEAE-4E19-A6B4-CE516D415C71}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_CutoffLow(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    function set_CutoffLow(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
    function get_CutoffHigh(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    function set_CutoffHigh(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
  end;

  { IDCChannelOrder - Interface to Control an Instance of TDCChannelOrder from other
    programming Languages. }
  IDCChannelOrder = interface(IUnknown)
    ['{567017E8-EBC3-444A-BAAC-7BEA3841EF2E}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Order(aChannel : Byte; out aOrder : Byte): HRESULT; stdcall;
    function set_Order(aChannel : Byte; aOrder : Byte): HRESULT; stdcall;
  end;

  { IDCCompressor - Interface to Control an Instance of TDCCompressor from other
    programming Languages. }
  IDCCompressor = interface(IUnknown)
    ['{E93CF724-B897-4A50-825B-AC8FFDCA937B}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_AttackTime(out aAttackTime: Single): HRESULT; stdcall;
    function set_AttackTime(aAttackTime: Single): HRESULT; stdcall;
    function get_DecayTime(out aDecayTime: Single): HRESULT; stdcall;
    function set_DecayTime(aDecayTime: Single): HRESULT; stdcall;
    function get_ThresholdDB(out aThresholdDB: Single): HRESULT; stdcall;
    function set_ThresholdDB(aThresholdDB: Single): HRESULT; stdcall;
    function get_Ratio(out aRatio: Single): HRESULT; stdcall;
    function set_Ratio(aRatio: Single): HRESULT; stdcall;
    function get_GainDB(out aGainDB: Single): HRESULT; stdcall;
    function set_GainDB(aGainDB: Single): HRESULT; stdcall;
  end;

  { IDCDownMix - Interface to Control an Instance of TDCDownMix from other
    programming Languages. }
  IDCDownMix = interface(IUnknown)
    ['{9496B84F-BC7B-4230-889D-1ADCC790D237}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  { IDCDynamicAmplify - Interface to Control an Instance of TDCDynamicAmplify from other
    programming Languages. }
  IDCDynamicAmplify = interface(IUnknown)
    ['{0B60A7EC-A76F-4526-AE29-FF1FD8EA7DC3}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_AttackTime(out aAttackTime: Cardinal): HRESULT; stdcall;
    function set_AttackTime(aAttackTime: Cardinal): HRESULT; stdcall;
    function get_ReleaseTime(out aReleaseTime: Cardinal): HRESULT; stdcall;
    function set_ReleaseTime(aReleaseTime: Cardinal): HRESULT; stdcall;
    function get_MaxAmplification(out aMaxAmplification: Cardinal): HRESULT; stdcall;
    function set_MaxAmplification(aMaxAmplification: Cardinal): HRESULT; stdcall;
    function get_CurrentAmplification(out aCurrentAmplification: Single): HRESULT; stdcall;
  end;

  { IDCEchoDelay - Interface to Control an Instance of TDCEchoDelay from other
    programming Languages. }
  IDCEchoDelay = interface(IUnknown)
    ['{F5803607-1B25-422C-B55A-22305FC40C37}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_KillMain(out aKillMain: BOOL): HRESULT; stdcall;
    function set_KillMain(aKillMain: BOOL): HRESULT; stdcall;
    function get_NumDelays(out aNumDelays: Byte): HRESULT; stdcall;
    function set_NumDelays(aNumDelays: Byte): HRESULT; stdcall;
    function get_Highpass(out aHighpass: BOOL): HRESULT; stdcall;
    function set_Highpass(aHighpass: BOOL): HRESULT; stdcall;
    function get_DelayAmp(out aDelayAmp: WORD): HRESULT; stdcall;
    function set_DelayAmp(aDelayAmp: WORD): HRESULT; stdcall;
    function get_Delay(out aDelay: WORD): HRESULT; stdcall;
    function set_Delay(aDelay: WORD): HRESULT; stdcall;
  end;

  { IDCEqualizer - Interface to Control an Instance of TDCEqualizer from other
    programming Languages. }
  IDCEqualizer = interface(IUnknown)
    ['{14D4A709-77ED-459B-B1E9-E4E4C84261BD}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function get_Band(aChannel: Byte; aIndex: Word; out aBand: ShortInt): HRESULT; stdcall;
    function set_Band(aChannel: Byte; aIndex: Word; aBand: ShortInt): HRESULT; stdcall;
  end;

  { IDCFlanger - Interface to Control an Instance of TDCFlanger from other
    programming Languages. }
  IDCFlanger = interface(IUnknown)
    ['{EB6F78D4-A20D-4115-8924-89A3571DD587}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT; stdcall;
    function set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT; stdcall;
    function get_Delay(aChannel : Byte; out aDelay: Single): HRESULT; stdcall;
    function set_Delay(aChannel : Byte; aDelay: Single): HRESULT; stdcall;
    function get_PhaseInvert(aChannel: Byte; out aPhaseInvert: BOOL): HRESULT; stdcall;
    function set_PhaseInvert(aChannel: Byte; aPhaseInvert: BOOL): HRESULT; stdcall;
  end;

  { IDCHighPass - Interface to Control an Instance of TDCHighPass from other
    programming Languages. }
  IDCHighPass = interface(IUnknown)
    ['{614F092A-50E0-4BEF-9CD3-B390EB5958A8}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

  { IDCLowPass - Interface to Control an Instance of TDCLowPass from other
    programming Languages. }
  IDCLowPass = interface(IUnknown)
    ['{FCAE49B0-005F-4F21-8B7F-555AB3196073}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

  { IDCNotch - Interface to Control an Instance of TDCNotch from other
    programming Languages. }
  IDCNotch = interface(IUnknown)
    ['{35D3996E-A312-4A8D-BEBA-05A26DBB2F34}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

  { IDCParametricEQ - Interface to Control an Instance of TDCParametricEQ from other
    programming Languages. }
  IDCParametricEQ = interface(IUnknown)
    ['{A51C1B92-5C17-40FB-A1E2-685F329D97CF}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT; stdcall;
    function set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT; stdcall;
    function get_Gain(aChannel : Byte; out aGain: Single): HRESULT; stdcall;
    function set_Gain(aChannel : Byte; aGain: Single): HRESULT; stdcall;
    function get_Q(aChannel : Byte; out aQ: Single): HRESULT; stdcall;
    function set_Q(aChannel : Byte; aQ: Single): HRESULT; stdcall;
  end;

  { IDCPhaseInvert - Interface to Control an Instance of TDCPhaseInvert from other
    programming Languages. }
  IDCPhaseInvert = interface(IUnknown)
    ['{0102BF81-AEFC-4ACC-9135-DAF0669151BD}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Invert(aChannel : Byte; out aInvert : BOOL): HRESULT; stdcall;
    function set_Invert(aChannel : Byte; aInvert : BOOL): HRESULT; stdcall;
  end;

  { IDCPhaser - Interface to Control an Instance of TDCPhaser from other
    programming Languages. }
  IDCPhaser = interface(IUnknown)
    ['{0CFA4706-977A-4B8C-B12F-DDB29B873C3F}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_DryWetRatio(aChannel : Byte; out aDryWetRatio : Byte): HRESULT; stdcall;
    function set_DryWetRatio(aChannel : Byte; aDryWetRatio : Byte): HRESULT; stdcall;
    function get_Feedback(aChannel : Byte; out aFeedback : Byte): HRESULT; stdcall;
    function set_Feedback(aChannel : Byte; aFeedback : Byte): HRESULT; stdcall;
    function get_Stages(aChannel : Byte; out aStages : Byte): HRESULT; stdcall;
    function set_Stages(aChannel : Byte; aStages : Byte): HRESULT; stdcall;
    function get_Depth(aChannel : Byte; out aDepth : Byte): HRESULT; stdcall;
    function set_Depth(aChannel : Byte; aDepth : Byte): HRESULT; stdcall;
    function get_StartPhase(aChannel : Byte; out aStartPhase : Single): HRESULT; stdcall;
    function set_StartPhase(aChannel : Byte; aStartPhase : Single): HRESULT; stdcall;
    function get_Frequency(aChannel : Byte; out aFrequency : Single): HRESULT; stdcall;
    function set_Frequency(aChannel : Byte; aFrequency : Single): HRESULT; stdcall;
  end;

  { IDCPitchScale - Interface to Control an Instance of TDCPitchScale from other
    programming Languages. }
  IDCPitchScale = interface(IUnknown)
    ['{3484EA9C-4598-469C-9EDB-4AEF85D835A8}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Pitch(aChannel : Byte; out aPitch : WORD): HRESULT; stdcall;
    function set_Pitch(aChannel : Byte; aPitch : WORD): HRESULT; stdcall;
    function get_Quality(aChannel : Byte; out aQuality : Byte): HRESULT; stdcall;
    function set_Quality(aChannel : Byte; aQuality : Byte): HRESULT; stdcall;
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
  end;

  { IDCPitchShift - Interface to Control an Instance of TDCPitchShift from other
    programming Languages. }
  IDCPitchShift = interface(IUnknown)
    ['{96979014-AD4E-49F9-A9DE-3BD3A55A4454}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Pitch(out aPitch: Cardinal): HRESULT; stdcall;
    function set_Pitch(aPitch: Cardinal): HRESULT; stdcall;
  end;

  { IDCSound3D - Interface to Control an Instance of TDCSound3D from other
    programming Languages. }
  IDCSound3D = interface(IUnknown)
    ['{E83F7666-DD0A-4358-A0F0-75E1B8F52062}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Volume(out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aVolume: Word): HRESULT; stdcall;
  end;

  { IDCTempo - Interface to Control an Instance of TDCTempo from other
    programming Languages. }
  IDCTempo = interface(IUnknown)
    ['{E25E423C-866D-49CD-B950-CE12A17ED26D}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Tempo(out aTempo: integer): HRESULT; stdcall;
    function set_Tempo(aTempo: integer): HRESULT; stdcall;
  end;

  { IDCTrebleEnhancer - Interface to Control an Instance of TDCTrebleEnhancer from other
    programming Languages. }
  IDCTrebleEnhancer = interface(IUnknown)
    ['{45694F89-26E6-48EC-B2CD-22D7E74BD1C3}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
    function get_Frequency(out aFrequency: integer): HRESULT; stdcall;
    function set_Frequency(aFrequency: integer): HRESULT; stdcall;
  end;

  { IDCTrueBass - Interface to Control an Instance of TDCTrueBass from other
    programming Languages. }
  IDCTrueBass = interface(IUnknown)
    ['{74EB17C2-ED23-4175-B5B7-26395030BF5D}']
    { Shows if the Filter is Enabled or Disabled. }
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    { Enables or Disables the Filter. }
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    { Shows if seperate DSP is Enabled or Disabled. }
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    { Enables or Disables seperate DSP. }
    function get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
    function get_Frequency(out aFrequency: integer): HRESULT; stdcall;
    function set_Frequency(aFrequency: integer): HRESULT; stdcall;
  end;

implementation

end.
