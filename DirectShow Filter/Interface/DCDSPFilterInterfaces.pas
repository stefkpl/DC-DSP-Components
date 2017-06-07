
    (*********************************************************************
     *  DCDSPFilterInterfaces.pas                                        *
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
  @abstract(Interfaces to Control DC-DSP Filter.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Jul 07, 2003)
}

unit DCDSPFilterInterfaces;

interface

uses
  Windows, Classes, DirectSound;

const
  CLSID_DCDSPFilter     : TGUID = '{B38C58A0-1809-11D6-A458-EDAE78F1DF12}';
  IID_DCDSPFilter       : TGUID = '{BD78EF46-1809-11D6-A458-EDAE78F1DF12}';
  IID_DCDSPFilterVisual : TGUID = '{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}';

  IID_IDCAmplify        : TGUID = '{3FB0116F-52EE-4286-BF3A-65C0055EAA45}';
  IID_IDCBandPass       : TGUID = '{0D608545-CEAE-4E19-A6B4-CE516D415C71}';
  IID_IDCChannelOrder   : TGUID = '{567017E8-EBC3-444A-BAAC-7BEA3841EF2E}';
  IID_IDCCompressor     : TGUID = '{E93CF724-B897-4A50-825B-AC8FFDCA937B}';
  IID_IDCDownMix        : TGUID = '{9496B84F-BC7B-4230-889D-1ADCC790D237}';
  IID_IDCDynamicAmplify : TGUID = '{0B60A7EC-A76F-4526-AE29-FF1FD8EA7DC3}';
  IID_IDCEchoDelay      : TGUID = '{F5803607-1B25-422C-B55A-22305FC40C37}';
  IID_IDCEqualizer      : TGUID = '{14D4A709-77ED-459B-B1E9-E4E4C84261BD}';
  IID_IDCFlanger        : TGUID = '{EB6F78D4-A20D-4115-8924-89A3571DD587}';
  IID_IDCHighPass       : TGUID = '{614F092A-50E0-4BEF-9CD3-B390EB5958A8}';
  IID_IDCLowPass        : TGUID = '{FCAE49B0-005F-4F21-8B7F-555AB3196073}';
  IID_IDCNotch          : TGUID = '{35D3996E-A312-4A8D-BEBA-05A26DBB2F34}';
  IID_IDCParametricEQ   : TGUID = '{A51C1B92-5C17-40FB-A1E2-685F329D97CF}';
  IID_IDCPhaseInvert    : TGUID = '{0102BF81-AEFC-4ACC-9135-DAF0669151BD}';
  IID_IDCPhaser         : TGUID = '{0CFA4706-977A-4B8C-B12F-DDB29B873C3F}';
  IID_IDCPitchScale     : TGUID = '{3484EA9C-4598-469C-9EDB-4AEF85D835A8}';
  IID_IDCPitchShift     : TGUID = '{96979014-AD4E-49F9-A9DE-3BD3A55A4454}';
  IID_IDCSound3D        : TGUID = '{E83F7666-DD0A-4358-A0F0-75E1B8F52062}';
  IID_IDCTempo          : TGUID = '{E25E423C-866D-49CD-B950-CE12A17ED26D}';
  IID_IDCTrebleEnhancer : TGUID = '{45694F89-26E6-48EC-B2CD-22D7E74BD1C3}';
  IID_IDCTrueBass       : TGUID = '{74EB17C2-ED23-4175-B5B7-26395030BF5D}';

  IID_IDCDMOChorus      : TGUID = '{2F99E23A-5D6E-4B51-8269-49C46ADC745C}';
  IID_IDCDMOCompressor  : TGUID = '{A5BA9E24-E017-48D2-9446-E42E5E1C9DCB}';
  IID_IDCDMODistortion  : TGUID = '{A09AE606-B22B-4C1B-8ACF-45285BC30925}';
  IID_IDCDMOEcho        : TGUID = '{22F654FD-7829-4BB8-9380-14EC6279D6D8}';
  IID_IDCDMOFlanger     : TGUID = '{50CEC6C5-0A67-4BBA-BEC5-A59D86DFC12B}';
  IID_IDCDMOGargle      : TGUID = '{0F5FDB37-E3EA-4556-98E4-790345E28D8D}';
  IID_IDCDMOI3DL2Reverb : TGUID = '{22D00724-2D31-47E8-A5C4-3FF732224C9B}';
  IID_IDCDMOParamEQ     : TGUID = '{414E947F-0327-4D2F-B3CC-5AAEAB8EC4F2}';
  IID_IDCDMOWavesReverb : TGUID = '{CB0809E2-8436-475A-B37E-F21BACDD5014}';

type
  TDCFilterItem = Pointer;

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

  TDCFilterType = (
    ftNone, ftAmplify, ftBandPass, ftChannelOrder, ftCompressor, ftDownMix, ftDynamicAmplify,
    ftEchoDelay, ftEqualizer, ftFlanger, ftHighPass, ftLowPass, ftNotch, ftPhaseInvert,
    ftPhaser, ftPitchScale, ftPitchShift, ftSound3D, ftTempo, ftTrebleEnhancer, ftTrueBass,
    ftDMOChorus, ftDMOCompressor, ftDMODistortion, ftDMOEcho, ftDMOFlanger, ftDMOGargle,
    ftDMOI3DL2Reverb, ftDMOParamEQ, ftDMOWavesReverb, ftParametricEQ
  );

  TDCBitRate = (
    br8BitInteger, br16BitInteger, br24BitInteger, br32BitInteger, br32BitFloat
  );

  TDCFFTSize = (
    fts2, fts4, fts8, fts16, fts32, fts64, fts128, fts256,
    fts512, fts1024, fts2048, fts4096, fts8192
  );

  IDCDSPFilterPCMCallBack = interface(IUnknown)
    ['{3971C5D4-1FDA-45C1-9131-C817326A4348}']
    function PCMDataCB(Buffer : Pointer; Length : integer; out NewSize : integer; Stream : PDSStream) : HRESULT; stdcall;
    function MediaTypeChanged(Stream : PDSStream) : HRESULT; stdcall;
    function Flush : HRESULT; stdcall;
  end;

  IDCDSPFilterVisualInterface = interface(IUnknown)
  ['{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}']
    function get_VISafterDSP(out AfterDSP : BOOL): HRESULT; stdcall;
    function set_VISafterDSP(AfterDSP : BOOL): HRESULT; stdcall;
    function get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream) : HRESULT; stdcall;
  end;

  IDCDSPFilterInterface = interface(IUnknown)
    ['{BD78EF46-1809-11D6-A458-EDAE78F1DF12}']
    // Callbacks for PCM Data
    function set_CallBackPCM(Callback : IDCDSPFilterPCMCallBack): HRESULT; stdcall;
    function set_PCMDataBeforeMainDSP(Before : BOOL): HRESULT; stdcall;
    // Winamp 2.x DSP Plugins
    function set_DSPPlugin(WindowHandle : hWnd; Path : PChar): HRESULT; stdcall;
    function get_DSPPlugin(out Path : PChar): HRESULT; stdcall;
    function get_DSPCount(out Count : integer): HRESULT; stdcall;
    function get_DSPDescription(out Description : PChar): HRESULT; stdcall;
    function get_DSPSubDescription(index : integer; out Description : PChar): HRESULT; stdcall;
    function set_DSPSubPlugin(index : integer): HRESULT; stdcall;
    function get_DSPSubPlugin(out index : integer): HRESULT; stdcall;
    function set_ShowConfig: HRESULT; stdcall;
    function set_UnloadDSPPlugin: HRESULT; stdcall;
    function get_EnableDSPPlug(out Enable : BOOL): HRESULT; stdcall;
    function set_EnableDSPPlug(Enable : BOOL): HRESULT; stdcall;
    function set_PluginOwnerWindow(Window : hWnd): HRESULT; stdcall;
    // Winamp Vis Plugins
    function get_WinampVisInterval(out Interval : integer) : HRESULT; stdcall;
    function set_WinampVisInterval(Interval : integer) : HRESULT; stdcall;
    function get_WinampVisPlugin(out Plugin : PChar; out Index : integer) : HRESULT; stdcall;
    function set_WinampVisPlugin(Plugin : PChar; Index : integer) : HRESULT; stdcall;
    function get_WinampVisAutostart(out Autostart : BOOL): HRESULT; stdcall;
    function set_WinampVisAutostart(Autostart : BOOL): HRESULT; stdcall;
    function set_StopWinampVisPlugin : HRESULT; stdcall;
    // Delay functions for delaying the Audio Stream through Timestamps
    function get_EnableDelay(out Enabled : BOOL): HRESULT; stdcall;
    function set_EnableDelay(Enabled : BOOL): HRESULT; stdcall;
    function get_Delay(out Delay : integer): HRESULT; stdcall;
    function set_Delay(Delay : integer): HRESULT; stdcall;
    // functions to work with DSP Filters
    function get_FilterCount(out Count : integer): HRESULT; stdcall;
    function get_FilterType(Index : integer; out FilterType : TDCFilterType): HRESULT; stdcall;
    function set_AddFilter(Index : integer; FilterType : TDCFilterType): HRESULT; stdcall;
    function get_FilterName(Index : integer; out Name : PChar): HRESULT; stdcall;
    function get_WindowShown(Index : integer; out Shown : BOOL): HRESULT; stdcall;
    function set_WindowShown(Index : integer; Shown : BOOL): HRESULT; stdcall;
    function set_DeleteFilter(Index : integer): HRESULT; stdcall;
    function get_EnableFilter(Index : integer; out Enabled : BOOL): HRESULT; stdcall;
    function set_EnableFilter(Index : integer; Enabled : BOOL): HRESULT; stdcall;
    function set_RemoveAllFilters : HRESULT; stdcall;
    function set_MoveFilter(FromIndex : integer; ToIndex : integer): HRESULT; stdcall;
    function set_ResetShownWindows : HRESULT; stdcall;
    function get_FilterClass(Index : integer; out Filter : TComponent): HRESULT; stdcall;
    function get_FilterInterface(Index : integer; out Intf): HRESULT; stdcall;
    function get_FilterItem(Index : integer; out Item : TDCFilterItem): HRESULT; stdcall; // for internal use only!
    function get_PresetCount(out Count : integer) : HRESULT; stdcall;
    function get_PresetExist(Name : PChar; out Exist : BOOL) : HRESULT; stdcall;
    function get_PresetName(Index : integer; out Name : PChar) : HRESULT; stdcall;
    function set_LoadPreset(Name : PChar) : HRESULT; stdcall;
    function set_SavePreset(Name : PChar) : HRESULT; stdcall;
    function set_DeletePreset(Name : PChar) : HRESULT; stdcall;
    // Stream Switching
    function get_EnableStreamSwitching(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamSwitching(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableStreamSwitchingInterface(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamSwitchingInterface(Enable : BOOL) : HRESULT; stdcall;
    function get_ForceStreamSwitchingDisconnect(out Enable : BOOL) : HRESULT; stdcall;
    function set_ForceStreamSwitchingDisconnect(Enable : BOOL) : HRESULT; stdcall;
    function get_ForceStreamSwitchingStopFilter(out Enable : BOOL) : HRESULT; stdcall;
    function set_ForceStreamSwitchingStopFilter(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableStreamLimitInstance(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableStreamLimitInstance(Enable : BOOL) : HRESULT; stdcall;
    // Bitrate Conversion
    function set_EnableBitrateConversion(Enable : BOOL) : HRESULT; stdcall;
    function get_EnableBitrateConversion(out Enable : BOOL) : HRESULT; stdcall;
    function set_EnableBitrateConversionBeforeDSP(Before : BOOL) : HRESULT; stdcall;
    function get_EnableBitrateConversionBeforeDSP(out Before : BOOL) : HRESULT; stdcall;
    function set_BitrateConversionBits(Bits : TDCBitRate) : HRESULT; stdcall;
    function get_BitrateConversionBits(out Bits : TDCBitRate) : HRESULT; stdcall;
    // Misc Settings
    function get_StreamInfo(out Stream : PDSStream): HRESULT; stdcall;
    function set_DisableSaving(Disable : BOOL) : HRESULT; stdcall;
    function get_FilterVersion(out Version : integer) : HRESULT; stdcall;
    function get_Instance(out Instance : integer) : HRESULT; stdcall;
    function get_CPUUsage(out Usage : Double) : HRESULT; stdcall;
    function get_EnablePropertyPage(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnablePropertyPage(Enable: BOOL) : HRESULT; stdcall;
    function get_TrayiconVisible(out Visible: BOOL) : HRESULT; stdcall;
    function set_TrayiconVisible(Visible: BOOL) : HRESULT; stdcall;
    function get_EnableBalloonHint(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableBalloonHint(Enable: BOOL) : HRESULT; stdcall;
    function get_EnableROT(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableROT(Enable: BOOL) : HRESULT; stdcall;
    function get_EnableVisualBuffering(out Enable: BOOL) : HRESULT; stdcall;
    function set_EnableVisualBuffering(Enable: BOOL) : HRESULT; stdcall;
  end;

  IDCAmplify = interface(IUnknown)
    ['{3FB0116F-52EE-4286-BF3A-65C0055EAA45}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Volume(aChannel: Byte; out aVolume: integer): HRESULT; stdcall;
    function set_Volume(aChannel: Byte; aVolume: integer): HRESULT; stdcall;
  end;

  IDCBandPass = interface(IUnknown)
    ['{0D608545-CEAE-4E19-A6B4-CE516D415C71}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_CutoffLow(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    function set_CutoffLow(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
    function get_CutoffHigh(aChannel : Byte; out aCutoff : Single): HRESULT; stdcall;
    function set_CutoffHigh(aChannel : Byte; aCutoff : Single): HRESULT; stdcall;
  end;

  IDCChannelOrder = interface(IUnknown)
    ['{567017E8-EBC3-444A-BAAC-7BEA3841EF2E}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Order(aChannel : Byte; out aOrder : Byte): HRESULT; stdcall;
    function set_Order(aChannel : Byte; aOrder : Byte): HRESULT; stdcall;
  end;

  IDCCompressor = interface(IUnknown)
    ['{E93CF724-B897-4A50-825B-AC8FFDCA937B}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
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

  IDCDownMix = interface(IUnknown)
    ['{9496B84F-BC7B-4230-889D-1ADCC790D237}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDynamicAmplify = interface(IUnknown)
    ['{0B60A7EC-A76F-4526-AE29-FF1FD8EA7DC3}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_AttackTime(out aAttackTime: Cardinal): HRESULT; stdcall;
    function set_AttackTime(aAttackTime: Cardinal): HRESULT; stdcall;
    function get_ReleaseTime(out aReleaseTime: Cardinal): HRESULT; stdcall;
    function set_ReleaseTime(aReleaseTime: Cardinal): HRESULT; stdcall;
    function get_MaxAmplification(out aMaxAmplification: Cardinal): HRESULT; stdcall;
    function set_MaxAmplification(aMaxAmplification: Cardinal): HRESULT; stdcall;
    function get_CurrentAmplification(out aCurrentAmplification: Single): HRESULT; stdcall;
  end;

  IDCEchoDelay = interface(IUnknown)
    ['{F5803607-1B25-422C-B55A-22305FC40C37}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
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

  IDCEqualizer = interface(IUnknown)
    ['{14D4A709-77ED-459B-B1E9-E4E4C84261BD}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function get_Band(aChannel: Byte; aIndex: Word; out aBand: ShortInt): HRESULT; stdcall;
    function set_Band(aChannel: Byte; aIndex: Word; aBand: ShortInt): HRESULT; stdcall;
  end;

  IDCFlanger = interface(IUnknown)
    ['{EB6F78D4-A20D-4115-8924-89A3571DD587}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Frequency(aChannel : Byte; out aFrequency: Single): HRESULT; stdcall;
    function set_Frequency(aChannel : Byte; aFrequency: Single): HRESULT; stdcall;
    function get_Delay(aChannel : Byte; out aDelay: Single): HRESULT; stdcall;
    function set_Delay(aChannel : Byte; aDelay: Single): HRESULT; stdcall;
    function get_PhaseInvert(aChannel: Byte; out aPhaseInvert: BOOL): HRESULT; stdcall;
    function set_PhaseInvert(aChannel: Byte; aPhaseInvert: BOOL): HRESULT; stdcall;
  end;

  IDCHighPass = interface(IUnknown)
    ['{614F092A-50E0-4BEF-9CD3-B390EB5958A8}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

  IDCLowPass = interface(IUnknown)
    ['{FCAE49B0-005F-4F21-8B7F-555AB3196073}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

  IDCNotch = interface(IUnknown)
    ['{35D3996E-A312-4A8D-BEBA-05A26DBB2F34}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Cutoff(aChannel : Byte; out aCutoff : Cardinal): HRESULT; stdcall;
    function set_Cutoff(aChannel : Byte; aCutoff : Cardinal): HRESULT; stdcall;
  end;

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

  IDCPhaseInvert = interface(IUnknown)
    ['{0102BF81-AEFC-4ACC-9135-DAF0669151BD}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Invert(aChannel : Byte; out aInvert : BOOL): HRESULT; stdcall;
    function set_Invert(aChannel : Byte; aInvert : BOOL): HRESULT; stdcall;
  end;

  IDCPhaser = interface(IUnknown)
    ['{0CFA4706-977A-4B8C-B12F-DDB29B873C3F}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
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

  IDCPitchScale = interface(IUnknown)
    ['{3484EA9C-4598-469C-9EDB-4AEF85D835A8}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Pitch(aChannel : Byte; out aPitch : WORD): HRESULT; stdcall;
    function set_Pitch(aChannel : Byte; aPitch : WORD): HRESULT; stdcall;
    function get_Quality(aChannel : Byte; out aQuality : Byte): HRESULT; stdcall;
    function set_Quality(aChannel : Byte; aQuality : Byte): HRESULT; stdcall;
    function get_FFTSize(out aFFTSize : TDCFFTSize): HRESULT; stdcall;
    function set_FFTSize(aFFTSize : TDCFFTSize): HRESULT; stdcall;
  end;

  IDCPitchShift = interface(IUnknown)
    ['{96979014-AD4E-49F9-A9DE-3BD3A55A4454}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Pitch(out aPitch: Cardinal): HRESULT; stdcall;
    function set_Pitch(aPitch: Cardinal): HRESULT; stdcall;
  end;

  IDCSound3D = interface(IUnknown)
    ['{E83F7666-DD0A-4358-A0F0-75E1B8F52062}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Volume(out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aVolume: Word): HRESULT; stdcall;
  end;

  IDCTempo = interface(IUnknown)
    ['{E25E423C-866D-49CD-B950-CE12A17ED26D}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Tempo(out aTempo: integer): HRESULT; stdcall;
    function set_Tempo(aTempo: integer): HRESULT; stdcall;
  end;

  IDCTrebleEnhancer = interface(IUnknown)
    ['{45694F89-26E6-48EC-B2CD-22D7E74BD1C3}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
    function get_Frequency(out aFrequency: integer): HRESULT; stdcall;
    function set_Frequency(aFrequency: integer): HRESULT; stdcall;
  end;

  IDCTrueBass = interface(IUnknown)
    ['{74EB17C2-ED23-4175-B5B7-26395030BF5D}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
    function get_Seperate(out aSeperate: BOOL): HRESULT; stdcall;
    function set_Seperate(aSeperate: BOOL): HRESULT; stdcall;
    function get_Volume(aChannel: Byte; out aVolume: Word): HRESULT; stdcall;
    function set_Volume(aChannel: Byte; aVolume: Word): HRESULT; stdcall;
    function get_Frequency(out aFrequency: integer): HRESULT; stdcall;
    function set_Frequency(aFrequency: integer): HRESULT; stdcall;
  end;

  IDCDMOChorus = interface(IDirectSoundFXChorus)
    ['{2F99E23A-5D6E-4B51-8269-49C46ADC745C}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOCompressor = interface(IDirectSoundFXCompressor)
    ['{A5BA9E24-E017-48D2-9446-E42E5E1C9DCB}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMODistortion = interface(IDirectSoundFXDistortion)
    ['{A09AE606-B22B-4C1B-8ACF-45285BC30925}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOEcho = interface(IDirectSoundFXEcho)
    ['{22F654FD-7829-4BB8-9380-14EC6279D6D8}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOFlanger = interface(IDirectSoundFXFlanger)
    ['{50CEC6C5-0A67-4BBA-BEC5-A59D86DFC12B}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOGargle = interface(IDirectSoundFXGargle)
    ['{0F5FDB37-E3EA-4556-98E4-790345E28D8D}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOI3DL2Reverb = interface(IDirectSoundFXI3DL2Reverb)
    ['{22D00724-2D31-47E8-A5C4-3FF732224C9B}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOParamEQ = interface(IDirectSoundFXParamEq)
    ['{414E947F-0327-4D2F-B3CC-5AAEAB8EC4F2}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

  IDCDMOWavesReverb = interface(IDirectSoundFXWavesReverb)
    ['{CB0809E2-8436-475A-B37E-F21BACDD5014}']
    function get_Enabled(out aEnabled: BOOL): HRESULT; stdcall;
    function set_Enabled(aEnabled: BOOL): HRESULT; stdcall;
  end;

implementation

end.
