
    /*********************************************************************
     *  DCDSPFilterInterfaces.h                                          *
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
     *********************************************************************/

#include <dsound.h>

#ifdef __cplusplus
extern "C" {
#endif

DEFINE_GUID(CLSID_DCDSPFilter,     0xB38C58A0, 0x1809, 0x11D6, 0xA4, 0x58, 0xED, 0xAE, 0x78, 0xF1, 0xDF, 0x12);
DEFINE_GUID(IID_DCDSPFilter,       0xBD78EF46, 0x1809, 0x11D6, 0xA4, 0x58, 0xED, 0xAE, 0x78, 0xF1, 0xDF, 0x12);
DEFINE_GUID(IID_DCDSPFilterVisual, 0x3AA3B85E, 0xFBD5, 0x4D30, 0x8D, 0x3C, 0xB7, 0x8A, 0xFC, 0x24, 0xCE, 0x57);

DEFINE_GUID(IID_IDCAmplify,        0x3FB0116F, 0x52EE, 0x4286, 0xBF, 0x3A, 0x65, 0xC0, 0x05, 0x5E, 0xAA, 0x45);
DEFINE_GUID(IID_IDCBandPass,       0x0D608545, 0xCEAE, 0x4E19, 0xA6, 0xB4, 0xCE, 0x51, 0x6D, 0x41, 0x5C, 0x71);
DEFINE_GUID(IID_IDCChannelOrder,   0x567017E8, 0xEBC3, 0x444A, 0xBA, 0xAC, 0x7B, 0xEA, 0x38, 0x41, 0xEF, 0x2E);
DEFINE_GUID(IID_IDCCompressor,     0xE93CF724, 0xB897, 0x4A50, 0x82, 0x5B, 0xAC, 0x8F, 0xFD, 0xCA, 0x93, 0x7B);
DEFINE_GUID(IID_IDCDownMix,        0x9496B84F, 0xBC7B, 0x4230, 0x88, 0x9D, 0x1A, 0xDC, 0xC7, 0x90, 0xD2, 0x37);
DEFINE_GUID(IID_IDCDynamicAmplify, 0x0B60A7EC, 0xA76F, 0x4526, 0xAE, 0x29, 0xFF, 0x1F, 0xD8, 0xEA, 0x7D, 0xC3);
DEFINE_GUID(IID_IDCEchoDelay,      0xF5803607, 0x1B25, 0x422C, 0xB5, 0x5A, 0x22, 0x30, 0x5F, 0xC4, 0x0C, 0x37);
DEFINE_GUID(IID_IDCEqualizer,      0x14D4A709, 0x77ED, 0x459B, 0xB1, 0xE9, 0xE4, 0xE4, 0xC8, 0x42, 0x61, 0xBD);
DEFINE_GUID(IID_IDCFlanger,        0xEB6F78D4, 0xA20D, 0x4115, 0x89, 0x24, 0x89, 0xA3, 0x57, 0x1D, 0xD5, 0x87);
DEFINE_GUID(IID_IDCHighPass,       0x614F092A, 0x50E0, 0x4BEF, 0x9C, 0xD3, 0xB3, 0x90, 0xEB, 0x59, 0x58, 0xA8);
DEFINE_GUID(IID_IDCLowPass,        0xFCAE49B0, 0x005F, 0x4F21, 0x8B, 0x7F, 0x55, 0x5A, 0xB3, 0x19, 0x60, 0x73);
DEFINE_GUID(IID_IDCNotch,          0x35D3996E, 0xA312, 0x4A8D, 0xBE, 0xBA, 0x05, 0xA2, 0x6D, 0xBB, 0x2F, 0x34);
DEFINE_GUID(IID_IDCParametricEQ,   0xA51C1B92, 0x5C17, 0x40FB, 0xA1, 0xE2, 0x68, 0x5F, 0x32, 0x9D, 0x97, 0xCF};
DEFINE_GUID(IID_IDCPhaseInvert,    0x0102BF81, 0xAEFC, 0x4ACC, 0x91, 0x35, 0xDA, 0xF0, 0x66, 0x91, 0x51, 0xBD);
DEFINE_GUID(IID_IDCPhaser,         0x0CFA4706, 0x977A, 0x4B8C, 0xB1, 0x2F, 0xDD, 0xB2, 0x9B, 0x87, 0x3C, 0x3F);
DEFINE_GUID(IID_IDCPitchScale,     0x3484EA9C, 0x4598, 0x469C, 0x9E, 0xDB, 0x4A, 0xEF, 0x85, 0xD8, 0x35, 0xA8);
DEFINE_GUID(IID_IDCPitchShift,     0x96979014, 0xAD4E, 0x49F9, 0xA9, 0xDE, 0x3B, 0xD3, 0xA5, 0x5A, 0x44, 0x54);
DEFINE_GUID(IID_IDCSound3D,        0xE83F7666, 0xDD0A, 0x4358, 0xA0, 0xF0, 0x75, 0xE1, 0xB8, 0xF5, 0x20, 0x62);
DEFINE_GUID(IID_IDCTempo,          0xE25E423C, 0x866D, 0x49CD, 0xB9, 0x50, 0xCE, 0x12, 0xA1, 0x7E, 0xD2, 0x6D);
DEFINE_GUID(IID_IDCTrebleEnhancer, 0x45694F89, 0x26E6, 0x48EC, 0xB2, 0xCD, 0x22, 0xD7, 0xE7, 0x4B, 0xD1, 0xC3);
DEFINE_GUID(IID_IDCTrueBass,       0x74EB17C2, 0xED23, 0x4175, 0xB5, 0xB7, 0x26, 0x39, 0x50, 0x30, 0xBF, 0x5D);

DEFINE_GUID(IID_IDCDMOChorus,      0x2F99E23A, 0x5D6E, 0x4B51, 0x82, 0x69, 0x49, 0xC4, 0x6A, 0xDC, 0x74, 0x5C);
DEFINE_GUID(IID_IDCDMOCompressor,  0xA5BA9E24, 0xE017, 0x48D2, 0x94, 0x46, 0xE4, 0x2E, 0x5E, 0x1C, 0x9D, 0xCB);
DEFINE_GUID(IID_IDCDMODistortion,  0xA09AE606, 0xB22B, 0x4C1B, 0x8A, 0xCF, 0x45, 0x28, 0x5B, 0xC3, 0x09, 0x25);
DEFINE_GUID(IID_IDCDMOEcho,        0x22F654FD, 0x7829, 0x4BB8, 0x93, 0x80, 0x14, 0xEC, 0x62, 0x79, 0xD6, 0xD8);
DEFINE_GUID(IID_IDCDMOFlanger,     0x50CEC6C5, 0x0A67, 0x4BBA, 0xBE, 0xC5, 0xA5, 0x9D, 0x86, 0xDF, 0xC1, 0x2B);
DEFINE_GUID(IID_IDCDMOGargle,      0x0F5FDB37, 0xE3EA, 0x4556, 0x98, 0xE4, 0x79, 0x03, 0x45, 0xE2, 0x8D, 0x8D);
DEFINE_GUID(IID_IDCDMOI3DL2Reverb, 0x22D00724, 0x2D31, 0x47E8, 0xA5, 0xC4, 0x3F, 0xF7, 0x32, 0x22, 0x4C, 0x9B);
DEFINE_GUID(IID_IDCDMOParamEQ,     0x414E947F, 0x0327, 0x4D2F, 0xB3, 0xCC, 0x5A, 0xAE, 0xAB, 0x8E, 0xC4, 0xF2);
DEFINE_GUID(IID_IDCDMOWavesReverb, 0xCB0809E2, 0x8436, 0x475A, 0xB3, 0x7E, 0xF2, 0x1B, 0xAC, 0xDD, 0x50, 0x14);

typedef DWORD TDCFilterItem;

typedef struct _DSStream {
  int   Size;
  int   Frequency;
  int   Channels;
  BOOL  _Float;
  BOOL  SPDIF;
  BOOL  DTS;
} TDSStream, *PDSStream;

typedef enum TDCFilterType {
  ftNone,
  ftAmplify, 
  ftBandPass, 
  ftChannelOrder, 
  ftCompressor, 
  ftDownMix, 
  ftDynamicAmplify,
  ftEchoDelay, 
  ftEqualizer, 
  ftFlanger, 
  ftHighPass, 
  ftLowPass, 
  ftNotch, 
  ftPhaseInvert,
  ftPhaser, 
  ftPitchScale, 
  ftPitchShift, 
  ftSound3D, 
  ftTempo, 
  ftTrebleEnhancer, 
  ftTrueBass,
  ftDMOChorus, 
  ftDMOCompressor, 
  ftDMODistortion, 
  ftDMOEcho, 
  ftDMOFlanger, 
  ftDMOGargle,
  ftDMOI3DL2Reverb, 
  ftDMOParamEQ, 
  ftDMOWavesReverb,
  ftParametricEQ
} TDCFilterType; 

typedef enum TDCBitRate {
  br8BitInteger,
  br16BitInteger, 
  br24BitInteger, 
  br32BitInteger, 
  br32BitFloat
} TDCBitRate;

typedef enum TDCFFTSize {
  fts2, 
  fts4, 
  fts8, 
  fts16, 
  fts32, 
  fts64, 
  fts128, 
  fts256,
  fts512, 
  fts1024, 
  fts2048, 
  fts4096, 
  fts8192
} TDCFFTSize;

DECLARE_INTERFACE_(IDCDSPFilterPCMCallBack, IUnknown) {
  STDMETHOD (PCMDataCB) (THIS_ int *Buffer, int Length, int *NewSize, PDSStream Stream) PURE;
  STDMETHOD (MediaTypeChanged) (THIS_ PDSStream Stream) PURE;
  STDMETHOD (Flush) (THIS_) PURE;
};

DECLARE_INTERFACE_(IDCDSPFilterVisualInterface, IUnknown) {
  STDMETHOD (get_VISafterDSP) (THIS_ BOOL *AfterDSP) PURE;
  STDMETHOD (set_VISafterDSP) (THIS_ BOOL AfterDSP) PURE;
  STDMETHOD (get_VisualData) (THIS_ int *Buffer, int *Size, PDSStream *Stream) PURE;
};

DECLARE_INTERFACE_(IDCDSPFilterInterface, IUnknown) {
  // Callbacks for PCM Data
  STDMETHOD (set_CallBackPCM) (THIS_ IDCDSPFilterPCMCallBack *Callback) PURE;
  STDMETHOD (set_PCMDataBeforeMainDSP) (THIS_ BOOL Before) PURE;
  // Winamp 2.x DSP Plugins
  STDMETHOD (set_DSPPlugin) (THIS_ DWORD WindowHandle, char* Path) PURE;
  STDMETHOD (get_DSPPlugin) (THIS_ char *Path) PURE;
  STDMETHOD (get_DSPCount) (THIS_ int *Count) PURE;
  STDMETHOD (get_DSPDescription) (THIS_ char *Description) PURE;
  STDMETHOD (get_DSPSubDescription) (THIS_ int index, char *Description) PURE;
  STDMETHOD (set_DSPSubPlugin) (THIS_ int index) PURE;
  STDMETHOD (get_DSPSubPlugin) (THIS_ int *index) PURE;
  STDMETHOD (set_ShowConfig) (THIS_) PURE;
  STDMETHOD (set_UnloadDSPPlugin) (THIS_) PURE;
  STDMETHOD (get_EnableDSPPlug) (THIS_ BOOL *Enable) PURE;
  STDMETHOD (set_EnableDSPPlug) (THIS_ BOOL Enable) PURE;
  STDMETHOD (set_PluginOwnerWindow) (THIS_ DWORD Window) PURE;
  // Winamp Vis Plugins
  STDMETHOD (get_WinampVisInterval) (THIS_ int *Interval)  PURE;
  STDMETHOD (set_WinampVisInterval) (THIS_ int Interval)  PURE;
  STDMETHOD (get_WinampVisPlugin) (THIS_ char *Plugin, int *Index)  PURE;
  STDMETHOD (set_WinampVisPlugin) (THIS_ char *Plugin, int Index)  PURE;
  STDMETHOD (get_WinampVisAutostart) (THIS_ BOOL *Autostart) PURE;
  STDMETHOD (set_WinampVisAutostart) (THIS_ BOOL Autostart) PURE;
  STDMETHOD (set_StopWinampVisPlugin) (THIS_) PURE;
  // Delay functions for delaying the Audio Stream through Timestamps
  STDMETHOD (get_EnableDelay) (THIS_ BOOL *Enabled) PURE;
  STDMETHOD (set_EnableDelay) (THIS_ BOOL Enabled) PURE;
  STDMETHOD (get_Delay) (THIS_ int *Delay) PURE;
  STDMETHOD (set_Delay) (THIS_ int Delay) PURE;
  // functions to work with DSP Filters
  STDMETHOD (get_FilterCount) (THIS_ int *Count) PURE;
  STDMETHOD (get_FilterType) (THIS_ int Index, TDCFilterType *FilterType) PURE;
  STDMETHOD (set_AddFilter) (THIS_ int Index, TDCFilterType FilterType) PURE;
  STDMETHOD (get_FilterName) (THIS_ int Index, char *Name) PURE;
  STDMETHOD (get_WindowShown) (THIS_ int Index, BOOL *Shown) PURE;
  STDMETHOD (set_WindowShown) (THIS_ int Index, BOOL Shown) PURE;
  STDMETHOD (set_DeleteFilter) (THIS_ int Index) PURE;
  STDMETHOD (get_EnableFilter) (THIS_ int Index, BOOL *Enabled) PURE;
  STDMETHOD (set_EnableFilter) (THIS_ int Index, BOOL Enabled) PURE;
  STDMETHOD (set_RemoveAllFilters) (THIS_) PURE;
  STDMETHOD (set_MoveFilter) (THIS_ int FromIndex, int ToIndex) PURE;
  STDMETHOD (set_ResetShownWindows) (THIS_) PURE;
  STDMETHOD (get_FilterClass) (THIS_ int Index, void **Filter) PURE; // usage within Delphi
  STDMETHOD (get_FilterInterface) (THIS_ int Index, void **Intf) PURE;
  STDMETHOD (get_FilterItem) (THIS_ int Index, void **Item) PURE; // for internal use only!
  STDMETHOD (get_PresetCount) (THIS_ int *Count) PURE;
  STDMETHOD (get_PresetExist) (THIS_ char *Name, BOOL *Exist) PURE;
  STDMETHOD (get_PresetName) (THIS_ int Index, char **Name) PURE;
  STDMETHOD (set_LoadPreset) (THIS_ char *Name) PURE;
  STDMETHOD (set_SavePreset) (THIS_ char *Name) PURE;
  STDMETHOD (set_DeletePreset) (THIS_ char *Name) PURE;
  // Stream Switching
  STDMETHOD (get_EnableStreamSwitching) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableStreamSwitching) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_EnableStreamSwitchingInterface) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableStreamSwitchingInterface) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_ForceStreamSwitchingDisconnect) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_ForceStreamSwitchingDisconnect) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_ForceStreamSwitchingStopFilter) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_ForceStreamSwitchingStopFilter) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_EnableStreamLimitInstance) (THIS_ BOOL  *Enable)  PURE;
  STDMETHOD (set_EnableStreamLimitInstance) (THIS_ BOOL Enable)  PURE;
  // Bitrate Conversion
  STDMETHOD (set_EnableBitrateConversion) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_EnableBitrateConversion) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableBitrateConversionBeforeDSP) (THIS_ BOOL Before)  PURE;
  STDMETHOD (get_EnableBitrateConversionBeforeDSP) (THIS_ BOOL *Before)  PURE;
  STDMETHOD (set_BitrateConversionBits) (THIS_ TDCBitRate Bits)  PURE;
  STDMETHOD (get_BitrateConversionBits) (THIS_ TDCBitRate *Bits) PURE;
  // Misc Settings
  STDMETHOD (get_StreamInfo) (THIS_ PDSStream *Stream) PURE;
  STDMETHOD (set_DisableSaving) (THIS_ BOOL Disable)  PURE;
  STDMETHOD (get_FilterVersion) (THIS_ int *Version)  PURE;
  STDMETHOD (get_Instance) (THIS_ int *Instance)  PURE;
  STDMETHOD (get_CPUUsage) (THIS_ double *Usage)  PURE;
  STDMETHOD (get_EnablePropertyPage) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnablePropertyPage) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_TrayiconVisible) (THIS_ BOOL *Visible)  PURE;
  STDMETHOD (set_TrayiconVisible) (THIS_ BOOL Visible)  PURE;
  STDMETHOD (get_EnableBalloonHint) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableBalloonHint) (THIS_ BOOL Enable)  PURE;
  STDMETHOD (get_EnableROT) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableROT) (THIS_ BOOL Enable)  PURE;  
  STDMETHOD (get_EnableVisualBuffering) (THIS_ BOOL *Enable)  PURE;
  STDMETHOD (set_EnableVisualBuffering) (THIS_ BOOL Enable)  PURE;  
};

DECLARE_INTERFACE_(IDCAmplify, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Volume) (THIS_ byte aChannel, int *aVolume) PURE;
  STDMETHOD (set_Volume) (THIS_ byte aChannel, int aVolume) PURE;
};

DECLARE_INTERFACE_(IDCBandPass, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_CutoffLow) (THIS_ byte aChannel, float *aCutoff) PURE;
  STDMETHOD (set_CutoffLow) (THIS_ byte aChannel, float aCutoff) PURE;
  STDMETHOD (get_CutoffHigh) (THIS_ byte aChannel, float *aCutoff) PURE;
  STDMETHOD (set_CutoffHigh) (THIS_ byte aChannel, float aCutoff) PURE;
};

DECLARE_INTERFACE_(IDCChannelOrder, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Order) (THIS_ byte aChannel, byte *aOrder) PURE;
  STDMETHOD (set_Order) (THIS_ byte aChannel, byte aOrder) PURE;
};

DECLARE_INTERFACE_(IDCCompressor, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_AttackTime) (THIS_ float *aAttackTime) PURE;
  STDMETHOD (set_AttackTime) (THIS_ float aAttackTime) PURE;
  STDMETHOD (get_DecayTime) (THIS_ float aDecayTime) PURE;
  STDMETHOD (set_DecayTime) (THIS_ float aDecayTime) PURE;
  STDMETHOD (get_ThresholdDB) (THIS_ float *aThresholdDB) PURE;
  STDMETHOD (set_ThresholdDB) (THIS_ float aThresholdDB) PURE;
  STDMETHOD (get_Ratio) (THIS_ float *aRatio) PURE;
  STDMETHOD (set_Ratio) (THIS_ float aRatio) PURE;
  STDMETHOD (get_GainDB) (THIS_ float *aGainDB) PURE;
  STDMETHOD (set_GainDB) (THIS_ float aGainDB) PURE;
};

DECLARE_INTERFACE_(IDCDownMix, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDynamicAmplify, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_AttackTime) (THIS_ DWORD *aAttackTime) PURE;
  STDMETHOD (set_AttackTime) (THIS_ DWORD aAttackTime) PURE;
  STDMETHOD (get_ReleaseTime) (THIS_ DWORD *aReleaseTime) PURE;
  STDMETHOD (set_ReleaseTime) (THIS_ DWORD aReleaseTime) PURE;
  STDMETHOD (get_MaxAmplification) (THIS_ DWORD *aMaxAmplification) PURE;
  STDMETHOD (set_MaxAmplification) (THIS_ DWORD aMaxAmplification) PURE;
  STDMETHOD (get_CurrentAmplification) (THIS_ float *aCurrentAmplification) PURE;
};

DECLARE_INTERFACE_(IDCEchoDelay, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_KillMain) (THIS_ BOOL *aKillMain) PURE;
  STDMETHOD (set_KillMain) (THIS_ BOOL aKillMain) PURE;
  STDMETHOD (get_NumDelays) (THIS_ byte *aNumDelays) PURE;
  STDMETHOD (set_NumDelays) (THIS_ byte aNumDelays) PURE;
  STDMETHOD (get_Highpass) (THIS_ BOOL *aHighpass) PURE;
  STDMETHOD (set_Highpass) (THIS_ BOOL aHighpass) PURE;
  STDMETHOD (get_DelayAmp) (THIS_ WORD *aDelayAmp) PURE;
  STDMETHOD (set_DelayAmp) (THIS_ WORD aDelayAmp) PURE;
  STDMETHOD (get_Delay) (THIS_ WORD *aDelay) PURE;
  STDMETHOD (set_Delay) (THIS_ WORD aDelay) PURE;
};

DECLARE_INTERFACE_(IDCEqualizer, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_FFTSize) (THIS_ TDCFFTSize *aFFTSize) PURE;
  STDMETHOD (set_FFTSize) (THIS_ TDCFFTSize aFFTSize) PURE;
  STDMETHOD (get_Band) (THIS_ byte aChannel, WORD aIndex, char *aBand) PURE;
  STDMETHOD (set_Band) (THIS_ byte aChannel, WORD aIndex, char aBand) PURE;
};

DECLARE_INTERFACE_(IDCFlanger, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Frequency) (THIS_ byte aChannel, float *aFrequency) PURE;
  STDMETHOD (set_Frequency) (THIS_ byte aChannel, float aFrequency) PURE;
  STDMETHOD (get_Delay) (THIS_ byte aChannel, float *aDelay) PURE;
  STDMETHOD (set_Delay) (THIS_ byte aChannel, float aDelay) PURE;
  STDMETHOD (get_PhaseInvert) (THIS_ byte aChannel, BOOL *aPhaseInvert) PURE;
  STDMETHOD (set_PhaseInvert) (THIS_ byte aChannel, BOOL aPhaseInvert) PURE;
};

DECLARE_INTERFACE_(IDCHighPass, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Cutoff) (THIS_ byte aChannel, DWORD *aCutoff) PURE;
  STDMETHOD (set_Cutoff) (THIS_ byte aChannel, DWORD aCutoff) PURE;
};

DECLARE_INTERFACE_(IDCLowPass, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Cutoff) (THIS_ byte aChannel, DWORD *aCutoff) PURE;
  STDMETHOD (set_Cutoff) (THIS_ byte aChannel, DWORD aCutoff) PURE;
};

DECLARE_INTERFACE_(IDCNotch, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Cutoff) (THIS_ byte aChannel, DWORD *aCutoff) PURE;
  STDMETHOD (set_Cutoff) (THIS_ byte aChannel, DWORD aCutoff) PURE;
};

DECLARE_INTERFACE_(IDCParametricEQ, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Frequency)(THIS_ byte aChannel, float *aFrequency) PURE;
  STDMETHOD (set_Frequency)(THIS_ byte aChannel, float aFrequency) PURE;
  STDMETHOD (get_Gain)(THIS_ byte aChannel, float *aGain) PURE;
  STDMETHOD (set_Gain)(THIS_ byte aChannel, float aGain) PURE;
  STDMETHOD (get_Q)(THIS_ byte aChannel, float *aQ) PURE;
  STDMETHOD (set_Q)(THIS_ byte aChannel, float aQ) PURE;
};

DECLARE_INTERFACE_(IDCPhaseInvert, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Invert) (THIS_ byte aChannel, BOOL *aInvert) PURE;
  STDMETHOD (set_Invert) (THIS_ byte aChannel, BOOL aInvert) PURE;
};

DECLARE_INTERFACE_(IDCPhaser, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_DryWetRatio) (THIS_ byte aChannel, byte *aDryWetRatio) PURE;
  STDMETHOD (set_DryWetRatio) (THIS_ byte aChannel, byte aDryWetRatio) PURE;
  STDMETHOD (get_Feedback) (THIS_ byte aChannel, byte *aFeedback) PURE;
  STDMETHOD (set_Feedback) (THIS_ byte aChannel, byte aFeedback) PURE;
  STDMETHOD (get_Stages) (THIS_ byte aChannel, byte *aStages) PURE;
  STDMETHOD (set_Stages) (THIS_ byte aChannel, byte aStages) PURE;
  STDMETHOD (get_Depth) (THIS_ byte aChannel, byte *aDepth) PURE;
  STDMETHOD (set_Depth) (THIS_ byte aChannel, byte aDepth) PURE;
  STDMETHOD (get_StartPhase) (THIS_ byte aChannel, float *aStartPhase) PURE;
  STDMETHOD (set_StartPhase) (THIS_ byte aChannel, float aStartPhase) PURE;
  STDMETHOD (get_Frequency) (THIS_ byte aChannel, float *aFrequency) PURE;
  STDMETHOD (set_Frequency) (THIS_ byte aChannel, float aFrequency) PURE;
};

DECLARE_INTERFACE_(IDCPitchScale, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Pitch) (THIS_ byte aChannel, WORD *aPitch) PURE;
  STDMETHOD (set_Pitch) (THIS_ byte aChannel, WORD aPitch) PURE;
  STDMETHOD (get_Quality) (THIS_ byte aChannel, byte *aQuality) PURE;
  STDMETHOD (set_Quality) (THIS_ byte aChannel, byte aQuality) PURE;
  STDMETHOD (get_FFTSize) (THIS_ TDCFFTSize *aFFTSize) PURE;
  STDMETHOD (set_FFTSize) (THIS_ TDCFFTSize aFFTSize) PURE;
};

DECLARE_INTERFACE_(IDCPitchShift, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Pitch) (THIS_ DWORD *aPitch) PURE;
  STDMETHOD (set_Pitch) (THIS_ DWORD aPitch) PURE;
};

DECLARE_INTERFACE_(IDCSound3D, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Volume) (THIS_ WORD *aVolume) PURE;
  STDMETHOD (set_Volume) (THIS_ WORD aVolume) PURE;
};

DECLARE_INTERFACE_(IDCTempo, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Tempo) (THIS_ int *aTempo) PURE;
  STDMETHOD (set_Tempo) (THIS_ int aTempo) PURE;
};

DECLARE_INTERFACE_(IDCTrebleEnhancer, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Volume) (THIS_ byte aChannel, WORD *aVolume) PURE;
  STDMETHOD (set_Volume) (THIS_ byte aChannel, WORD aVolume) PURE;
  STDMETHOD (get_Frequency) (THIS_ int *aFrequency) PURE;
  STDMETHOD (set_Frequency) (THIS_ int aFrequency) PURE;
};

DECLARE_INTERFACE_(IDCTrueBass, IUnknown) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
  STDMETHOD (get_Seperate) (THIS_ BOOL *aSeperate) PURE;
  STDMETHOD (set_Seperate) (THIS_ BOOL aSeperate) PURE;
  STDMETHOD (get_Volume) (THIS_ byte aChannel, WORD *aVolume) PURE;
  STDMETHOD (set_Volume) (THIS_ byte aChannel, WORD aVolume) PURE;
  STDMETHOD (get_Frequency) (THIS_ int *aFrequency) PURE;
  STDMETHOD (set_Frequency) (THIS_ int aFrequency) PURE;
};

DECLARE_INTERFACE_(IDCDMOChorus, IDirectSoundFXChorus) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOCompressor, IDirectSoundFXCompressor) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMODistortion, IDirectSoundFXDistortion) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOEcho, IDirectSoundFXEcho) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOFlanger, IDirectSoundFXFlanger) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOGargle, IDirectSoundFXGargle) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOI3DL2Reverb, IDirectSoundFXI3DL2Reverb) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOParamEQ, IDirectSoundFXParamEq) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

DECLARE_INTERFACE_(IDCDMOWavesReverb, IDirectSoundFXWavesReverb) {
  STDMETHOD (get_Enabled) (THIS_ BOOL *aEnabled) PURE;
  STDMETHOD (set_Enabled) (THIS_ BOOL aEnabled) PURE;
};

#ifdef __cplusplus
}
#endif
