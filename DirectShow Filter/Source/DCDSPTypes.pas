
    (*********************************************************************
     *  DCDSPTypes.pas                                                   *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
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

unit DCDSPTypes;

{$I Compiler.inc}

interface

uses
  Windows, DirectShow9, SysUtils, dspConst,
  {$IFDEF WITH_INTERNAL_DSP} DynamicFilterList, {$ENDIF}
  Classes;

const
  FilterVersion = 01030;  // First 4 digits / 1000 indicates Version,
                          // last digit indicates Beta Version, 0 = Final, >0 = Beta/RC
  FilterReleaseDate = '22. August 2005';

  DefaultAudioTrack = 'Audio Track ';

{$IFDEF WITH_PROPERTYPAGES}
  PPWidth = 604;
  PPHeight = 439;
{$ENDIF}

  SudPinTypes : array[0..1] of TRegPinTypes =
  (
    (clsMajorType: @MEDIATYPE_AUDIO;clsMinorType: @MEDIASUBTYPE_PCM),
    (clsMajorType: @MEDIATYPE_AUDIO;clsMinorType: @MEDIASUBTYPE_IEEE_FLOAT)
  );

  SudPins : array[0..1] of TRegFilterPins =
  (
    (strName: 'Input'; bRendered: FALSE; bOutput: FALSE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: 'Output'; nMediaTypes: 2; lpMediaType: @SudPinTypes),
    (strName: 'Output'; bRendered: FALSE; bOutput: TRUE; bZero: FALSE; bMany: FALSE; oFilter: nil; strConnectsToPin: 'Input'; nMediaTypes: 2; lpMediaType: @SudPinTypes)
  );

  CLSID_DCDSPFilter     : TGUID = '{B38C58A0-1809-11D6-A458-EDAE78F1DF12}';
  IID_DCDSPFilter       : TGUID = '{BD78EF46-1809-11D6-A458-EDAE78F1DF12}';
  IID_DCDSPFilterVisual : TGUID = '{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}';

  CLSID_DCDSPFilterPropertyPageWinamp    : TGUID = '{76060504-F57F-47FA-B553-2E4ADACDE384}';
  CLSID_DCDSPFilterPropertyPageWinampVis : TGUID = '{AAEFADF6-0514-4650-9C9C-3F1876E721AD}';
  CLSID_DCDSPFilterPropertyPageAbout     : TGUID = '{87754825-3B14-49AC-84E2-53278A96EFF8}';
  CLSID_DCDSPFilterPropertyPageDSP       : TGUID = '{0CF63C7E-4C75-4EDA-95DA-084310D6CF0B}';
  CLSID_DCDSPFilterPropertyPageOptions   : TGUID = '{1304D1C1-8ED2-4984-8F10-7B3E25805D66}';

type
{$IFNDEF WITH_INTERNAL_DSP}
  TDCFilterType = integer;
  TDCFilterItem = Pointer;
{$ENDIF}

  TLangItem = record
    aLCID: DWORD;
    aName: array[0..1023] of WCHAR;
  end;
  PLangItem = ^TLangItem;
  TDSPPlugin = record
    FileName : String;
    Desription : String;
  end;

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
    function get_FilterItem(Index : integer; out Item : TDCFilterItem): HRESULT; stdcall;
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

implementation

end.
