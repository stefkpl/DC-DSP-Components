
    (*********************************************************************
     *  ControlDCDSPFilter.pas                                           *
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

unit ControlDCDSPFilter;

{$I Compiler.inc}

interface

uses
  Classes, Windows, DirectShow9, ActiveX, DCDSPTypes, dspConst
  {$IFDEF WITH_INTERNAL_DSP} ,DynamicFilterList {$ENDIF}
  ;

type
  TDCDSPFilterControl = class(TComponent)
  private
    fSetup: IDCDSPFilterInterface;
    fVisual : IDCDSPFilterVisualInterface;
    fIsDCDSPFilterUsed : Boolean;
    procedure Set_Graph (Graph : IGraphBuilder);
    procedure Set_Filter(Filter : IUnknown);
    procedure Set_EnableAutoLoadWinampVis(Enable : Boolean);
    function Get_EnableAutoLoadWinampVis : Boolean;
    procedure Set_VISafterDSP(Enable : Boolean);
    function Get_VISafterDSP : Boolean;
    function Get_DSPCount : integer;
    function Get_DSPDescription : PChar;
    function Get_EnableDSPPlug : Boolean;
    procedure Set_EnableDSPPlug(Enable : Boolean);
    function Get_StreamInfo : PDSStream;
    procedure Set_DSPSubPlugin(index : integer);
    function Get_DSPSubPlugin : integer;
    function Get_FilterVersion : integer;
    procedure Set_WinampVisInterval(Interval : integer);
    function Get_WinampVisInterval : integer;
    function GetEnableDelay : Boolean;
    procedure SetEnableDelay(Enable : Boolean);
    function GetDelay : integer;
    procedure SetDelay(Delay : integer);
    function Get_FilterCount : integer;
    procedure Set_FilterEnabled(Index : integer; Enabled : Boolean);
    function Get_FilterEnabled(Index : integer) : Boolean;
    procedure Set_WindowShown(Index : integer; Enabled : Boolean);
    function Get_WindowShown(Index : integer) : Boolean;
    function Get_PresetCount : integer;
    function Get_CPUUsage : Double;
    function GetEnableStreamSwitching : Boolean;
    procedure SetEnableStreamSwitching(Enable : Boolean);
    function GetEnableStreamSwitchingInterface : Boolean;
    procedure SetEnableStreamSwitchingInterface(Enable : Boolean);
    function GetEnableLimitInstance : Boolean;
    procedure SetEnableLimitInstance(Enable : Boolean);
    function GetEnableBitrateConversion : Boolean;
    procedure SetEnableBitrateConversion(Enable : Boolean);
    function GetEnableBitrateConversionBeforeDSP : Boolean;
    procedure SetEnableBitrateConversionBeforeDSP(Before : Boolean);
    function GetBitrateConversionBits : TDCBitRate;
    procedure SetBitrateConversionBits(Bits : TDCBitRate);
    procedure SetEnableStreamSwitchingForceDisconnect(Enable : Boolean);
    function GetEnableStreamSwitchingForceDisconnect : Boolean;
    procedure SetEnableStreamSwitchingForceStopFilter(Enable : Boolean);
    function GetEnableStreamSwitchingForceStopFilter : Boolean;
    procedure SetTrayIconVisible(Visible : Boolean);
    function GetTrayIconVisible : Boolean;
    procedure SetEnableBalloonHint(Enable : Boolean);
    function GetEnableBalloonHint: Boolean;
    function GetEnableRunningObjectTable : Boolean;
    procedure SetEnableRunningObjectTable(Enable : Boolean);
    function GetEnableVisualBuffering : Boolean;
    procedure SetEnableVisualBuffering(Enable : Boolean);
  public
    property GraphBuilder : IGraphBuilder write Set_Graph;
    property DCDSPFilter : IUnknown write Set_Filter;
    property IsDCDSPFilterUsed : Boolean read fIsDCDSPFilterUsed;
    // Callbacks for PCM Data
    function Set_PCMCallback (Callback : IDCDSPFilterPCMCallBack) : HRESULT;
    procedure Set_PCMDataBeforeMainDSP(Before : Boolean);
    // Winamp 2.x DSP Plugins
    property DSPPluginCount : integer read Get_DSPCount;
    property DSPPlugin : integer read Get_DSPSubPlugin write Set_DSPSubPlugin;
    property DSPDescription : PChar read Get_DSPDescription;
    property DSPPlugEnabled : Boolean read Get_EnableDSPPlug write Set_EnableDSPPlug;
    function Set_DSPPlugin(WindowHandle : hWnd; Path : PChar): HRESULT;
    function Set_DSPPluginOwner(WindowHandle : hWnd): HRESULT;
    function Get_DSPPlugin : String;
    function Get_DSPSubDescription(index : integer; out Description : PChar): HRESULT;
    function DSPShowConfig: HRESULT;
    function UnloadDSPPlugin: HRESULT;
    // Winamp Vis Plugins
    property WinampVisInterval : integer read Get_WinampVisInterval write Set_WinampVisInterval;
    property AutoLoadWinampVis : Boolean read Get_EnableAutoLoadWinampVis write Set_EnableAutoLoadWinampVis;
    procedure StopWinampVisPlugin;
    procedure Get_WinampVisPlugin(out Plugin : String; out Index : integer);
    procedure Set_WinampVisPlugin(Plugin : String; Index : integer);
    // Delay functions for delaying the Audio Stream through Timestamps
    property EnableAudioDelay : Boolean read GetEnableDelay write SetEnableDelay;
    property AudioDelay : integer read GetDelay write SetDelay;
    // functions to work with DSP Filters
    property FilterCount : integer read Get_FilterCount;
    function FilterType(Index : integer) : TDCFilterType;
    procedure AddFilter(Index : integer; FilterType : TDCFilterType);
    function FilterName(Index : integer) : String;
    property WindowShown[Index : integer] : Boolean read Get_WindowShown write Set_WindowShown;
    procedure DeleteFilter(Index : integer);
    property FilterEnabled[Index : integer] : Boolean read Get_FilterEnabled write Set_FilterEnabled;
    procedure RemoveAllFilters;
    procedure MoveFilter(FromIndex : integer; ToIndex : integer);
    procedure ResetShownWindows;
    function FilterClass(Index : integer) : TComponent;
    procedure GetFilterInterface(Index : integer; out Obj);
    function FilterItem(Index : integer) : TDCFilterItem;
    property PresetCount : integer read Get_PresetCount;
    function PresetExist(Name : String) : Boolean;
    function PresetName(Index : integer) : String;
    procedure LoadPreset(Name : String);
    procedure SavePreset(Name : String);
    procedure DeletePreset(Name : String);
    // Misc Settings
    property StreamInfo : PDSStream read Get_StreamInfo;
    property FilterVersion : integer read Get_FilterVersion;
    procedure DisableSaving(Disable : Boolean);
    property CPUUsage : Double read Get_CPUUsage;
    property EnableStreamSwitching : Boolean read GetEnableStreamSwitching write SetEnableStreamSwitching;
    property EnableStreamSwitchingInterface : Boolean read GetEnableStreamSwitchingInterface write SetEnableStreamSwitchingInterface;
    property EnableStreamSwitchingForceDisconnect : Boolean read GetEnableStreamSwitchingForceDisconnect write SetEnableStreamSwitchingForceDisconnect;
    property EnableStreamSwitchingForceStopFilter : Boolean read GetEnableStreamSwitchingForceStopFilter write SetEnableStreamSwitchingForceStopFilter;
    property EnableStreamLimitInstance : Boolean read GetEnableLimitInstance write SetEnableLimitInstance;
    property TrayIconVisible : Boolean read GetTrayIconVisible write SetTrayIconVisible;
    property EnableBalloonHint: Boolean read GetEnableBalloonHint write SetEnableBalloonHint;
    property EnableRunningObjectTable : Boolean read GetEnableRunningObjectTable write SetEnableRunningObjectTable;
    property EnableVisualBuffering : Boolean read GetEnableVisualBuffering write SetEnableVisualBuffering;
    // Bitrate Conversion
    property EnableBitrateConversion: Boolean read GetEnableBitrateConversion write SetEnableBitrateConversion;
    property BitrateConversionBeforeDSP: Boolean read GetEnableBitrateConversionBeforeDSP write SetEnableBitrateConversionBeforeDSP;
    property BitrateConversionBits: TDCBitRate read GetBitrateConversionBits write SetBitrateConversionBits;

    // IDCDSPFilterVisualInterface
    function Get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream): Boolean;
    property VISafterDSP : Boolean read Get_VISafterDSP write Set_VISafterDSP;

    destructor Destroy; override;
    constructor Create(AOwner : TComponent); override;
  end;

implementation
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_Graph(Graph : IGraphBuilder);
var
  EnumFilters: IEnumFilters;
  BaseFilter: IBaseFilter;
  ClassID : TGUID;
begin
  fSetup := nil;
  fVisual := nil;
  fIsDCDSPFilterUsed := False;
  if (Graph <> nil) then
  begin
    if Graph.EnumFilters(EnumFilters) = S_OK then
    begin
      while (EnumFilters.Next(1, BaseFilter, nil) = S_OK) do
      begin
        BaseFilter.GetClassID(ClassID);
        if IsEqualGUID(ClassID, CLSID_DCDSPFilter) then
        begin
          BaseFilter.QueryInterface(IID_DCDSPFilter, fSetup);
          BaseFilter.QueryInterface(IID_DCDSPFilterVisual, fVisual);
          if (fSetup <> nil) and (fVisual <> nil) then
          begin
            fIsDCDSPFilterUsed := True;
            break;
          end;
        end;
      end;
    end;
  end;
  EnumFilters := nil;
  BaseFilter := nil;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_Filter(Filter : IUnknown);
begin
  if Filter = nil then
  begin
    fIsDCDSPFilterUsed := False;
    fSetup := nil;
    fVisual := nil;
    Exit;
  end;
  Filter.QueryInterface(IID_DCDSPFilter, fSetup);
  Filter.QueryInterface(IID_DCDSPFilterVisual, fVisual);
  fIsDCDSPFilterUsed := (fSetup <> nil) and (fVisual <> nil);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_VISafterDSP (Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fVisual.Set_VISafterDSP(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_VISafterDSP : Boolean;
var
  Res: BOOL;
begin
  Result := True;
  if not fIsDCDSPFilterUsed then Exit;
  fVisual.Get_VISafterDSP(Res);
  Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Set_PCMCallback(Callback : IDCDSPFilterPCMCallBack) : HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_CallBackPCM(Callback);
  Result := S_OK;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_DSPCount : integer;
begin
  Result := 0;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_DSPCount(Result);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_DSPDescription : PChar;
begin
  Result := '';
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_DSPDescription(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_EnableDSPPlug (Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_EnableDSPPlug(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_EnableDSPPlug : Boolean;
var
  Res: Bool;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_EnableDSPPlug(Res);
  Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_StreamInfo : PDSStream;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_StreamInfo(Result);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Set_DSPPlugin(WindowHandle : hWnd; Path : PChar): HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  Result := fSetup.Set_DSPPlugin(WindowHandle,Path);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_DSPPlugin : String;
var
  res : PChar;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_DSPPlugin(Res);
  Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_DSPSubDescription(index : integer; out Description : PChar): HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  Result := fSetup.Get_DSPSubDescription(index,Description);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_DSPSubPlugin : integer;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_DSPSubPlugin(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_DSPSubPlugin(index : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_DSPSubPlugin(index);
end;
(*******************************************************************************)
function TDCDSPFilterControl.DSPShowConfig : HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  Result := fSetup.Set_ShowConfig;
end;
(*******************************************************************************)
function TDCDSPFilterControl.UnloadDSPPlugin : HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  Result := fSetup.Set_UnloadDSPPlugin;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Set_DSPPluginOwner(WindowHandle : hWnd): HRESULT;
begin
  Result := S_FALSE;
  if not fIsDCDSPFilterUsed then Exit;
  Result := fSetup.Set_PluginOwnerWindow(WindowHandle);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream): Boolean;
begin
  if not fIsDCDSPFilterUsed then
  begin
    Result := False;
    Exit;
  end;
  Result := fVisual.Get_VisualData(Buffer,Size,Stream) = S_OK;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.DisableSaving(Disable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_DisableSaving(Disable);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_PCMDataBeforeMainDSP(Before : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_PCMDataBeforeMainDSP(Before);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_FilterVersion : integer;
begin
  if not fIsDCDSPFilterUsed then
  begin
    Result := 0;
    Exit;
  end;
  fSetup.Get_FilterVersion(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_WinampVisInterval(Interval : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_WinampVisInterval(Interval);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_WinampVisInterval : integer;
begin
  if not fIsDCDSPFilterUsed then
  begin
    Result := 0;
    Exit;
  end;
  fSetup.Get_WinampVisInterval(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.StopWinampVisPlugin;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_StopWinampVisPlugin;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Get_WinampVisPlugin(out Plugin : String; out Index : integer);
var
  s : PChar;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_WinampVisPlugin(s,Index);
  Plugin := s;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_WinampVisPlugin(Plugin : String; Index : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_WinampVisPlugin(PChar(Plugin),Index);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_EnableAutoLoadWinampVis(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Set_WinampVisAutostart(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_EnableAutoLoadWinampVis : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.Get_WinampVisAutostart(Res);
  Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableDelay : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableDelay(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableDelay(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableDelay(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetDelay : integer;
begin
  Result := 0;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_Delay(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetDelay(Delay : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_Delay(Delay);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_FilterCount : integer;
begin
  Result := 0;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_FilterCount(Result);
end;
(*******************************************************************************)
function TDCDSPFilterControl.FilterType(Index : integer) : TDCFilterType;
begin
  {$IFDEF WITH_INTERNAL_DSP}
  Result := ftNone;
  {$ELSE}
  Result := -1;
  {$ENDIF}
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_FilterType(Index,Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.AddFilter(Index : integer; FilterType : TDCFilterType);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_AddFilter(Index,FilterType);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_WindowShown(Index : integer) : Boolean;
var
  Res : Bool;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_WindowShown(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_WindowShown(Index : integer; Enabled : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_WindowShown(Index,Enabled);
end;
(*******************************************************************************)
function TDCDSPFilterControl.FilterName(Index : integer) : String;
var
  Res : PChar;
begin
  Result := '';
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_FilterName(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.DeleteFilter(Index : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_DeleteFilter(Index);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.Set_FilterEnabled(Index : integer; Enabled : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableFilter(Index,Enabled);
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_FilterEnabled(Index : integer) : Boolean;
var
  Res : Bool;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_EnableFilter(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.RemoveAllFilters;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_RemoveAllFilters;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.MoveFilter(FromIndex : integer; ToIndex : integer);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_MoveFilter(FromIndex,ToIndex);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.ResetShownWindows;
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_ResetShownWindows;
end;
(*******************************************************************************)
function TDCDSPFilterControl.FilterClass(Index : integer) : TComponent;
var
  Res : TComponent;
begin
  Result := nil;
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_FilterClass(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.GetFilterInterface(Index : integer; out Obj);
var
  Res : IUnknown;
begin
  Pointer(Obj) := nil;
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_FilterInterface(Index,Res) = S_OK then IUnknown(Obj) := Res;
  Res := nil;
end;
(*******************************************************************************)
function TDCDSPFilterControl.FilterItem(Index : integer) : TDCFilterItem;
var
  Res : TDCFilterItem;
begin
  Result := nil;
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_FilterItem(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_PresetCount : integer;
begin
  Result := 0;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_PresetCount(Result);
end;
(*******************************************************************************)
function TDCDSPFilterControl.PresetExist(Name : String) : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_PresetExist(PChar(Name),Res);
  Result := Res;
end;
(*******************************************************************************)
function TDCDSPFilterControl.PresetName(Index : integer) : String;
var
  Res : PChar;
begin
  Result := '';
  if not fIsDCDSPFilterUsed then Exit;
  if fSetup.get_PresetName(Index,Res) = S_OK then Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.LoadPreset(Name : String);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_LoadPreset(PChar(Name));
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SavePreset(Name : String);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_SavePreset(PChar(Name));
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.DeletePreset(Name : String);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_DeletePreset(PChar(Name));
end;
(*******************************************************************************)
function TDCDSPFilterControl.Get_CPUUsage : Double;
begin
  Result := 0;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_CPUUsage(Result);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableStreamSwitching : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableStreamSwitching(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableStreamSwitching(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableStreamSwitching(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableStreamSwitchingInterface : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableStreamSwitchingInterface(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableStreamSwitchingInterface(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableStreamSwitchingInterface(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableLimitInstance : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableStreamLimitInstance(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableLimitInstance(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableStreamLimitInstance(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableBitrateConversion : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableBitrateConversion(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableBitrateConversion(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableBitrateConversion(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableBitrateConversionBeforeDSP : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableBitrateConversionBeforeDSP(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableBitrateConversionBeforeDSP(Before : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableBitrateConversionBeforeDSP(Before);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetBitrateConversionBits : TDCBitRate;
begin
  Result := br16BitInteger;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_BitrateConversionBits(Result);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetBitrateConversionBits(Bits : TDCBitRate);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_BitrateConversionBits(Bits);
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableStreamSwitchingForceDisconnect(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_ForceStreamSwitchingDisconnect(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableStreamSwitchingForceDisconnect : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_ForceStreamSwitchingDisconnect(Res);
  Result := Res;
end;
(*******************************************************************************)
procedure TDCDSPFilterControl.SetEnableStreamSwitchingForceStopFilter(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_ForceStreamSwitchingStopFilter(Enable);
end;
(*******************************************************************************)
function TDCDSPFilterControl.GetEnableStreamSwitchingForceStopFilter : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_ForceStreamSwitchingStopFilter(Res);
  Result := Res;
end;

procedure TDCDSPFilterControl.SetTrayIconVisible(Visible : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_TrayiconVisible(Visible);
end;

function TDCDSPFilterControl.GetTrayIconVisible : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_TrayiconVisible(Res);
  Result := Res;
end;

function TDCDSPFilterControl.GetEnableRunningObjectTable : Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableROT(Res);
  Result := Res;
end;

procedure TDCDSPFilterControl.SetEnableRunningObjectTable(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableROT(Enable);
end;

procedure TDCDSPFilterControl.SetEnableBalloonHint(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableBalloonHint(Enable);
end;

function TDCDSPFilterControl.GetEnableBalloonHint: Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableBalloonHint(Res);
  Result := Res;
end;

procedure TDCDSPFilterControl.SetEnableVisualBuffering(Enable : Boolean);
begin
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.set_EnableVisualBuffering(Enable);
end;

function TDCDSPFilterControl.GetEnableVisualBuffering: Boolean;
var
  Res: BOOL;
begin
  Result := False;
  if not fIsDCDSPFilterUsed then Exit;
  fSetup.get_EnableVisualBuffering(Res);
  Result := Res;
end;

(******************************************************************************)

destructor TDCDSPFilterControl.Destroy;
begin
  fIsDCDSPFilterUsed := False;
  fSetup := nil;
  fVisual := nil;
  inherited Destroy;
end;
(*******************************************************************************)
constructor TDCDSPFilterControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fIsDCDSPFilterUsed := False;
  fSetup := nil;
  fVisual := nil;
end;
(*******************************************************************************)
end.
