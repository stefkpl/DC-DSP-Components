
    (*********************************************************************
     *  DCVISPluginRenderer.pas                                          *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 04-08-2003                                           *
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

unit DCVISPluginRenderer;

interface

uses
  SysUtils, Windows, Classes, visWaveform, visSpectrum, ExtCtrls, dspConst,
  DCVISPluginAPI, DirectShow9, Messages, waUtils;

const
  VISHeaderExport = 'DCVISPluginGetHeader';
  IID_IVisualInterface : TGUID = '{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}';

type
  IVisualInterface = interface(IUnknown)
  ['{3AA3B85E-FBD5-4D30-8D3C-B78AFC24CE57}']
    function get_VISafterDSP(out AfterDSP : Boolean): HRESULT; stdcall;
    function set_VISafterDSP(AfterDSP : Boolean): HRESULT; stdcall;
    function get_VisualData(out Buffer : Pointer; out Size : integer; out Stream : PDSStream) : HRESULT; stdcall;
  end;

  TDCVISPluginList = class;

  TDCVISPluginItem  = class(TCollectionItem)
  private
    fVISWindow : integer;
    fCurrentIndex : integer;
    fCurrentConfig : integer;
    fPlugin : PDCVISPluginHeader;
    fMediaName : String;
    fOwnerWindow : hWnd;
    fVISOwnerWindow : hWnd;
    fNumPlugins : integer;
    fLibHandle : hWnd;
    fOwner : TDCVISPluginList;
    fFilename : String;
    procedure SetFilename(sFilename : String);
    function GetPluginName : String;
    function GetSubPluginName(Index : integer) : String;
    procedure SetOwnerWindow(Window : hWnd);
    procedure SetVISOwnerWindow(Window : hWnd);
    function VISWndProc(Handle : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : Boolean;
  public
    destructor Destroy; override;
    procedure AboutInit;
    procedure AboutClose;
    procedure ConfigInit(Index : integer);
    procedure ConfigClose;
    procedure Init(Index : integer; Width : integer; Height : integer);
    procedure Quit;
    procedure Resize(Width : integer; Height : integer);
    property SubPluginName[Index : integer] : String read GetSubPluginName;
    procedure SetMediaName(Name : String);
  published
    property Count : integer read fNumPlugins;
    property Owner : TDCVISPluginList read fOwner write fOwner;
    property Filename : String read fFilename write SetFilename;
    property PluginName : String read GetPluginName;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
    property VISOwnerWindow : hWnd read fVISOwnerWindow write SetVISOwnerWindow;
    property CurrentIndex : integer read fCurrentIndex;
  end;

  TDCVISPluginList = class(TCollection)
  private
    fOwnerWindow : hWnd;
    fHasSpectrum : Boolean;
    fHasWaveform : Boolean;
    fVISOwnerWindow : hWnd;
    fCurrentPlugin : integer;
    fCurrentPluginIndex : integer;
    fDirectory : String;
    fTimer : TTimer;
    fIntf : IVisualInterface;
    fSpectrum : TDCSpectrum;
    fWaveform : TDCWaveform;
    function  GetItem(Index : Integer) : TDCVISPluginItem;
    procedure SetItem(Index : Integer; Value : TDCVISPluginItem);
    procedure SetDirectory(Dir : String);
    procedure OnTimer(Sender : TObject);
    procedure SetOwnerWindow(Window : hWnd);
    procedure SetVISOwnerWindow(Window : hWnd);
    function IsOurPlugin(Filename : String) : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add : TDCVISPluginItem;
    property Items[Index: Integer]: TDCVISPluginItem read GetItem write SetItem; default;
    procedure Resize(Width : integer; Height : integer);
    procedure Init(Index : integer; SubIndex : integer; Width : integer; Height : integer);
    procedure Quit;
    procedure SetMediaName(Name : String);
    procedure SetAudiofilter(Filter : IBaseFilter);
  published
    property Directory : String read fDirectory write SetDirectory;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
    property VISOwnerWindow : hWnd read fVISOwnerWindow write SetVISOwnerWindow;
    property CurrentPlugin : integer read fCurrentPlugin write fCurrentPlugin;
    property CurrentPluginIndex : integer read fCurrentPluginIndex write fCurrentPluginIndex;
  end;

implementation

var
  gWndProcOld : Pointer;
  gItem : TDCVISPluginItem;

constructor TDCVISPluginList.Create;
begin
  inherited Create(TDCVISPluginItem);

  fSpectrum := TDCSpectrum.Create(nil);
  fSpectrum.MinY := 0;
  fSpectrum.MaxY := 65535;
  fSpectrum.FFTSize := fts1024;
  fSpectrum.PreEmphesis := 91;
  fSpectrum.PreEmphesisEnabled := True;
  fSpectrum.WindowMode := wmRectangular;
  fSpectrum.SampleSkip := 2;
  fSpectrum.Logarithmic := False;

  fWaveform := TDCWaveform.Create(nil);
  fWaveform.MinY := 0;
  fWaveform.MaxY := 65535;
  fWaveform.NumSamples := 512;
  fWaveform.SampleSkip := 2;

  fCurrentPlugin := -1;

  fTimer := TTimer.Create(nil);
  fTimer.Enabled := False;
  fTimer.OnTimer := OnTimer;
  fTimer.Interval := 40;
end;

destructor TDCVISPluginList.Destroy;
begin
  Clear;
  fTimer.Enabled := False;
  FreeAndNil(fTimer);
  FreeAndNil(fSpectrum);
  FreeAndNil(fWaveform);
  inherited Destroy;
end;

function TDCVISPluginList.Add : TDCVISPluginItem;
begin
  Result := TDCVISPluginItem(inherited Add);
end;

function TDCVISPluginList.GetItem(Index : Integer) : TDCVISPluginItem;
begin
  Result := TDCVISPluginItem(inherited GetItem(Index));
end;

procedure TDCVISPluginList.SetItem(Index : Integer; Value : TDCVISPluginItem);
begin
  inherited SetItem(Index, Value);
end;

function TDCVISPluginList.IsOurPlugin(Filename : String) : Boolean;
var
  DLL : hWnd;
  Proc : function : Pointer; stdcall;
  Plugin : PDCVISPluginHeader;
  NumPlugins : integer;
begin
  Result := False;
  DLL := LoadLibrary(PChar(Filename));
  if DLL <> 0 then
  begin
    Proc := GetProcAddress(DLL,PChar(VISHeaderExport));
    if @Proc <> nil then
    begin
      Plugin := Proc;
      if Plugin.Version = DCSDKVerVIS then
      begin
        NumPlugins := 0;
        while Plugin.GetModule(NumPlugins) <> nil do inc(NumPlugins);
        Result := NumPlugins > 0;
      end;
    end;
    FreeLibrary(DLL);
  end;
end;

procedure TDCVISPluginList.SetDirectory(Dir : String);
var
  srh: THandle;
  sr : TWin32FindData;
begin
  Dir := AddBackSlash(Dir);
  if UpperCase(Dir) = UpperCase(fDirectory) then Exit;
  fDirectory := Dir;
  Clear;

  srh := FindFirstFile(PChar(fDirectory + 'vis_*.dll'),sr);
  if srh <> INVALID_HANDLE_VALUE then
  repeat
    if IsOurPlugin(fDirectory + sr.cFileName) then
    begin
      Add;
      Items[Count -1].Owner := Self;
      Items[Count -1].fOwnerWindow := fOwnerWindow;
      Items[Count -1].fVISOwnerWindow := fVISOwnerWindow;
      Items[Count -1].Filename := fDirectory + sr.cFileName;
    end;
  until FindNextFile(srh,sr) = False;
  FindClose(srh);
end;

procedure TDCVISPluginList.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  fOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].OwnerWindow := fOwnerWindow;
end;

procedure TDCVISPluginList.SetVISOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  if Window = fOwnerWindow then Exit;
  fVISOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].VISOwnerWindow := fVISOwnerWindow;
end;

procedure TDCVISPluginList.Resize(Width : integer; Height : integer);
var
  i : integer;
begin
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].Resize(Width,Height);
end;

procedure TDCVISPluginList.Init(Index : integer; SubIndex : integer; Width : integer; Height : integer);
begin
  if (fCurrentPlugin = Index) and (fCurrentPluginIndex = SubIndex) then Exit;
  Quit;
  if (Index > Count -1) or (SubIndex > Items[Index].Count -1) then Exit;
  Items[Index].Init(SubIndex,Width,Height)
end;

procedure TDCVISPluginList.Quit;
begin
  if (fCurrentPlugin = -1) or (fCurrentPluginIndex = -1) then Exit;
  Items[fCurrentPlugin].Quit;
end;

procedure TDCVISPluginList.SetMediaName(Name : String);
var
  i : integer;
begin
  if (Count < 1) then Exit;
  for i := 0 to Count -1 do Items[i].SetMediaName(Name);
end;

procedure TDCVISPluginList.OnTimer(Sender : TObject);
var
  Stream : PDSStream;
  Buffer : Pointer;
  Size : integer;
  i, c : integer;
begin
  if (fCurrentPlugin = -1) or (fCurrentPluginIndex = -1) or not Assigned(fIntf) then Exit;
  if fIntf.get_VisualData(Buffer,Size,Stream) = S_OK then
  begin
    Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).SampleRate := Stream.Frequency;
    Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).Channels := Stream.Channels;
    if fHasSpectrum then
    begin
      fSpectrum.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
      if fSpectrum.MixChannels then
      begin
        for i := 0 to fWaveform.NumSamples -1 do
          Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).SpectrumData[0,i] := fSpectrum.Buffer[0,i];
      end else
      begin
        for c := 0 to Stream.Channels -1 do
          for i := 0 to fWaveform.NumSamples -1 do
            Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).SpectrumData[c,i] := fSpectrum.Buffer[c,i];
      end;
    end;
    if fHasWaveform then
    begin
      fWaveform.Process(Buffer,Size,Stream.Bits,Stream.Channels,Stream.Float);
      if fWaveform.MixChannels then
      begin
        for i := 0 to fWaveform.NumSamples -1 do
          Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).WaveformData[0,i] := fWaveform.Buffer[0,i];
      end else
      begin
        for c := 0 to Stream.Channels -1 do
          for i := 0 to fWaveform.NumSamples -1 do
            Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).WaveformData[c,i] := fWaveform.Buffer[c,i];
      end;
    end;
    Items[fCurrentPlugin].fPlugin.GetModule(fCurrentPluginIndex).Render(fCurrentPluginIndex);
  end;
end;

procedure TDCVISPluginList.SetAudiofilter(Filter : IBaseFilter);
begin
  fIntf := nil;
  if Assigned(Filter) then Filter.QueryInterface(IID_IVisualInterface,fIntf);
end;
{*******************************************************************************}
destructor TDCVISPluginItem.Destroy;
begin
  ConfigClose;
  Quit;
  if fLibHandle <> 0 then FreeLibrary(fLibHandle);
  fLibHandle := 0;
  inherited Destroy;
end;

procedure TDCVISPluginItem.SetFilename(sFilename : String);
var
  Proc : function : Pointer; stdcall;
begin
  if UpperCase(sFilename) = UpperCase(fFilename) then Exit;
  fFilename := sFilename;
  if fLibHandle <> 0 then
  begin
    Quit;
    FreeLibrary(fLibHandle);
  end;
  fNumPlugins := 0;
  fCurrentIndex := -1;
  fCurrentConfig := -1;
  fPlugin := nil;
  fLibHandle := LoadLibrary(PChar(fFilename));
  if fLibHandle > 0 then
  begin
    Proc := GetProcAddress(fLibHandle,PChar(VISHeaderExport));
    fPlugin := Proc;

    while fPlugin.GetModule(fNumPlugins) <> nil do inc(fNumPlugins);

    fPlugin.hDllInstance := fLibHandle;
    SetVISOwnerWindow(fVISOwnerWindow);
    SetOwnerWindow(fOwnerWindow);
  end;
end;

function TDCVISPluginItem.GetPluginName : String;
begin
  Result := '';
  if Assigned(fPlugin) then Result := fPlugin.Description;
end;

function TDCVISPluginItem.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if (fLibHandle = 0) or (Index > fNumPlugins -1) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;
  Result := fPlugin.GetModule(Index).description;
end;

procedure TDCVISPluginItem.SetOwnerWindow(Window : hWnd);
begin
  if (fLibHandle = 0) or (fPlugin = nil) then Exit;
  fOwnerWindow := Window;
  if Assigned(fPlugin) then fPlugin.Parent := fOwnerWindow;
end;

procedure TDCVISPluginItem.SetVISOwnerWindow(Window : hWnd);
begin
  if (fLibHandle = 0) or (fPlugin = nil) then Exit;
  fVISOwnerWindow := Window;
  if Assigned(fPlugin) then fPlugin.ParentRenderer := fVISOwnerWindow;
end;

procedure TDCVISPluginItem.AboutInit;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) or
     (@fPlugin.AboutInit = nil) then Exit;
  fPlugin.AboutInit;
end;

procedure TDCVISPluginItem.AboutClose;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) or
     (@fPlugin.AboutClose = nil) then Exit;
  fPlugin.AboutClose;
end;

procedure TDCVISPluginItem.ConfigInit(Index : integer);
begin
  if (fCurrentConfig = Index) or (fLibHandle = 0) or (fNumPlugins = 0) or
     (fPlugin = nil) or (fPlugin.GetModule(Index) = nil) or
     (@fPlugin.GetModule(Index).ConfigInit = nil) then Exit;
  fPlugin.GetModule(Index).ConfigInit(Index);
  fCurrentConfig := Index;
end;

procedure TDCVISPluginItem.ConfigClose;
begin
  if (fCurrentConfig < 0) or (fLibHandle = 0) or (fNumPlugins = 0) or
     (fPlugin = nil) or (fPlugin.GetModule(Index) = nil) or
     (@fPlugin.GetModule(fCurrentConfig).ConfigClose = nil) then Exit;
  fPlugin.GetModule(fCurrentConfig).ConfigClose(fCurrentConfig);
  fCurrentConfig := -1;
end;

function fWNDProc(Handle : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; stdcall;
begin
  if Assigned(gItem) then
  begin
    if not gItem.VISWndProc(Handle,message,wParam,lParam)
      then Result := CallWindowProc(gWndProcOld, Handle, message, wParam, lParam)
      else Result := 0;
  end else Result := CallWindowProc(gWndProcOld, Handle, message, wParam, lParam);
end;

procedure TDCVISPluginItem.Init(Index : integer; Width : integer; Height : integer);
begin
  if (fCurrentIndex = Index) or (fLibHandle = 0) or (Index > fNumPlugins -1) or
     (fPlugin = nil) or (fPlugin.GetModule(Index) = nil) then Exit;
  Quit;
  fVISWindow := fPlugin.GetModule(Index).Init(Index,Width,Height);

  if fVISWindow < 0 then Exit;

  if fVISWindow > 0 then
  begin
    gWndProcOld := Pointer(GetWindowLong(fVISWindow,GWL_WNDPROC));
    SetWindowLong(fVISWindow,GWL_WNDPROC,integer(@fWNDProc));
  end;
    
  gItem := Self;
  fCurrentIndex := Index;
  SetMediaName(fMediaName);
  fOwner.CurrentPluginIndex := Index;
  fOwner.CurrentPlugin := ID;
  if fPlugin.GetModule(Index).FramesPerSec = 0
    then fOwner.fTimer.Interval := 1000 div 25
    else fOwner.fTimer.Interval := 1000 div fPlugin.GetModule(Index).FramesPerSec;
  fOwner.fHasSpectrum := fPlugin.GetModule(Index).SpectrumChan > 0;
  fOwner.fHasWaveform := fPlugin.GetModule(Index).WaveformChan > 0;
  fOwner.fSpectrum.MixChannels := Boolean(fPlugin.GetModule(Index).MixSpectrum);
  fOwner.fWaveform.MixChannels := Boolean(fPlugin.GetModule(Index).MixWaveform);
  fOwner.fTimer.Enabled := True;
end;

procedure TDCVISPluginItem.Quit;
begin
  if (fCurrentIndex = -1) or (fLibHandle = 0) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;

  if Assigned(gWndProcOld) and (fVISWindow <> 0) then
  begin
    SetWindowLong(fVISWindow,GWL_WNDPROC,integer(gWndProcOld));
    gWndProcOld := nil;
    fVISWindow := 0;
  end;

  gItem := nil;

  fOwner.fHasSpectrum := False;
  fOwner.fHasWaveform := False;
  if Assigned(fOwner.fTimer) then fOwner.fTimer.Enabled := False;
  fOwner.CurrentPlugin := -1;
  fOwner.CurrentPluginIndex := -1;

  fPlugin.GetModule(fCurrentIndex).Quit(fCurrentIndex);
  fCurrentIndex := -1;
end;

procedure TDCVISPluginItem.Resize(Width : integer; Height : integer);
begin
  if (fCurrentIndex = -1) or (fLibHandle = 0) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;
  fPlugin.GetModule(fCurrentIndex).Resize(fCurrentIndex,Width,Height);
end;

procedure TDCVISPluginItem.SetMediaName(Name : String);
begin
  fMediaName := Name;
  if (fCurrentIndex = -1) or (fLibHandle = 0) or (fPlugin = nil) or
     (@fPlugin.GetModule(fCurrentIndex).SetMediaName = nil) then Exit;
  fPlugin.GetModule(fCurrentIndex).SetMediaName(PChar(fMediaName));
end;

function TDCVISPluginItem.VISWndProc(Handle : HWND; message : UINT; wParam : WPARAM; lParam : LPARAM) : Boolean;
begin
  Result := False;
  if (fVISOwnerWindow = 0) or InSendMessage then Exit;

  case message of
    WM_CHAR,
    WM_DEADCHAR,
    WM_KEYDOWN,
    WM_KEYUP,
    WM_LBUTTONDBLCLK,
    WM_LBUTTONDOWN,
    WM_LBUTTONUP,
    WM_MBUTTONDBLCLK,
    WM_MBUTTONDOWN,
    WM_MBUTTONUP,
    WM_MOUSEACTIVATE,
    WM_MOUSEMOVE,
    // If we pass this on we don't get any mouse clicks
    // WM_NCHITTEST,
    WM_NCLBUTTONDBLCLK,
    WM_NCLBUTTONDOWN,
    WM_NCLBUTTONUP,
    WM_NCMBUTTONDBLCLK,
    WM_NCMBUTTONDOWN,
    WM_NCMBUTTONUP,
    WM_NCMOUSEMOVE,
    WM_NCRBUTTONDBLCLK,
    WM_NCRBUTTONDOWN,
    WM_NCRBUTTONUP,
    WM_RBUTTONDBLCLK,
    WM_RBUTTONDOWN,
    WM_RBUTTONUP,
    WM_SYSCHAR,
    WM_SYSDEADCHAR,
    WM_SYSKEYDOWN,
    WM_SYSKEYUP:
    begin
      PostMessage(fVISOwnerWindow, message, wParam, lParam);
      Result := True;
      Exit;
    end;
  end;
end;

end.
