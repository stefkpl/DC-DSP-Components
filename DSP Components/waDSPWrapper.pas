
    (*********************************************************************
     *  waDSPWrapper.pas                                                 *
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
  @abstract(Wrapper Component for Winamp 2 DSP Plugins.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit waDSPWrapper;

interface

uses
  Windows, Classes, dspConst, dspUtils, SysUtils, waConst, waUtils, MMSystem;

type
  {@exclude}
  TDCWADSPPluginItem  = class(TCollectionItem)
  private
    fLibHandle : hWnd;
    fFilename : String;
    fCurrentIndex : SmallInt;
    fNumPlugins : Byte;
    fModule : PWinampDSPModule;
    fPlugin : PWinampDSPHeader;
    fOwnerWindow : hWnd;
    fFormat : TWaveFormatEx;
    procedure SetFilename(sFilename : String);
    function GetPluginName : String;
    function GetSubPluginName(Index : Byte) : String;
    procedure SetOwnerWindow(Window : hWnd);
  public
    destructor Destroy; override;
    property SubPluginName[Index : Byte] : String read GetSubPluginName;
    procedure Config;
    procedure Init(Index : byte);
    procedure Quit;
    function Process(Buffer : Pointer; Size : integer) : integer;
  published
    property Count : Byte read fNumPlugins;
    property Filename : String read fFilename write SetFilename;
    property PluginName : String read GetPluginName;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
    property Format : TWaveFormatEx read fFormat write fFormat;
  end;

  {@exclude}
  TDCWADSPPluginList = class(TCollection)
  private
    fDirectory : String;
    fOwnerWindow : hWnd;
    fFormat : TWaveFormatEx;
    function  GetItem(Index : Integer) : TDCWADSPPluginItem;
    procedure SetItem(Index : Integer; Value : TDCWADSPPluginItem);
    procedure SetDirectory(Dir : String);
    procedure SetOwnerWindow(Window : hWnd);
    function IsOurPlugin(Filename : String) : Boolean;
  public
    constructor Create; virtual;
    function Add : TDCWADSPPluginItem;
    property Items[Index: Integer]: TDCWADSPPluginItem read GetItem write SetItem; default;
    procedure Init(Stream : PDSStream);
    procedure Quit;
    function Process(Buffer : Pointer; Size : integer) : integer;
  published
    property Directory : String read fDirectory write SetDirectory;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

  { TDCWADSPWrapper - Winamp2 DSP Plugin Wrapper Component. }
  TDCWADSPWrapper = class(TComponent)
  private
    {@exclude}
    fPlugins : TDCWADSPPluginList;
  public
    { TDCWADSPWrapper constructor. }
    constructor Create(AOwner: TComponent); override;
    { TDCWADSPWrapper destructor }
    destructor Destroy; override;
  published
    { The core of the Wrapper. Use the Plugins Property to Start/Stop ... Plugins. }
    property Plugins : TDCWADSPPluginList read fPlugins write fPlugins;
  end;

implementation

constructor TDCWADSPWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlugins := TDCWADSPPluginList.Create;
end;

destructor TDCWADSPWrapper.Destroy;
begin
  fPlugins.Free;
  inherited Destroy;
end;

constructor TDCWADSPPluginList.Create;
begin
  inherited Create(TDCWADSPPluginItem);
end;

function TDCWADSPPluginList.Add : TDCWADSPPluginItem;
begin
  Result := TDCWADSPPluginItem(inherited Add);
end;

function TDCWADSPPluginList.GetItem(Index : Integer) : TDCWADSPPluginItem;
begin
  Result := TDCWADSPPluginItem(inherited GetItem(Index));
end;

procedure TDCWADSPPluginList.SetItem(Index : Integer; Value : TDCWADSPPluginItem);
begin
  inherited SetItem(Index, Value);
end;

function TDCWADSPPluginList.IsOurPlugin(Filename : String) : Boolean;
var
  DLL : hWnd;
  Proc : function : Pointer; stdcall;
  Plugin : PWinampDSPHeader;
  NumPlugins : integer;
begin
  Result := False;
  DLL := LoadLibrary(PChar(Filename));
  if DLL <> 0 then
  begin
    Proc := GetProcAddress(DLL,PChar('winampDSPGetHeader2'));
    if @Proc <> nil then
    begin
      Plugin := Proc;
      if Plugin.Version = 32 then
      begin
        NumPlugins := 0;
        while Plugin.GetModule(NumPlugins) <> nil do inc(NumPlugins);
        Result := NumPlugins > 0;
      end;
    end;
    FreeLibrary(DLL);
  end;
end;

procedure TDCWADSPPluginList.SetDirectory(Dir : String);
var
  srh: THandle;
  sr : TWin32FindData;
begin
  Dir := AddBackSlash(Dir);
  if UpperCase(Dir) = UpperCase(fDirectory) then Exit;
  fDirectory := Dir;
  Clear;

  srh := FindFirstFile(PChar(fDirectory + 'dsp_*.dll'),sr);
  if srh <> INVALID_HANDLE_VALUE then
  repeat
    if IsOurPlugin(fDirectory + sr.cFileName) then
    begin
      Add;
      Items[Count -1].Filename := fDirectory + sr.cFileName;
      Items[Count -1].Format := fFormat;
      Items[Count -1].fOwnerWindow := fOwnerWindow;
    end;
  until FindNextFile(srh,sr) = False;
  Windows.FindClose(srh);
end;

procedure TDCWADSPPluginList.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  if Window = fOwnerWindow then Exit;
  fOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].OwnerWindow := fOwnerWindow;
end;

procedure TDCWADSPPluginList.Init(Stream : PDSStream);
var
  i : integer;
begin
  fFormat.nChannels := Stream.Channels;
  fFormat.nSamplesPerSec := Stream.Frequency;
  fFormat.wBitsPerSample := Stream.Bits;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].fFormat := fFormat;
end;

procedure TDCWADSPPluginList.Quit;
var
  i : integer;
begin
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].Quit;
end;

function TDCWADSPPluginList.Process(Buffer : Pointer; Size : integer) : integer;
var
  i : integer;
begin
  Result := Size;
  if Count > 0 then
    for i := 0 to Count -1 do Result := Items[i].Process(Buffer,Result);
end;
{*******************************************************************************}
destructor TDCWADSPPluginItem.Destroy;
begin
  Quit;
  if fLibHandle <> 0 then FreeLibrary(fLibHandle);
  inherited Destroy;
end;

procedure TDCWADSPPluginItem.SetFilename(sFilename : String);
var
  Proc : function : Pointer; stdcall;
  i : integer;
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
  fPlugin := nil;
  fLibHandle := LoadLibrary(PChar(fFilename));
  if fLibHandle > 0 then
  begin
    Proc := GetProcAddress(fLibHandle,PChar('winampDSPGetHeader2'));

    if Proc = nil then
    begin
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
      Exit;
    end;

    fPlugin := Proc;
    if fPlugin.Version <> 32 then
    begin
      fPlugin := nil;
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
      Exit;
    end;

    for i := 0 to 255 do
    begin
      if fPlugin.GetModule(i) = nil then
      begin
        fNumPlugins := i;
        break;
      end;
      fPlugin.GetModule(i).hDllInstance := fLibHandle;
    end;
    SetOwnerWindow(fOwnerWindow);
  end;
end;

function TDCWADSPPluginItem.GetPluginName : String;
begin
  Result := '';
  if Assigned(fPlugin) then Result := fPlugin.Description;
end;

function TDCWADSPPluginItem.GetSubPluginName(Index : Byte) : String;
begin
  Result := '';
  if (fLibHandle = 0) or (Index > fNumPlugins -1) or (fPlugin.GetModule(Index) = nil) then Exit;
  Result := fPlugin.GetModule(Index).description;
end;

procedure TDCWADSPPluginItem.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) then Exit;
  fOwnerWindow := Window;
  for i := 0 to fNumPlugins -1 do fPlugin.GetModule(i).hMainWindow := fOwnerWindow;
end;

procedure TDCWADSPPluginItem.Config;
begin
  if (fLibHandle = 0) or (fCurrentIndex > fNumPlugins -1) or (fPlugin.GetModule(fCurrentIndex) = nil) then Exit;
  fPlugin.GetModule(Index).Config(fPlugin.GetModule(fCurrentIndex));
end;

procedure TDCWADSPPluginItem.Init(Index : byte);
begin
  if (fLibHandle = 0) or (Index = fCurrentIndex) or (Index > fNumPlugins -1) or (fPlugin.GetModule(Index) = nil) then Exit;
  Quit;
  fModule := fPlugin.GetModule(Index);
  fModule.hMainWindow := fOwnerWindow;
  fModule.hDllInstance := fLibHandle;
  fModule.Init(fModule);
  fCurrentIndex := Index;
end;

procedure TDCWADSPPluginItem.Quit;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fCurrentIndex < 0) then Exit;
  fModule.Quit(fModule);
  fCurrentIndex := -1;
end;

function TDCWADSPPluginItem.Process(Buffer : Pointer; Size : integer) : integer;
var
  nSamples : integer;
begin
  Result := Size;
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fCurrentIndex < 0) then Exit;
  if (fFormat.wBitsPerSample = 0) or (fFormat.nChannels = 0) then Exit;
  nSamples := Size div (fFormat.wBitsPerSample div 8) div fFormat.nChannels;
  Result := fModule.ModifySamples(fModule,Buffer,nSamples,fFormat.wBitsPerSample,fFormat.nChannels,fFormat.nSamplesPerSec) * (fFormat.wBitsPerSample div 8) * fFormat.nChannels;
end;

end.
