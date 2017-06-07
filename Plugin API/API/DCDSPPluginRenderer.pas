
    (*********************************************************************
     *  DCDSPPluginRenderer.pas                                          *
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

unit DCDSPPluginRenderer;

interface

uses
  SysUtils, Windows, Classes, DCDSPPluginAPI, MMSystem, dmoConst, waUtils;

const
  DSPHeaderExport = 'DCDSPPluginGetHeader';

type
  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array[0..High(integer) -1] of Boolean;

  TDCDSPPluginItem  = class(TCollectionItem)
  private
    fEnabled : PBooleanArray;
    fLibHandle : hWnd;
    fFilename : String;
    fNumPlugins : integer;
    fPlugin : PDCDSPPluginHeader;
    fOwnerWindow : hWnd;
    fWidth : integer;
    fHeight : integer;
    fFormat : TWaveFormatEx;
    procedure SetFilename(sFilename : String);
    function GetPluginName : String;
    function GetSubPluginName(Index : integer) : String;
    procedure SetOwnerWindow(Window : hWnd);
    function GetEnabled(Index : integer) : Boolean;
  public
    destructor Destroy; override;
    property SubPluginName[Index : integer] : String read GetSubPluginName;
    procedure Init(Index : integer);
    procedure Quit(Index : integer);
    procedure AboutInit;
    procedure AboutClose;
    procedure ConfigInit(Index : integer);
    procedure ConfigClose(Index : integer);
    function Process(Buffer : Pointer; Size : integer) : integer;
    procedure Flush;
    property Enabled[Index : integer] : Boolean read GetEnabled;
  published
    property Count : integer read fNumPlugins;
    property Filename : String read fFilename write SetFilename;
    property PluginName : String read GetPluginName;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
    property Format : TWaveFormatEx read fFormat write fFormat;
    property Width : integer read fWidth write fWidth;
    property Height : integer read fHeight write fHeight;
  end;

  TDCDSPPluginList = class(TCollection)
  private
    fDirectory : String;
    fOwnerWindow : hWnd;
    fFormat : TWaveFormatEx;
    function  GetItem(Index : Integer) : TDCDSPPluginItem;
    procedure SetItem(Index : Integer; Value : TDCDSPPluginItem);
    procedure SetDirectory(Dir : String);
    procedure SetOwnerWindow(Window : hWnd);
    function IsOurPlugin(Filename : String) : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add : TDCDSPPluginItem;
    property Items[Index: Integer]: TDCDSPPluginItem read GetItem write SetItem; default;
    procedure Init(SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
    function Process(Buffer : Pointer; Size : integer) : integer;
    procedure Flush;
  published
    property Directory : String read fDirectory write SetDirectory;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

implementation

constructor TDCDSPPluginList.Create;
begin
  inherited Create(TDCDSPPluginItem);
end;

destructor TDCDSPPluginList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDCDSPPluginList.Add : TDCDSPPluginItem;
begin
  Result := TDCDSPPluginItem(inherited Add);
end;

function TDCDSPPluginList.GetItem(Index : Integer) : TDCDSPPluginItem;
begin
  Result := TDCDSPPluginItem(inherited GetItem(Index));
end;

procedure TDCDSPPluginList.SetItem(Index : Integer; Value : TDCDSPPluginItem);
begin
  inherited SetItem(Index, Value);
end;

function TDCDSPPluginList.IsOurPlugin(Filename : String) : Boolean;
var
  DLL : hWnd;
  Proc : function : Pointer; stdcall;
  Plugin : PDCDSPPluginHeader;
  NumPlugins : integer;
begin
  Result := False;
  DLL := LoadLibrary(PChar(Filename));
  if DLL <> 0 then
  begin
    Proc := GetProcAddress(DLL,PChar(DSPHeaderExport));
    if @Proc <> nil then
    begin
      Plugin := Proc;
      if Plugin.Version = DCSDKVerDSP then
      begin
        NumPlugins := 0;
        while Plugin.GetModule(NumPlugins) <> nil do inc(NumPlugins);
        Result := NumPlugins > 0;
      end;
    end;
    FreeLibrary(DLL);
  end;
end;

procedure TDCDSPPluginList.SetDirectory(Dir : String);
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
      Items[Count -1].fOwnerWindow := fOwnerWindow;
      Items[Count -1].Filename := fDirectory + sr.cFileName;
      Items[Count -1].Format := fFormat;
    end;
  until FindNextFile(srh,sr) = False;
  FindClose(srh);
end;

procedure TDCDSPPluginList.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  fOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].OwnerWindow := fOwnerWindow;
end;

procedure TDCDSPPluginList.Init(SampleRate : integer; Bits : Byte; Channels : Byte; Float : Boolean);
var
  i : integer;
begin
  fFormat.nChannels := Channels;
  fFormat.nSamplesPerSec := SampleRate;
  fFormat.wBitsPerSample := Bits;
  if Float then fFormat.wFormatTag := WAVE_FORMAT_IEEE_FLOAT
           else fFormat.wFormatTag := WAVE_FORMAT_PCM;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].fFormat := fFormat;
end;

function TDCDSPPluginList.Process(Buffer : Pointer; Size : integer) : integer;
var
  i : integer;
begin
  Result := Size;
  if Count > 0 then
    for i := 0 to Count -1 do Result := Items[i].Process(Buffer,Result);
end;

procedure TDCDSPPluginList.Flush;
var
  i : integer;
begin
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].Flush;
end;
{*******************************************************************************}
destructor TDCDSPPluginItem.Destroy;
var
  i : integer;
begin
  if Assigned(fEnabled) then
  begin
    for i := 0 to fNumPlugins -1 do
      if fEnabled[i] then
      begin
        fEnabled[i] := False;
        if Assigned(fPlugin) and Assigned(fPlugin.GetModule(i))
          then fPlugin.GetModule(i).Quit(i);
      end;
    FreeMem(fEnabled);  
  end;
  if fLibHandle <> 0 then FreeLibrary(fLibHandle);
  fLibHandle := 0;
  inherited Destroy;
end;

procedure TDCDSPPluginItem.SetFilename(sFilename : String);
var
  Proc : function : Pointer; stdcall;
  i : integer;
begin
  if UpperCase(sFilename) = UpperCase(fFilename) then Exit;
  fFilename := sFilename;
  if fLibHandle <> 0 then
  begin
    if Assigned(fEnabled) then
    begin
      for i := 0 to fNumPlugins -1 do
        if fEnabled[i] then
        begin
          fEnabled[i] := False;
          if Assigned(fPlugin) and Assigned(fPlugin.GetModule(i))
            then fPlugin.GetModule(i).Quit(i);
        end;
      FreeMemory(fEnabled);
      fEnabled := nil;
    end;
    FreeLibrary(fLibHandle);
  end;
  fNumPlugins := 0;
  fPlugin := nil;
  fLibHandle := LoadLibrary(PChar(fFilename));
  if fLibHandle > 0 then
  begin
    Proc := GetProcAddress(fLibHandle,PChar(DSPHeaderExport));

    if Proc = nil then
    begin
      FreeLibrary(fLibHandle);
      fLibHandle := 0;
      Exit;
    end;

    fPlugin := Proc;

    while fPlugin.GetModule(fNumPlugins) <> nil do inc(fNumPlugins);
    fEnabled := AllocMem(fNumPlugins * SizeOf(Boolean));
    
    fPlugin.hDllInstance := fLibHandle;
    SetOwnerWindow(fOwnerWindow);
  end;
end;

function TDCDSPPluginItem.GetPluginName : String;
begin
  Result := '';
  if Assigned(fPlugin) then Result := fPlugin.Description;
end;

function TDCDSPPluginItem.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if (fLibHandle = 0) or (Index > fNumPlugins -1) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;
  Result := fPlugin.GetModule(Index).description;
end;

procedure TDCDSPPluginItem.SetOwnerWindow(Window : hWnd);
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) then Exit;
  fOwnerWindow := Window;
  if Assigned(fPlugin) then fPlugin.Parent := fOwnerWindow;
end;

procedure TDCDSPPluginItem.Init(Index : integer);
begin
  if (fLibHandle = 0) or (Index > fNumPlugins -1) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) or fEnabled[Index] then Exit;
  fEnabled[Index] := fPlugin.GetModule(Index).Init(Index) = S_OK;
end;

procedure TDCDSPPluginItem.Quit(Index : integer);
begin
  if (fLibHandle = 0) or (Index > fNumPlugins -1) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) or not fEnabled[Index] then Exit;
  fPlugin.GetModule(Index).Quit(Index);
  fEnabled[Index] := False;
end;

function TDCDSPPluginItem.Process(Buffer : Pointer; Size : integer) : integer;
var
  i : integer;
begin
  Result := Size;
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) or
     (fFormat.wBitsPerSample = 0) or (fFormat.nChannels = 0) then Exit;
  for i := 0 to fNumPlugins -1 do
    if fEnabled[i] then
    begin
      if Assigned(fPlugin.GetModule(i)) then
        Result := fPlugin.GetModule(i).ModifySamples(i,Buffer,Result,fFormat.nChannels,
          fFormat.wBitsPerSample,fFormat.nSamplesPerSec,fFormat.wFormatTag = WAVE_FORMAT_IEEE_FLOAT);
    end;
end;

procedure TDCDSPPluginItem.AboutInit;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) then Exit;
  fPlugin.AboutInit;
end;

procedure TDCDSPPluginItem.AboutClose;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) then Exit;
  fPlugin.AboutClose;
end;

procedure TDCDSPPluginItem.ConfigInit(Index : integer);
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;
  fPlugin.GetModule(Index).ConfigInit(Index);
end;

procedure TDCDSPPluginItem.ConfigClose(Index : integer);
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fPlugin = nil) or
     (fPlugin.GetModule(Index) = nil) then Exit;
  fPlugin.GetModule(Index).ConfigClose(Index);
end;

function TDCDSPPluginItem.GetEnabled(Index : integer) : Boolean;
begin
  Result := fEnabled[Index];
end;

procedure TDCDSPPluginItem.Flush;
var
  i : integer;
begin
  if (fLibHandle = 0) or (fNumPlugins = 0) or (fFormat.wBitsPerSample = 0) or
     (fFormat.nChannels = 0) then Exit;
  for i := 0 to fNumPlugins -1 do
    if fEnabled[i] and (fPlugin.GetModule(i) <> nil) then
      fPlugin.GetModule(i).Flush;
end;

end.
