
    (*********************************************************************
     *  waDSPThreadedWrapper.pas                                         *
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
  @abstract(Threaded Version of the Wrapper Component for Winamp 2 DSP Plugins.)
  @author(Milenko Mitrovic <dcoder@dsp-worx.de>)
  @created(Sep 01, 2002)
  @lastmod(Apr 02, 2004)
}

unit waDSPThreadedWrapper;

interface

uses
  Classes, Windows, waConst, Messages, SysUtils, dspConst, waUtils;

type
  {@exclude}
  TDCWADSPThreadedPluginItem = class;

  {@exclude}
  TDCWADSPThreadedPluginItemThread = class(TThread)
  private
    fPluginName : String;
    fSubPluginNames : TStringList;
    fPluginCount : integer;
    fCurrentIndex : integer;
    fLibHandle : hWnd;
    fOwner : TDCWADSPThreadedPluginItem;
    fPlugin : PWinampDSPModule;
    fHeader : PWinampDSPHeader;
    function GetSubPluginName(Index : integer) : String;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TDCWADSPThreadedPluginItem);
    property SubPluginName[Index : integer] : String read GetSubPluginName;
  published
    property Count : integer read fPluginCount;
    property CurrentIndex : integer read fCurrentIndex;
    property PluginName : String read fPluginName;
  end;

  {@exclude}
  TDCWADSPThreadedPluginItem = class(TCollectionItem)
  private
    fLoading : Boolean;
    fOwnerWindow : hWnd;
    fFileName : String;
    fPriority : TThreadPriority;
    fEnabled : Boolean;
    fThread : TDCWADSPThreadedPluginItemThread;
    fThreadRunning : Boolean;
    function GetPriority : TThreadPriority;
    procedure SetPriority(aPriority : TThreadPriority);
    procedure SetFileName(aFileName : String);
    procedure ClearThread;
    function GetSubPluginName(Index : integer) : String;
    function GetPluginName : String;
    function GetPluginCount : integer;
    function GetPluginCurrentIndex : integer;
    procedure SetOwnerWindow(aOwner : hWnd);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Init(Index : integer);
    procedure Config;
    procedure Quit;
    function Process(Buffer : Pointer; Size : integer; Channels : integer; Bits : integer; Frequency : integer; Float : Boolean) : integer;
    property SubPluginName[Index : integer] : String read GetSubPluginName;
    property PluginName : String read GetPluginName;
    property PluginCount : integer read GetPluginCount;
    property PluginCurrentIndex : integer read GetPluginCurrentIndex;
  published
    property Enabled : Boolean read fEnabled write fEnabled;
    property Priority : TThreadPriority read GetPriority write SetPriority;
    property Filename : String read fFileName write SetFileName;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

  {@exclude}
  TDCWADSPThreadedPluginList = class(TCollection)
  private
    fDirectory : String;
    fOwnerWindow : hWnd;
    fStream : TDSStream;
    function  GetItem(Index : Integer) : TDCWADSPThreadedPluginItem;
    procedure SetItem(Index : Integer; Value : TDCWADSPThreadedPluginItem);
    procedure SetDirectory(Dir : String);
    procedure SetOwnerWindow(Window : hWnd);
    function IsOurPlugin(Filename : String) : Boolean;
    procedure SetEnabled(aIndex : integer; aEnabled : Boolean);
    function GetEnabled(aIndex : integer) : Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add : TDCWADSPThreadedPluginItem;
    property Items[Index: Integer]: TDCWADSPThreadedPluginItem read GetItem write SetItem; default;
    procedure Init(Stream : PDSStream);
    procedure Quit;
    function Process(Buffer : Pointer; Size : integer) : integer;
    property Enabled[Index : integer] : Boolean read GetEnabled write SetEnabled;
  published
    property Directory : String read fDirectory write SetDirectory;
    property OwnerWindow : hWnd read fOwnerWindow write SetOwnerWindow;
  end;

  { TDCWADSPThreadedWrapper - Winamp2 DSP Plugin Wrapper Component. }
  TDCWADSPThreadedWrapper = class(TComponent)
  private
    {@exclude}
    fPlugins : TDCWADSPThreadedPluginList;
  public
    { TDCWADSPThreadedWrapper constructor. }
    constructor Create(AOwner: TComponent); override;
    { TDCWADSPThreadedWrapper destructor }
    destructor Destroy; override;
  published
    { The core of the Wrapper. Use the Plugins Property to Start/Stop ... Plugins. }
    property Plugins : TDCWADSPThreadedPluginList read fPlugins write fPlugins;
  end;

implementation
{*** TDCWADSPThreadedPluginItemThread *************************************************}
constructor TDCWADSPThreadedPluginItemThread.Create(AOwner : TDCWADSPThreadedPluginItem);
begin
  fOwner := AOwner;
  fPluginCount := 0;
  fCurrentIndex := -1;
  fPluginName := '';
  fSubPluginNames := TStringList.Create;
  inherited Create(False);
end;

function TDCWADSPThreadedPluginItemThread.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if (Index > fPluginCount -1) or (Index > fSubPluginNames.Count -1) then Exit;
  Result := fSubPluginNames[Index];
end;

procedure TDCWADSPThreadedPluginItemThread.Execute;
var
  aMsg : TMsg;
  aProc : function : Pointer; stdcall;
begin
  PeekMessage(aMsg,0,0,0,PM_NOREMOVE);

  fOwner.fThreadRunning := True;

  while not Terminated do
  begin
    if not PeekMessage(aMsg,0,0,0,PM_REMOVE) then
    begin
      WaitForSingleObject(Handle,1);
      Continue;
    end;

    case aMsg.message of
      UM_DC_LOAD:
      begin
        fSubPluginNames.Clear;
        fPluginName := '';
        fPluginCount := 0;
        fCurrentIndex := -1;
        fLibHandle := LoadLibrary(PChar(aMsg.wParam));
        if fLibHandle <> 0 then
        begin
          aProc := GetProcAddress(fLibHandle,PChar('winampDSPGetHeader2'));
          if @aProc <> nil then
          begin
            fHeader := aProc;
            if fHeader.Version <> 32 then
            begin
              FreeLibrary(fLibHandle);
              break;
            end;
            fPluginName := StrPas(fHeader.Description);
            while fHeader.GetModule(fPluginCount) <> nil do
            begin
              fSubPluginNames.Add(fHeader.GetModule(fPluginCount).description);
              inc(fPluginCount);
            end;
          end;
        end;
        fOwner.fLoading := False;
      end;
      UM_DC_FREE:
      begin
        if fLibHandle <> 0 then
        begin
          if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
          fSubPluginNames.Clear;
          fPlugin := nil;
          fCurrentIndex := -1;
          FreeLibrary(fLibHandle);
          fLibHandle := 0;
        end;
        break;
      end;
      UM_DC_INIT:
      begin
        if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
        fPlugin := fHeader.GetModule(aMsg.wParam);
        fPlugin.hMainWindow := fOwner.fOwnerWindow;
        fPlugin.hDllInstance := fLibHandle;
        fPlugin.Init(fPlugin);
        fCurrentIndex := aMsg.wParam;
      end;
      UM_DC_QUIT:
      begin
        if fCurrentIndex > -1 then fPlugin.Quit(fPlugin);
        fPlugin := nil;
        fCurrentIndex := -1;
      end;
      UM_DC_CONFIG:
      begin
        if fCurrentIndex > -1 then fPlugin.Config(fPlugin);
      end else
      begin
        TranslateMessage(aMsg);
        DispatchMessage(aMsg);
      end;
    end;
  end;
  FreeAndNil(fSubPluginNames);
  fOwner.fThreadRunning := False;
end;
{*** TDCWADSPThreadedPluginItem *******************************************************}
constructor TDCWADSPThreadedPluginItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fPriority := tpNormal;
  fEnabled := False;
  fOwnerWindow := GetDesktopWindow;
end;

destructor TDCWADSPThreadedPluginItem.Destroy;
begin
  ClearThread;
  inherited Destroy;
end;

function TDCWADSPThreadedPluginItem.GetPriority : TThreadPriority;
begin
  if fThreadRunning then Result := fThread.Priority
                    else Result := fPriority;
end;

procedure TDCWADSPThreadedPluginItem.SetPriority(aPriority : TThreadPriority);
begin
  fPriority := aPriority;
  if fThreadRunning then fThread.Priority := fPriority;
end;

procedure TDCWADSPThreadedPluginItem.SetFileName(aFileName : String);
begin
  ClearThread;
  fFileName := aFileName;
  fThread := TDCWADSPThreadedPluginItemThread.Create(Self);
  while not fThreadRunning do;
  fThread.Priority := fPriority;
  fLoading := True;
  PostThreadMessage(fThread.ThreadID,UM_DC_LOAD,integer(PChar(fFileName)),0);
  while fLoading do;
end;

procedure TDCWADSPThreadedPluginItem.ClearThread;
begin
  if fThreadRunning then
  begin
    PostThreadMessage(fThread.ThreadID,UM_DC_FREE,0,0);
    while fThreadRunning do;
  end;
end;

procedure TDCWADSPThreadedPluginItem.Init(Index : integer);
begin
  if not fThreadRunning or (Index < 0) or (Index > fThread.Count -1) then Exit;
  Quit;
  PostThreadMessage(fThread.ThreadID,UM_DC_INIT,Index,0);
end;

procedure TDCWADSPThreadedPluginItem.Config;
begin
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  PostThreadMessage(fThread.ThreadID,UM_DC_CONFIG,0,0);
end;

procedure TDCWADSPThreadedPluginItem.Quit;
begin
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  PostThreadMessage(fThread.ThreadID,UM_DC_QUIT,0,0);
end;

function TDCWADSPThreadedPluginItem.Process(Buffer : Pointer; Size : integer; Channels : integer; Bits : integer; Frequency : integer; Float : Boolean) : integer;
begin
  Result := Size;
  if not fEnabled then Exit;
  if Float or not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  if (Bits = 0) or (Channels = 0) then Exit;
  Result := fThread.fPlugin.ModifySamples(fThread.fPlugin,Buffer,Size div (Bits div 8) div Channels,Bits,Channels,Frequency) * (Bits div 8) * Channels;
end;

function TDCWADSPThreadedPluginItem.GetSubPluginName(Index : integer) : String;
begin
  Result := '';
  if not fThreadRunning then Exit;
  Result := fThread.SubPluginName[Index];
end;

function TDCWADSPThreadedPluginItem.GetPluginName : String;
begin
  Result := '';
  if not fThreadRunning then Exit;
  Result := fThread.PluginName;
end;

function TDCWADSPThreadedPluginItem.GetPluginCount : integer;
begin
  Result := 0;
  if not fThreadRunning then Exit;
  Result := fThread.Count;
end;

function TDCWADSPThreadedPluginItem.GetPluginCurrentIndex : integer;
begin
  Result := -1;
  if not fThreadRunning then Exit;
  Result := fThread.CurrentIndex;
end;

procedure TDCWADSPThreadedPluginItem.SetOwnerWindow(aOwner : hWnd);
begin
  fOwnerWindow := aOwner;
  if not fThreadRunning or (fThread.CurrentIndex < 0) then Exit;
  fThread.fPlugin.hMainWindow := fOwnerWindow;
end;
{*** TDCWADSPThreadedPluginList *******************************************************}
constructor TDCWADSPThreadedPluginList.Create;
begin
  inherited Create(TDCWADSPThreadedPluginItem);
  fOwnerWindow := GetDesktopWindow;
end;

destructor TDCWADSPThreadedPluginList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function  TDCWADSPThreadedPluginList.GetItem(Index : Integer) : TDCWADSPThreadedPluginItem;
begin
  Result := TDCWADSPThreadedPluginItem(inherited GetItem(Index));
end;

procedure TDCWADSPThreadedPluginList.SetItem(Index : Integer; Value : TDCWADSPThreadedPluginItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TDCWADSPThreadedPluginList.SetDirectory(Dir : String);
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
      Items[Count -1].OwnerWindow := fOwnerWindow;
    end;
  until FindNextFile(srh,sr) = False;
  Windows.FindClose(srh);
end;

procedure TDCWADSPThreadedPluginList.SetOwnerWindow(Window : hWnd);
var
  i : integer;
begin
  fOwnerWindow := Window;
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].OwnerWindow := fOwnerWindow;
end;

function TDCWADSPThreadedPluginList.IsOurPlugin(Filename : String) : Boolean;
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

function TDCWADSPThreadedPluginList.Add : TDCWADSPThreadedPluginItem;
begin
  Result := TDCWADSPThreadedPluginItem(inherited Add);
end;

procedure TDCWADSPThreadedPluginList.Init(Stream : PDSStream);
begin
  fStream := Stream^;
end;

function TDCWADSPThreadedPluginList.Process(Buffer : Pointer; Size : integer) : integer;
var
  i : integer;
begin
  Result := Size;
  if Count > 0 then
    for i := 0 to Count -1 do Result := Items[i].Process(Buffer,Result,fStream.Channels,fStream.Bits,fStream.Frequency,fStream.Float);
end;

procedure TDCWADSPThreadedPluginList.Quit;
var
  i : integer;
begin
  if Count > 0 then
    for i := 0 to Count -1 do Items[i].Quit;
end;

procedure TDCWADSPThreadedPluginList.SetEnabled(aIndex : integer; aEnabled : Boolean);
begin
  if (aIndex < 0) or (aIndex > Count -1) then Exit;
  Items[aIndex].Enabled := aEnabled;
end;

function TDCWADSPThreadedPluginList.GetEnabled(aIndex : integer) : Boolean;
begin
  Result := False;
  if (aIndex < 0) or (aIndex > Count -1) then Exit;
  Result := Items[aIndex].Enabled;
end;
{*** TDCWADSPThreadedWrapper **********************************************************}
constructor TDCWADSPThreadedWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlugins := TDCWADSPThreadedPluginList.Create;
end;

destructor TDCWADSPThreadedWrapper.Destroy;
begin
  fPlugins.Free;
  inherited Destroy;
end;

end.
