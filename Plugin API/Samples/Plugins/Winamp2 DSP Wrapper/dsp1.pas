unit dsp1;

interface

uses
  Windows, DCDSPPluginAPI, Forms, formWA2DSPPlugins, formAbout, Graphics,
  SysUtils, Dialogs, waDSPWrapper, Inifiles, dspConst;

  function GetModule(Index : integer) : PDCDSPModule; stdcall;
  function DCDSPPluginGetHeader : PDCDSPPluginHeader; stdcall; export;

  function Init(Index : integer) : HRESULT; stdcall;
  function Quit(Index : integer) : HRESULT; stdcall;
  function ConfigInit(Index : integer) : HRESULT; stdcall;
  function ConfigClose(Index : integer) : HRESULT; stdcall;
  function AboutInit : HRESULT; stdcall;
  function AboutClose : HRESULT; stdcall;
  function ModifySamples(Index : integer; Buffer : Pointer; Size : integer; Channels, Bits : Byte; Frequency : integer; Float : Boolean) : HRESULT; stdcall;
  function Flush : HRESULT; stdcall;

const
  Header : TDCDSPPluginHeader = (
    Version       : DCSDKVerDSP;
    Description   : 'Winamp2 DSP Wrapper';
    GetModule     : GetModule;
    AboutInit     : AboutInit;
    AboutClose    : AboutClose;
  );

  Plugin1 : TDCDSPModule = (
    Description   : 'DSP Wrapper';
    Init          : Init;
    Quit          : Quit;
    ConfigInit    : ConfigInit;
    ConfigClose   : ConfigClose;
    ModifySamples : ModifySamples;
    Flush         : Flush;
  );

var
  Wrapper : TDCWADSPPluginList;
  fLastPlugin : integer = -1;
  fLastPluginName : String;
  fLastPluginIndex : integer = -1;

implementation

procedure SaveSettings;
var
  Setting : TIniFile;
  strbuf : array[0..MAX_PATH -1] of Char;
  name : String;
begin
  GetModuleFileName(GetModuleHandle(nil),strbuf,MAX_PATH);
  name := ExtractFilePath(strbuf) + 'Plugins\Plugins.ini';
  Setting := TIniFile.Create(name);
  Setting.WriteInteger('DCWinamp2DSPWrapper','LastIndex',fLastPluginIndex);
  Setting.WriteString('DCWinamp2DSPWrapper','LastName',fLastPluginName);
  Setting.WriteString('DCWinamp2DSPWrapper','Directory',Wrapper.Directory);
  Setting.Free;
end;

procedure LoadSettings;
var
  i : integer;
  Setting : TIniFile;
  strbuf : array[0..MAX_PATH -1] of Char;
  name : String;
begin
  GetModuleFileName(GetModuleHandle(nil),strbuf,MAX_PATH);
  name := ExtractFilePath(strbuf) + 'Plugins\Plugins.ini';
  Setting := TIniFile.Create(name);
  fLastPluginIndex := Setting.ReadInteger('DCWinamp2DSPWrapper','LastIndex',-1);
  fLastPluginName := Setting.ReadString('DCWinamp2DSPWrapper','LastName','');
  Wrapper.Directory := Setting.ReadString('DCWinamp2DSPWrapper','Directory','');
  if (fLastPluginName <> '') and (fLastPluginIndex <> -1) and (Wrapper.Count > 0) then
  begin
    for i := 0 to Wrapper.Count -1 do
    begin
      If Wrapper.Items[i].Filename = fLastPluginName then
      begin
        fLastPlugin := i + 1;
        Wrapper.Items[i].Init(fLastPluginIndex);
        break;
      end;
    end;
  end;
  Setting.Free;
end;

function Init (Index : integer) : HRESULT; stdcall;
begin
  if (Index = 0) and (Wrapper = nil) then
  begin
    Wrapper := TDCWADSPPluginList.Create;
    LoadSettings;
  end;
  Result := S_OK;
end;

function Quit(Index : integer) : HRESULT; stdcall;
begin
  Result := S_OK;
end;

function ModifySamples(Index : integer; Buffer : Pointer; Size : integer; Channels, Bits : Byte; Frequency : integer; Float : Boolean) : HRESULT; stdcall;
var
  Stream : TDSStream;
begin
  case Index of
    0:
    begin
      if Wrapper <> nil then
      begin
        Stream.Frequency := Frequency;
        Stream.Channels := Channels;
        Stream.Bits := Bits;
        Stream.Float := Float;
        Wrapper.Init(@Stream);
        Result := Wrapper.Process(Buffer,Size);
      end else Result := Size;
    end
    else Result := Size;
  end;
end;

function Flush : HRESULT; stdcall;
begin
  Result := S_OK;
end;

function ConfigInit(Index : integer) : HRESULT; stdcall;
begin
  Result := S_FALSE;
  case Index of
    0:
    begin
      if (Wrapper = nil) then
      begin
        Wrapper := TDCWADSPPluginList.Create;
        LoadSettings;
      end;
      if frmWA2DSPPlugins = nil then
      begin
        frmWA2DSPPlugins := TfrmWA2DSPPlugins.CreateParented(Header.Parent);
        frmWA2DSPPlugins.Width := DCWindowWidth;
        frmWA2DSPPlugins.Height := DCWindowHeight;
        frmWA2DSPPlugins.Left := 0;
        frmWA2DSPPlugins.Top := 0;
        frmWA2DSPPlugins.Visible := True;
        Result := S_OK;
      end else Result := S_FALSE;
    end;
  end;
end;

function ConfigClose(Index : integer) : HRESULT; stdcall;
begin
  Result := S_FALSE;
  case Index of
    0:
    begin
      if frmWA2DSPPlugins <> nil then
      begin
        frmWA2DSPPlugins.Free;
        frmWA2DSPPlugins := nil;
        Result := S_OK;
      end else Result := S_FALSE;
    end;
  end;
end;

function AboutInit : HRESULT; stdcall;
begin
  if frmAbout = nil then
  begin
    frmAbout := TfrmAbout.CreateParented(Header.Parent);
    frmAbout.Width := DCWindowWidth;
    frmAbout.Height := DCWindowHeight;
    frmAbout.Left := 0;
    frmAbout.Top := 0;
    frmAbout.Visible := True;
    Result := S_OK;
  end else Result := S_FALSE;
end;

function AboutClose : HRESULT; stdcall;
begin
  if frmAbout <> nil then
  begin
    frmAbout.Free;
    frmAbout := nil;
    Result := S_OK;
  end else Result := S_FALSE;
end;

function GetModule(Index : integer) : PDCDSPModule; stdcall;
begin
  case Index of
    0: Result := @Plugin1;
    else Result := nil;
  end;
end;

function DCDSPPluginGetHeader : PDCDSPPluginHeader;
begin
  Result := @Header;
end;

initialization

finalization
  if Wrapper <> nil then
  begin
    SaveSettings;
    FreeAndNil(Wrapper);
  end;
end.
