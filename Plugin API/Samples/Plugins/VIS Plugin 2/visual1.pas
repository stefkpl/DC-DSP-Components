unit visual1;

interface

uses
  Windows, DCVISPluginAPI, Forms, formVisual, formOptions, formAbout, Graphics,
  SysUtils, Dialogs;

  function GetModule(Index : integer) : PDCVISModule; stdcall;
  function DCVISPluginGetHeader : PDCVISPluginHeader; stdcall; export;

  function SetMediaName(Name : PChar) : HRESULT; stdcall;
  function Init(Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
  function Quit(Index : integer) : HRESULT; stdcall;
  function Resize(Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
  function ConfigInit(Index : integer) : HRESULT; stdcall;
  function ConfigClose(Index : integer) : HRESULT; stdcall;
  function AboutInit : HRESULT; stdcall;
  function AboutClose : HRESULT; stdcall;
  function Render(Index : integer) : HRESULT; stdcall;

const
  Header : TDCVISPluginHeader = (
    Version     : DCSDKVerVIS;
    Description : 'Visual Spectrum Library';
    GetModule   : GetModule;
    AboutInit   : AboutInit;
    AboutClose  : AboutClose;
  );

  Plugin1 : TDCVISModule = (
    Description  : 'Spectrum 1';
    FramesPerSec : 40;
    MixSpectrum  : 1;
    MixWaveform  : 0;
    SpectrumChan : 1;
    WaveformChan : 0;
    Init         : Init;
    Quit         : Quit;
    ConfigInit   : ConfigInit;
    ConfigClose  : ConfigClose;
    Render       : Render;
    Resize       : Resize;
    SetMediaName : SetMediaName;
  );

  Plugin2 : TDCVISModule = (
    Description  : 'Spectrum 2';
    FramesPerSec : 40;
    MixSpectrum  : 0;
    MixWaveform  : 0;
    SpectrumChan : 2;
    WaveformChan : 0;
    Init         : Init;
    Quit         : Quit;
    ConfigInit   : ConfigInit;
    ConfigClose  : ConfigClose;
    Render       : Render;
    Resize       : Resize;
    SetMediaName : SetMediaName;
  );

var
  MediaName : String;
  frmVisual : TfrmVisual;
  CurrentIndex : integer = -1;
  fVisualColor : TColor = $808080;

implementation

function Init (Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
begin
  if frmVisual = nil then
  begin
    CurrentIndex := Index;
    frmVisual := TfrmVisual.CreateParented(Header.ParentRenderer);
    frmVisual.Color := clBlack;
    frmVisual.Left := 0;
    frmVisual.Top := 0;
    frmVisual.Width := Width;
    frmVisual.Height := Height;
    frmVisual.Show;
    Result := frmVisual.Handle;
  end else Result := S_FALSE;
end;

function Quit (Index : integer) : HRESULT; stdcall;
begin
  if frmVisual <> nil then
  begin
    frmVisual.Free;
    frmVisual := nil;
    Result := S_OK;
  end else Result := S_FALSE;
end;

function Resize (Index : integer; Width : integer; Height : integer) : HRESULT; stdcall;
begin
  if frmVisual <> nil then
  begin
    SetWindowPos(frmVisual.Handle,0,0,0,Width,Height,SWP_NOZORDER or SWP_NOMOVE);
    Result := S_OK;
  end else Result := S_FALSE;
end;

function ConfigInit(Index : integer) : HRESULT; stdcall;
begin
  if frmOptions = nil then
  begin
    frmOptions := TfrmOptions.CreateParented(Header.Parent);
    frmOptions.Width := DCWindowWidth;
    frmOptions.Height := DCWindowHeight;
    frmOptions.Left := 0;
    frmOptions.Top := 0;
    frmOptions.Visible := True;
    Result := S_OK;
  end else Result := S_FALSE;
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

function Render (Index : integer) : HRESULT; stdcall;
begin
  if frmVisual <> nil then
  begin
    case Index of
      0: frmVisual.Render1;
      1: frmVisual.Render2;
    end;
    Result := S_OK;
  end else Result := S_FALSE;
end;

function SetMediaName(Name : PChar) : HRESULT; stdcall;
begin
  MediaName := Name;
  Result := S_OK;
end;

function GetModule(Index : integer) : PDCVISModule; stdcall;
begin
  case Index of
    0: Result := @Plugin1;
    1: Result := @Plugin2;
    else Result := nil;
  end;
end;

function ConfigClose (Index : integer) : HRESULT; stdcall;
begin
  if frmOptions <> nil then
  begin
    frmOptions.Free;
    frmOptions := nil;
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

function DCVISPluginGetHeader : PDCVISPluginHeader;
begin
  Result := @Header;
end;

end.
