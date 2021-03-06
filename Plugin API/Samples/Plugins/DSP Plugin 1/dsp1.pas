unit dsp1;

interface

uses
  Windows, DCDSPPluginAPI, Forms, formOptions, formOptions2, formAbout, Graphics,
  SysUtils, Dialogs, dspAmplify, dspDownMix;

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
    Description   : 'DSP Test Library 1';
    GetModule     : GetModule;
    AboutInit     : AboutInit;
    AboutClose    : AboutClose;
  );

  Plugin1 : TDCDSPModule = (
    Description   : 'Volume';
    Init          : Init;
    Quit          : Quit;
    ConfigInit    : ConfigInit;
    ConfigClose   : ConfigClose;
    ModifySamples : ModifySamples;
    Flush         : Flush;
  );

  Plugin2 : TDCDSPModule = (
    Description   : 'Downmix';
    Init          : Init;
    Quit          : Quit;
    ConfigInit    : ConfigInit;
    ConfigClose   : ConfigClose;
    ModifySamples : ModifySamples;
    Flush         : Flush;
  );

var
  fAmplify : TDCAmplify;
  fDownMix : TDCDownMix;

implementation

function Init (Index : integer) : HRESULT; stdcall;
begin
  Result := S_OK;
end;

function Quit(Index : integer) : HRESULT; stdcall;
begin
  Result := S_OK;
end;

function ModifySamples(Index : integer; Buffer : Pointer; Size : integer; Channels, Bits : Byte; Frequency : integer; Float : Boolean) : HRESULT; stdcall;
begin
  case Index of
    0: fAmplify.Process(Buffer,Size,Bits,Channels,Float);
    1: fDownMix.Process(Buffer,Size,Bits,Channels,Float);
  end;
  Result := Size;
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
    1:
    begin
      if frmOptions2 = nil then
      begin
        frmOptions2 := TfrmOptions2.CreateParented(Header.Parent);
        frmOptions2.Width := DCWindowWidth;
        frmOptions2.Height := DCWindowHeight;
        frmOptions2.Left := 0;
        frmOptions2.Top := 0;
        frmOptions2.Visible := True;
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
      if frmOptions <> nil then
      begin
        frmOptions.Free;
        frmOptions := nil;
        Result := S_OK;
      end else Result := S_FALSE;
    end;
    1:
    begin
      if frmOptions2 <> nil then
      begin
        frmOptions2.Free;
        frmOptions2 := nil;
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
    1: Result := @Plugin2
    else Result := nil;
  end;
end;

function DCDSPPluginGetHeader : PDCDSPPluginHeader;
begin
  Result := @Header;
end;

initialization
  fAmplify := TDCAmplify.Create(nil);
  fAmplify.Enabled := True;
  fDownMix := TDCDownMix.Create(nil);

finalization
  FreeAndNil(fAmplify);
  FreeAndNil(fDownMix);

end.
