program configure;

{$R WindowsXP.res}

uses
  Windows, ActiveX;

var
  Lib: THandle;
  config: function: HResult; stdcall;

begin
  OleInitialize(nil);
  CoInitialize(nil);

  Lib := LoadLibrary('DCDSPFilter.ax');
  if (Lib = 0) then
  begin
    MessageBox(0, 'Error creating filter (maybe not registered)', 'Error', MB_OK or MB_ICONERROR);
  end else
  begin
    @config := GetProcAddress(Lib, 'configure');
    config;
    FreeLibrary(Lib);
  end;

  OleUninitialize;
  CoUninitialize;
end.
