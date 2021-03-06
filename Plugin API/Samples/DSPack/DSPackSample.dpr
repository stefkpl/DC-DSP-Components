program DSPackSample;

uses
  FastMM4,
  FastMove,
  Forms,
  formMain in 'formMain.pas' {frmMain},
  formPlugins in 'formPlugins.pas' {frmPlugins},
  DCDSPPluginAPI in '..\..\API\DCDSPPluginAPI.pas',
  DCDSPPluginRenderer in '..\..\API\DCDSPPluginRenderer.pas',
  DCVISPluginAPI in '..\..\API\DCVISPluginAPI.pas',
  DCVISPluginRenderer in '..\..\API\DCVISPluginRenderer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPlugins, frmPlugins);
  Application.Run;
end.
