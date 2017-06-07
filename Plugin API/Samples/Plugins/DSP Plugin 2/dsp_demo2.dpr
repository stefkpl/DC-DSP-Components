library dsp_demo2;

{$R *.res}

uses
  FastMM4,
  FastMove,
  DCDSPPluginAPI in '..\..\..\API\DCDSPPluginAPI.pas',
  dsp1 in 'dsp1.pas',
  formAbout in 'formAbout.pas' {frmAbout};

exports
  DCDSPPluginGetHeader;

end.
 