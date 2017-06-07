library dsp_demo1;

{$R *.res}

uses
  FastMM4,
  FastMove,
  DCDSPPluginAPI in '..\..\..\API\DCDSPPluginAPI.pas',
  dsp1 in 'dsp1.pas',
  formOptions2 in 'formOptions2.pas' {frmOptions2},
  formAbout in 'formAbout.pas' {frmAbout};

exports
  DCDSPPluginGetHeader;

end.
 