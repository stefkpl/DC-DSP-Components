library vis_Waveform;

{$R *.res}

uses
  FastMM4,
  FastMove,
  DCVISPluginAPI in '..\..\..\API\DCVISPluginAPI.pas',
  visual1 in 'visual1.pas',
  formVisual in 'formVisual.pas' {frmVisual},
  formOptions in 'formOptions.pas' {frmOptions},
  formAbout in 'formAbout.pas' {frmAbout};

exports
  DCVISPluginGetHeader;

end.
 