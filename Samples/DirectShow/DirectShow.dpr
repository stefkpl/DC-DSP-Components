program DirectShow;

uses
  FastMM4,
  FastMove,
  Forms,
  formMain in 'formMain.pas' {frmMain},
  DirectShowUtils in '..\DSPack\DirectShowUtils.pas',
  DCDSPFilterInterfaces in '..\..\DirectShow Filter\Interface\DCDSPFilterInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
