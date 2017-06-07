unit formDSPSound3D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPSound3D = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tb3DSound: TTrackBar;
    chkEnabled: TCheckBox;
    procedure chkEnabledClick(Sender: TObject);
    procedure tb3DSoundChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPSound3D: TfrmDSPSound3D;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPSound3D.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCSound3D.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPSound3D.tb3DSoundChange(Sender: TObject);
begin
  frmMain.DCSound3D.Volume := tb3DSound.Position;
  label13.Caption := inttostr((tb3DSound.Position) div 100) + ' %';
end;

end.
