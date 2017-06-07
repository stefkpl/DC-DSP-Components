unit formDSPPitchShift;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPPitchShift = class(TForm)
    Label12: TLabel;
    Label13: TLabel;
    tbPitchShift: TTrackBar;
    chkEnabled: TCheckBox;
    procedure chkEnabledClick(Sender: TObject);
    procedure tbPitchShiftChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPPitchShift: TfrmDSPPitchShift;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPPitchShift.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCPitchShift.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPPitchShift.tbPitchShiftChange(Sender: TObject);
begin
  label13.Caption := Format('%.2fx', [tbPitchShift.Position / 1000]);
  frmMain.DCPitchShift.Pitch := tbPitchShift.Position;
end;

end.
