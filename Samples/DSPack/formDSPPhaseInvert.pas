unit formDSPPhaseInvert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmDSPPhaseInvert = class(TForm)
    chkPhaseInvert: TCheckBox;
    Label5: TLabel;
    cmbPhaseInvertChannels: TComboBox;
    chkPhaseInvEnabled: TCheckBox;
    procedure chkPhaseInvertClick(Sender: TObject);
    procedure cmbPhaseInvertChannelsChange(Sender: TObject);
    procedure chkPhaseInvEnabledClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPPhaseInvert: TfrmDSPPhaseInvert;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDSPPhaseInvert.chkPhaseInvertClick(Sender: TObject);
begin
  frmMain.DCPhaseInvert.Enabled := chkPhaseInvert.Checked;
end;

procedure TfrmDSPPhaseInvert.cmbPhaseInvertChannelsChange(Sender: TObject);
begin
  chkPhaseInvEnabled.Checked := frmMain.DCPhaseInvert.Invert[cmbPhaseInvertChannels.ItemIndex];
end;

procedure TfrmDSPPhaseInvert.chkPhaseInvEnabledClick(Sender: TObject);
begin
  frmMain.DCPhaseInvert.Invert[cmbPhaseInvertChannels.ItemIndex] := chkPhaseInvEnabled.Checked;
end;

end.
