unit formDSPFlanger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmDSPFlanger = class(TForm)
    Label6: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    Label15: TLabel;
    tbFrequency: TTrackBar;
    tbDelay: TTrackBar;
    chkEnabled: TCheckBox;
    cmbChannels: TComboBox;
    chkInvertEnabled: TCheckBox;
    procedure chkEnabledClick(Sender: TObject);
    procedure cmbPhaseInvertChannelsChange(Sender: TObject);
    procedure cmbChannelsChange(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure chkInvertEnabledClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDSPFlanger: TfrmDSPFlanger;

implementation

uses formMain, dspFlanger;

{$R *.dfm}

procedure TfrmDSPFlanger.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCFlanger.Enabled := chkEnabled.Checked;
end;

procedure TfrmDSPFlanger.cmbPhaseInvertChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  chkInvertEnabled.Checked := frmMain.DCFlanger.PhaseInvert[i];
end;

procedure TfrmDSPFlanger.cmbChannelsChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  frmMain.DCFlanger.Seperate := i <> 0;
  if i > 0 then dec(i);
  chkInvertEnabled.Checked := frmMain.DCFlanger.PhaseInvert[i];
  tbFrequency.Position := Round(frmMain.DCFlanger.Frequency[i]* 1000);
  tbFrequencyChange(Self);
  tbDelay.Position := Round(frmMain.DCFlanger.Delay[i]* 1000);
  tbDelayChange(Self);
end;

procedure TfrmDSPFlanger.tbDelayChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCFlanger.Delay[i] := tbDelay.Position / 1000;
  label8.Caption := inttostr(tbDelay.Position) + ' ms';
end;

procedure TfrmDSPFlanger.tbFrequencyChange(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCFlanger.Frequency[i] := tbFrequency.Position / 1000;
  label6.Caption := Format('%.1f sec', [2000 / tbFrequency.Position]);
end;

procedure TfrmDSPFlanger.chkInvertEnabledClick(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  frmMain.DCFlanger.PhaseInvert[i] := chkInvertEnabled.Checked;
end;

procedure TfrmDSPFlanger.FormCreate(Sender: TObject);
var
  i : integer;
begin
  i := cmbChannels.ItemIndex;
  if i > 0 then dec(i);
  chkInvertEnabled.Checked := frmMain.DCFlanger.PhaseInvert[i];
  tbFrequency.Position := Round(frmMain.DCFlanger.Frequency[i]* 1000);
  tbFrequencyChange(Self);
  tbDelay.Position := Round(frmMain.DCFlanger.Delay[i]* 1000);
  tbDelayChange(Self);
end;

end.
