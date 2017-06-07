unit formDSPEchoDelay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls;

type
  TfrmDSPEchoDelay = class(TForm)
    Label6: TLabel;
    Label9: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    tbDelay: TTrackBar;
    tbDelayGain: TTrackBar;
    spNumEchos: TSpinEdit;
    chkEchoHighPass: TCheckBox;
    chkKillMain: TCheckBox;
    chkEchoEnabled: TCheckBox;
    procedure tbDelayChange(Sender: TObject);
    procedure tbDelayGainChange(Sender: TObject);
    procedure chkEchoEnabledClick(Sender: TObject);
    procedure chkKillMainClick(Sender: TObject);
    procedure chkEchoHighPassClick(Sender: TObject);
    procedure spNumEchosChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    CanChange : Boolean;
  public
    { Public declarations }
  end;

var
  frmDSPEchoDelay: TfrmDSPEchoDelay;

implementation

uses formMain, dspEchoDelay;

{$R *.dfm}

procedure TfrmDSPEchoDelay.tbDelayChange(Sender: TObject);
begin
  if CanChange then frmMain.DCEchoDelay.Delay := tbDelay.Position;
  label6.Caption := inttostr(tbDelay.position div 10) + ' ms';
end;

procedure TfrmDSPEchoDelay.tbDelayGainChange(Sender: TObject);
begin
  frmMain.DCEchoDelay.DelayAmp := tbDelayGain.Position;
  label8.Caption := inttostr(tbDelayGain.position div 100) + ' %';
end;

procedure TfrmDSPEchoDelay.chkEchoEnabledClick(Sender: TObject);
begin
  if CanChange then frmMain.DCEchoDelay.Enabled := chkEchoEnabled.Checked;
end;

procedure TfrmDSPEchoDelay.chkKillMainClick(Sender: TObject);
begin
  frmMain.DCEchoDelay.KillMain := chkKillMain.Checked;
end;

procedure TfrmDSPEchoDelay.chkEchoHighPassClick(Sender: TObject);
begin
  frmMain.DCEchoDelay.Highpass := chkEchoHighPass.Checked;
end;

procedure TfrmDSPEchoDelay.spNumEchosChange(Sender: TObject);
begin
  frmMain.DCEchoDelay.NumDelays := spNumEchos.Value;
end;

procedure TfrmDSPEchoDelay.FormShow(Sender: TObject);
begin
  CanChange := False;
  spNumEchos.Value := frmMain.DCEchoDelay.NumDelays;
  chkKillMain.Checked := frmMain.DCEchoDelay.KillMain;
  chkEchoEnabled.Checked := frmMain.DCEchoDelay.Enabled;
  tbDelayGain.Position := frmMain.DCEchoDelay.DelayAmp;
  tbDelayGainChange(Self);
  tbDelay.Position := frmMain.DCEchoDelay.Delay;
  tbDelayChange(Self);
  chkEchoHighPass.Checked := frmMain.DCEchoDelay.Highpass;
  CanChange := True;
end;

end.
