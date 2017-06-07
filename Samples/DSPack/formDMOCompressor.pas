unit formDMOCompressor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons, DirectSound;

type
  TfrmDMOCompressor = class(TForm)
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    tbGain: TTrackBar;
    tbAttack: TTrackBar;
    tbRelease: TTrackBar;
    tbThreshold: TTrackBar;
    tbRatio: TTrackBar;
    tbPreDelay: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
    procedure tbAttackChange(Sender: TObject);
    procedure tbReleaseChange(Sender: TObject);
    procedure tbThresholdChange(Sender: TObject);
    procedure tbRatioChange(Sender: TObject);
    procedure tbPreDelayChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOCompressor: TfrmDMOCompressor;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDMOCompressor.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOCompressor.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOCompressor.UpdateValues;
begin
  tbGain.Position := Round(frmMain.DCDMOCompressor.Gain * 10);
  tbAttack.Position := Round(frmMain.DCDMOCompressor.Attack * 100);
  tbRelease.Position := Round(frmMain.DCDMOCompressor.Release * 10);
  tbThreshold.Position := Round(frmMain.DCDMOCompressor.Threshold * 10);
  tbRatio.Position := Round(frmMain.DCDMOCompressor.Ratio * 10);
  tbPreDelay.Position := Round(frmMain.DCDMOCompressor.PreDelay * 10);
  chkEnabled.Checked := frmMain.DCDMOCompressor.Enabled;
end;

procedure TfrmDMOCompressor.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOCompressor.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOCompressor.tbGainChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Gain := tbGain.Position / 10;
  label3.Caption := Format('%.1f', [tbGain.Position / 10]);
end;

procedure TfrmDMOCompressor.tbAttackChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Attack := tbAttack.Position / 100;
  label5.Caption := Format('%.1f', [tbAttack.Position / 100]);
end;

procedure TfrmDMOCompressor.tbReleaseChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Release := tbRelease.Position / 10;
  label7.Caption := Format('%.1f', [tbRelease.Position / 10]);
end;

procedure TfrmDMOCompressor.tbThresholdChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Threshold := tbThreshold.Position / 10;
  label9.Caption := Format('%.1f', [tbThreshold.Position / 10]);
end;

procedure TfrmDMOCompressor.tbRatioChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.Ratio := tbRatio.Position / 10;
  label11.Caption := Format('%.1f', [tbRatio.Position / 10]);
end;

procedure TfrmDMOCompressor.tbPreDelayChange(Sender: TObject);
begin
  frmMain.DCDMOCompressor.PreDelay := tbPreDelay.Position / 10;
  label13.Caption := Format('%.1f', [tbPreDelay.Position / 10]);
end;

end.
