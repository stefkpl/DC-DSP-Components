unit formDMOFlanger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, DirectSound;

type
  TfrmDMOFlanger = class(TForm)
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
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
    rbSine: TRadioButton;
    rbTriangle: TRadioButton;
    tbWetDryMix: TTrackBar;
    tbDepth: TTrackBar;
    tbFeedback: TTrackBar;
    tbFrequency: TTrackBar;
    tbDelay: TTrackBar;
    tbLFOPhase: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbWetDryMixChange(Sender: TObject);
    procedure tbDepthChange(Sender: TObject);
    procedure tbFeedbackChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure tbLFOPhaseChange(Sender: TObject);
    procedure rbSineClick(Sender: TObject);
    procedure rbTriangleClick(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOFlanger: TfrmDMOFlanger;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDMOFlanger.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOFlanger.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOFlanger.UpdateValues;
begin
  rbSine.Checked := frmMain.DCDMOFlanger.Waveform = DSFXFLANGER_WAVE_SIN;
  rbTriangle.Checked := frmMain.DCDMOFlanger.Waveform = DSFXFLANGER_WAVE_TRIANGLE;
  tbWetDryMix.Position := Round(frmMain.DCDMOFlanger.WetDryMix * 10);
  tbDepth.Position := Round(frmMain.DCDMOFlanger.Depth * 10);
  tbFeedback.Position := Round(frmMain.DCDMOFlanger.Feedback * 10);
  tbFrequency.Position := Round(frmMain.DCDMOFlanger.Frequency * 10);
  tbDelay.Position := Round(frmMain.DCDMOFlanger.Delay * 10);
  tbLFOPhase.Position := frmMain.DCDMOFlanger.Phase;
  chkEnabled.Checked := frmMain.DCDMOFlanger.Enabled;
end;

procedure TfrmDMOFlanger.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOFlanger.tbWetDryMixChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.WetDryMix := tbWetDryMix.Position / 10;
  label3.Caption := Format('%.1f', [tbWetDryMix.Position / 10]);
end;

procedure TfrmDMOFlanger.tbDepthChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Depth := tbDepth.Position / 10;
  label5.Caption := Format('%.1f', [tbDepth.Position / 10]);
end;

procedure TfrmDMOFlanger.tbFeedbackChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Feedback := tbFeedback.Position / 10;
  label7.Caption := Format('%.1f', [tbFeedback.Position / 10]);
end;

procedure TfrmDMOFlanger.tbFrequencyChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Frequency := tbFrequency.Position / 10;
  label9.Caption := Format('%.1f', [tbFrequency.Position / 10]);
end;

procedure TfrmDMOFlanger.tbDelayChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Delay := tbDelay.Position / 10;
  label11.Caption := Format('%.1f', [tbDelay.Position / 10]);
end;

procedure TfrmDMOFlanger.tbLFOPhaseChange(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Phase := tbLFOPhase.Position;
  label13.Caption := inttostr(-180 + (tbLFOPhase.Position * 90));
end;

procedure TfrmDMOFlanger.rbSineClick(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Waveform := DSFXCHORUS_WAVE_SIN;
end;

procedure TfrmDMOFlanger.rbTriangleClick(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Waveform := DSFXCHORUS_WAVE_TRIANGLE;
end;

procedure TfrmDMOFlanger.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOFlanger.Enabled := chkEnabled.Checked;
end;

end.
