unit formDMOChorus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, ComCtrls, DirectSound;

type
  TfrmDMOChorus = class(TForm)
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    rbSine: TRadioButton;
    rbTriangle: TRadioButton;
    Label2: TLabel;
    tbWetDryMix: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    tbDepth: TTrackBar;
    Label5: TLabel;
    Label6: TLabel;
    tbFeedback: TTrackBar;
    Label7: TLabel;
    Label8: TLabel;
    tbFrequency: TTrackBar;
    Label9: TLabel;
    Label10: TLabel;
    tbDelay: TTrackBar;
    Label11: TLabel;
    Label12: TLabel;
    tbLFOPhase: TTrackBar;
    Label13: TLabel;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbWetDryMixChange(Sender: TObject);
    procedure tbDepthChange(Sender: TObject);
    procedure tbFeedbackChange(Sender: TObject);
    procedure tbFrequencyChange(Sender: TObject);
    procedure tbDelayChange(Sender: TObject);
    procedure tbLFOPhaseChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure rbTriangleClick(Sender: TObject);
    procedure rbSineClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOChorus: TfrmDMOChorus;

implementation

uses formMain, dmoChorus;

{$R *.dfm}

procedure TfrmDMOChorus.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOChorus.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOChorus.UpdateValues;
begin
  rbSine.Checked := frmMain.DCDMOChorus.Waveform = DSFXCHORUS_WAVE_SIN;
  rbTriangle.Checked := frmMain.DCDMOChorus.Waveform = DSFXCHORUS_WAVE_TRIANGLE;
  tbWetDryMix.Position := Round(frmMain.DCDMOChorus.WetDryMix * 10);
  tbDepth.Position := Round(frmMain.DCDMOChorus.Depth * 10);
  tbFeedback.Position := Round(frmMain.DCDMOChorus.Feedback * 10);
  tbFrequency.Position := Round(frmMain.DCDMOChorus.Frequency * 10);
  tbDelay.Position := Round(frmMain.DCDMOChorus.Delay * 10);
  tbLFOPhase.Position := frmMain.DCDMOChorus.Phase;
  chkEnabled.Checked := frmMain.DCDMOChorus.Enabled;
end;

procedure TfrmDMOChorus.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOChorus.tbWetDryMixChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.WetDryMix := tbWetDryMix.Position / 10;
  label3.Caption := Format('%.1f', [tbWetDryMix.Position / 10]);
end;

procedure TfrmDMOChorus.tbDepthChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.Depth := tbDepth.Position / 10;
  label5.Caption := Format('%.1f', [tbDepth.Position / 10]);
end;

procedure TfrmDMOChorus.tbFeedbackChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.Feedback := tbFeedback.Position / 10;
  label7.Caption := Format('%.1f', [tbFeedback.Position / 10]);
end;

procedure TfrmDMOChorus.tbFrequencyChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.Frequency := tbFrequency.Position / 10;
  label9.Caption := Format('%.1f', [tbFrequency.Position / 10]);
end;

procedure TfrmDMOChorus.tbDelayChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.Delay := tbDelay.Position / 10;
  label11.Caption := Format('%.1f', [tbDelay.Position / 10]);
end;

procedure TfrmDMOChorus.tbLFOPhaseChange(Sender: TObject);
begin
  frmMain.DCDMOChorus.Phase := tbLFOPhase.Position;
  label13.Caption := inttostr(-180 + (tbLFOPhase.Position * 90));
end;

procedure TfrmDMOChorus.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOChorus.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOChorus.rbTriangleClick(Sender: TObject);
begin
  frmMain.DCDMOChorus.Waveform := DSFXCHORUS_WAVE_TRIANGLE;
end;

procedure TfrmDMOChorus.rbSineClick(Sender: TObject);
begin
  frmMain.DCDMOChorus.Waveform := DSFXCHORUS_WAVE_SIN;
end;

end.
