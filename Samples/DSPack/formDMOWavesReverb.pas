unit formDMOWavesReverb;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons;

type
  TfrmDMOWavesReverb = class(TForm)
    SpeedButton1: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    tbInGain: TTrackBar;
    tbReverbMix: TTrackBar;
    tbReverbTime: TTrackBar;
    tbHFRatio: TTrackBar;
    chkEnabled: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure tbInGainChange(Sender: TObject);
    procedure tbReverbMixChange(Sender: TObject);
    procedure tbReverbTimeChange(Sender: TObject);
    procedure tbHFRatioChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateValues;
  public
    { Public declarations }
  end;

var
  frmDMOWavesReverb: TfrmDMOWavesReverb;

implementation

uses formMain;

{$R *.dfm}

procedure TfrmDMOWavesReverb.SpeedButton1Click(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.ShowPropertyPage(Handle);
  UpdateValues;
end;

procedure TfrmDMOWavesReverb.FormShow(Sender: TObject);
begin
  UpdateValues;
end;

procedure TfrmDMOWavesReverb.UpdateValues;
begin
  tbInGain.Position := Round(frmMain.DCDMOWavesReverb.InGain * 10);
  tbReverbMix.Position := Round(frmMain.DCDMOWavesReverb.ReverbMix * 10);
  tbReverbTime.Position := Round(frmMain.DCDMOWavesReverb.ReverbTime * 1000);
  tbHFRatio.Position := Round(frmMain.DCDMOWavesReverb.HighFreqRTRatio * 1000);
  chkEnabled.Checked := frmMain.DCDMOWavesReverb.Enabled;
end;

procedure TfrmDMOWavesReverb.chkEnabledClick(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.Enabled := chkEnabled.Checked;
end;

procedure TfrmDMOWavesReverb.tbInGainChange(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.InGain := tbInGain.Position / 10;
  label3.Caption := Format('%.1f', [tbInGain.Position / 10]);
end;

procedure TfrmDMOWavesReverb.tbReverbMixChange(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.ReverbMix := tbReverbMix.Position / 10;
  label5.Caption := Format('%.1f', [tbReverbMix.Position / 10]);
end;

procedure TfrmDMOWavesReverb.tbReverbTimeChange(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.ReverbTime := tbReverbTime.Position / 1000;
  label7.Caption := Format('%.3f', [tbReverbTime.Position / 1000]);
end;

procedure TfrmDMOWavesReverb.tbHFRatioChange(Sender: TObject);
begin
  frmMain.DCDMOWavesReverb.HighFreqRTRatio := tbHFRatio.Position / 1000;
  label9.Caption := Format('%.3f', [tbHFRatio.Position / 1000]);
end;

end.
